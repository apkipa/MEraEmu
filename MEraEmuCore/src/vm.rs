use crate::{
    bytecode::{
        EraBytecodePrimaryType, FlatValue, PrintExtendedFlags, SourcePosInfo, StrValue, Value,
    },
    compiler::{EraBytecodeChunk, EraBytecodeCompilation, EraFuncBytecodeInfo},
    util::*,
};
use std::{
    collections::HashMap,
    rc::Rc,
    sync::atomic::{AtomicBool, Ordering},
};

struct EraErrReportContext<'a> {
    chunks: &'a [EraBytecodeChunk],
    callback: &'a mut dyn EraVirtualMachineCallback,
}

// TODO: Introduce report error context to simplify invoking
macro_rules! vm_report_err {
    ($ctx:expr, None, $is_error:expr, $msg:expr) => {
        Self::report_err($ctx.callback, $ctx.chunks, None, $is_error, $msg);
    };
    ($ctx:expr, $frame:expr, $is_error:expr, $msg:expr) => {
        Self::report_err($ctx.callback, $ctx.chunks, Some($frame), $is_error, $msg);
    };
}
macro_rules! vm_read_chunk_u8 {
    ($ctx:expr, $frame:expr, $chunk:expr, $offset:expr) => {
        match $chunk.read_u8($offset) {
            Some(x) => Some(x),
            None => {
                vm_report_err!($ctx, $frame, true, "ip exceeds bounds of chunk");
                None
            }
        }
    };
}
macro_rules! vm_pop_stack {
    ($ctx:expr, $frame:expr, $stack:expr) => {
        match $stack.pop() {
            Some(x) => Some(x),
            None => {
                vm_report_err!($ctx, $frame, true, "too few elements in stack");
                None
            }
        }
    };
    ($ctx:expr, $frame:expr, $stack:expr, $count:literal) => {
        match $stack.len().checked_sub($count) {
            Some(new_len) => {
                let mut it = $stack.drain(new_len..).fuse();
                let res: [_; $count] = std::array::from_fn(|_| it.next().unwrap());
                assert!(matches!(it.next(), None));
                Some(res)
            }
            None => {
                vm_report_err!($ctx, $frame, true, "too few elements in stack");
                None
            }
        }
    };
    ($ctx:expr, $frame:expr, $stack:expr, $count:expr) => {
        match $stack.len().checked_sub($count) {
            Some(new_len) => Some($stack.drain(new_len..)),
            None => {
                vm_report_err!($ctx, $frame, true, "too few elements in stack");
                None
            }
        }
    };
}

#[derive(Default)]
pub struct EraVarPool {
    // Mapping from names to indices.
    var_names: HashMap<Rc<CaselessStr>, usize>,
    vars: Vec<EraVarInfo>,
}

pub struct EraVarInfo {
    pub name: Rc<CaselessStr>,
    pub val: crate::bytecode::Value,
}

impl EraVarPool {
    pub fn new() -> Self {
        EraVarPool {
            var_names: HashMap::new(),
            vars: Vec::new(),
        }
    }
    #[must_use]
    pub fn add_var(&mut self, name: &str, val: crate::bytecode::Value) -> Option<usize> {
        let name: Rc<CaselessStr> = CaselessStr::new(name).into();
        let var_idx = self.vars.len();
        if self.var_names.insert(name.clone(), var_idx).is_some() {
            return None;
        }
        self.vars.push(EraVarInfo { name, val });
        Some(var_idx)
    }
    #[must_use]
    pub fn get_var(&self, name: &str) -> Option<&crate::bytecode::Value> {
        self.var_names
            .get(CaselessStr::new(name))
            .map(|x| &self.vars[*x].val)
    }
    #[must_use]
    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut crate::bytecode::Value> {
        self.var_names
            .get_mut(CaselessStr::new(name))
            .map(|x| &mut self.vars[*x].val)
    }
    #[must_use]
    pub fn get_var_idx(&self, name: &str) -> Option<usize> {
        self.var_names.get(CaselessStr::new(name)).copied()
    }
    #[must_use]
    pub fn get_var_info(&self, idx: usize) -> Option<&EraVarInfo> {
        self.vars.get(idx)
    }
}

pub struct EraExecIp {
    pub chunk: usize,
    pub offset: usize,
}

struct EraFuncExecFrame {
    stack_start: usize,
    ip: EraExecIp,
    ret_ip: EraExecIp,
}

pub struct EraVirtualMachine {
    func_names: HashMap<Rc<CaselessStr>, usize>,
    funcs: Vec<EraFuncBytecodeInfo>,
    chunks: Vec<EraBytecodeChunk>,
    global_vars: EraVarPool,
    stack: Vec<Value>,
    frames: Vec<EraFuncExecFrame>,
    is_halted: bool,
}

pub struct EraRuntimeErrorInfo {
    pub file_name: String,
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}

pub trait EraVirtualMachineCallback {
    fn on_execution_error(&mut self, error: EraRuntimeErrorInfo);
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
}

impl EraVirtualMachine {
    pub fn new(compilation: EraBytecodeCompilation) -> Self {
        EraVirtualMachine {
            func_names: compilation.func_names,
            funcs: compilation.funcs,
            chunks: compilation.chunks,
            global_vars: compilation.global_vars,
            stack: Vec::new(),
            frames: Vec::new(),
            is_halted: false,
        }
    }
    pub fn reset_stack_and_ip(&mut self, ip: EraExecIp) {
        // TODO: Verify input
        self.is_halted = false;
        self.stack.clear();
        self.stack.push(Value::new_int(0)); // Stub value
        self.frames.clear();
        self.frames.push(EraFuncExecFrame {
            stack_start: 0,
            ip,
            ret_ip: EraExecIp {
                chunk: 0,
                offset: 0,
            },
        });
    }
    pub fn get_is_halted(&self) -> bool {
        self.is_halted
    }
    // Returns whether execution can progress further
    pub fn execute(
        &mut self,
        stop_flag: &AtomicBool,
        max_inst_cnt: u64,
        callback: &mut dyn EraVirtualMachineCallback,
    ) -> bool {
        if self.is_halted {
            return false;
        }

        if self
            .execute_inner(stop_flag, max_inst_cnt, callback)
            .is_none()
        {
            self.is_halted = true;
        }

        !self.is_halted
    }
    fn execute_inner(
        &mut self,
        stop_flag: &AtomicBool,
        max_inst_cnt: u64,
        callback: &mut dyn EraVirtualMachineCallback,
    ) -> Option<()> {
        use EraBytecodePrimaryType::*;

        // Helpers
        // let mut read_chunk_u8 = |chunk: &EraBytecodeChunk, offset: usize| -> Option<u8> {
        //     match chunk.read_u8(offset) {
        //         Some(x) => Some(x),
        //         None => {
        //             self.report_err(true, "ip exceeds bounds of chunk");
        //             return None;
        //         }
        //     }
        // };

        let ctx = EraErrReportContext {
            chunks: self.chunks.as_slice(),
            callback,
        };

        let mut cur_frame = match self.frames.last_mut() {
            Some(x) => x,
            None => {
                // Self::report_err(err_report_fn, &self.chunks, None, true, "unexpected end of execution frame");
                vm_report_err!(ctx, None, true, "unexpected end of execution frame");
                //self.report_err(true, "unexpected end of execution frame");
                return None;
            }
        };
        let mut cur_chunk = match self.chunks.get(cur_frame.ip.chunk) {
            Some(x) => x,
            None => {
                vm_report_err!(ctx, cur_frame, true, "ip exceeds bounds of chunk");
                return None;
            }
        };

        for _ in 0..max_inst_cnt {
            if stop_flag.load(Ordering::Relaxed) {
                break;
            }

            let mut ip_offset_delta = 1;
            let primary_bytecode =
                vm_read_chunk_u8!(ctx, cur_frame, cur_chunk, cur_frame.ip.offset)?;
            //let primary_bytecode = self.read_chunk_u8(cur_chunk, cur_frame.ip.offset)?;
            let primary_bytecode = match EraBytecodePrimaryType::try_from_i(primary_bytecode) {
                Some(x) => x,
                None => {
                    vm_report_err!(
                        ctx,
                        cur_frame,
                        true,
                        format!(
                            "invalid bytecode `{}` (memory corrupted?)",
                            primary_bytecode
                        )
                    );
                    return None;
                }
            };
            match primary_bytecode {
                DebugBreak => break,
                Quit => return None,
                InvalidWithMessage => {
                    let msg = self.stack.pop().and_then(|x| match x.into_unpacked() {
                        FlatValue::Int(x) => Some(Rc::new(StrValue {
                            val: x.val.to_string(),
                        })),
                        FlatValue::Str(x) => Some(x.clone()),
                        FlatValue::ArrInt(x) => x.borrow().vals.first().map(|x| {
                            Rc::new(StrValue {
                                val: x.val.to_string(),
                            })
                        }),
                        FlatValue::ArrStr(x) => x.borrow().vals.first().map(|x| x.clone()),
                    });
                    let msg = msg.as_ref().map(|x| x.val.as_str()).unwrap_or("<invalid>");
                    vm_report_err!(ctx, cur_frame, true, format!("invalid bytecode: {msg}"));
                    return None;
                }
                Pop => {
                    _ = vm_pop_stack!(ctx, cur_frame, &mut self.stack)?;
                }
                ReturnVoid => {
                    self.stack.drain(cur_frame.stack_start..);
                    self.frames.pop();
                    cur_frame = match self.frames.last_mut() {
                        Some(x) => x,
                        None => break,
                    };
                    cur_chunk = match self.chunks.get(cur_frame.ip.chunk) {
                        Some(x) => x,
                        None => {
                            vm_report_err!(ctx, cur_frame, true, "ip exceeds bounds of chunk");
                            return None;
                        }
                    };
                }
                ReturnInteger | ReturnString => {
                    todo!()
                }
                FunCall => {
                    todo!()
                }
                LoadConst => {
                    let entry =
                        vm_read_chunk_u8!(ctx, cur_frame, cur_chunk, cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let value = match cur_chunk.get_constant(entry as usize) {
                        Some(x) => x,
                        None => {
                            vm_report_err!(
                                ctx,
                                cur_frame,
                                true,
                                format!(
                                    "invalid index {} into constant pool of size {}",
                                    entry,
                                    cur_chunk.get_constants_cnt()
                                )
                            );
                            return None;
                        }
                    };
                    self.stack.push(value.clone());
                }
                BuildString => {
                    let count =
                        vm_read_chunk_u8!(ctx, cur_frame, cur_chunk, cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    // let new_stack_len = match self.stack.len().checked_sub(count as _) {
                    //     Some(x) => x,
                    //     None => {
                    //         vm_report_err!(self, cur_frame, true, "too few elements in stack");
                    //         return None;
                    //     }
                    // };
                    // let mut new_str = String::new();
                    // for value in self.stack.drain(new_stack_len..) {
                    //     match value.into_unpacked() {
                    //         FlatValue::Str(x) => new_str += &x.val,
                    //         _ => {
                    //             vm_report_err!(
                    //                 self,
                    //                 cur_frame,
                    //                 true,
                    //                 "BuildString requires string values"
                    //             );
                    //             return None;
                    //         }
                    //     }
                    // }
                    let mut new_str = String::new();
                    for value in vm_pop_stack!(ctx, cur_frame, &mut self.stack, count.into())? {
                        match value.into_unpacked() {
                            FlatValue::Str(x) => new_str += &x.val,
                            _ => {
                                vm_report_err!(
                                    ctx,
                                    cur_frame,
                                    true,
                                    "BuildString requires string values as operands"
                                );
                                return None;
                            }
                        }
                    }
                    let new_str = Value::new_str(new_str);
                    self.stack.push(new_str);
                }
                ConvertToString => {
                    let value =
                        match vm_pop_stack!(ctx, cur_frame, &mut self.stack)?.into_unpacked() {
                            FlatValue::Int(x) => Value::new_str(x.val.to_string()),
                            FlatValue::Str(x) => Value::new_str_rc(x),
                            _ => {
                                vm_report_err!(
                                    ctx,
                                    cur_frame,
                                    true,
                                    "expected primitive values as operands"
                                );
                                return None;
                            }
                        };
                    self.stack.push(value);
                }
                ConvertToInteger => {
                    let value =
                        match vm_pop_stack!(ctx, cur_frame, &mut self.stack)?.into_unpacked() {
                            FlatValue::Int(x) => Value::new_int_rc(x),
                            FlatValue::Str(x) => {
                                let x = match x.val.parse() {
                                    Ok(x) => x,
                                    Err(_) => {
                                        vm_report_err!(
                                            ctx,
                                            cur_frame,
                                            true,
                                            format!(
                                            "string `{}` cannot be converted to a valid integer",
                                            x.val
                                        )
                                        );
                                        return None;
                                    }
                                };
                                Value::new_int(x)
                            }
                            _ => {
                                vm_report_err!(
                                    ctx,
                                    cur_frame,
                                    true,
                                    "expected primitive values as operands"
                                );
                                return None;
                            }
                        };
                    self.stack.push(value);
                }
                Print => {
                    let value = vm_pop_stack!(ctx, cur_frame, &mut self.stack)?;
                    let flags = PrintExtendedFlags::new();
                    match value.into_unpacked() {
                        FlatValue::Int(x) => ctx.callback.on_print(&x.val.to_string(), flags),
                        FlatValue::Str(x) => ctx.callback.on_print(&x.val, flags),
                        _ => {
                            vm_report_err!(
                                ctx,
                                cur_frame,
                                true,
                                "expected primitive values as operands"
                            );
                            return None;
                        }
                    }
                }
                Invalid | _ => {
                    vm_report_err!(
                        ctx,
                        cur_frame,
                        true,
                        format!("invalid or unimplemented bytecode `{primary_bytecode:?}`")
                    );
                    return None;
                }
            }

            cur_frame.ip.offset = match cur_frame.ip.offset.checked_add_signed(ip_offset_delta) {
                Some(x) => x,
                None => {
                    vm_report_err!(ctx, cur_frame, true, "ip exceeds bounds of chunk");
                    return None;
                }
            }
        }

        Some(())
    }
    // fn read_chunk_u8(&mut self, chunk: &EraBytecodeChunk, offset: usize) -> Option<u8> {
    //     match chunk.read_u8(offset) {
    //         Some(x) => Some(x),
    //         None => {
    //             self.report_err(true, "ip exceeds bounds of chunk");
    //             return None;
    //         }
    //     }
    // }
    // fn read_chunk_u16(&mut self, chunk: &EraBytecodeChunk, offset: usize) -> Option<u16> {
    //     match chunk.read_u16(offset) {
    //         Some(x) => Some(x),
    //         None => {
    //             self.report_err(true, "ip exceeds bounds of chunk");
    //             return None;
    //         }
    //     }
    // }
    // fn read_chunk_u32(&mut self, chunk: &EraBytecodeChunk, offset: usize) -> Option<u32> {
    //     match chunk.read_u32(offset) {
    //         Some(x) => Some(x),
    //         None => {
    //             self.report_err(true, "ip exceeds bounds of chunk");
    //             return None;
    //         }
    //     }
    // }
    // fn report_err<V: Into<String>>(
    //     &mut self,
    //     // src_info: SourcePosInfo,
    //     is_error: bool,
    //     msg: V,
    // ) {
    //     let ph_si = SourcePosInfo { line: 1, column: 1 };
    //     let (file_name, src_info) = self
    //         .frames
    //         .last()
    //         .and_then(|f| {
    //             self.chunks.get(f.ip.chunk).map(|x| {
    //                 (
    //                     x.get_name().to_owned(),
    //                     x.source_info_at(f.ip.offset).unwrap_or(ph_si),
    //                 )
    //             })
    //         })
    //         .unwrap_or(("<invalid>".to_owned(), ph_si));
    //     (self.err_report_fn)(&EraRuntimeErrorInfo {
    //         file_name,
    //         src_info,
    //         is_error,
    //         msg: msg.into(),
    //     });
    // }
    fn report_err<V: Into<String>>(
        callback: &mut dyn EraVirtualMachineCallback,
        chunks: &[EraBytecodeChunk],
        frame: Option<&EraFuncExecFrame>,
        is_error: bool,
        msg: V,
    ) {
        let ph_si = SourcePosInfo { line: 1, column: 1 };
        let (file_name, src_info) = frame
            .and_then(|f| {
                chunks.get(f.ip.chunk).map(|x| {
                    (
                        x.get_name().to_owned(),
                        x.source_info_at(f.ip.offset).unwrap_or(ph_si),
                    )
                })
            })
            .unwrap_or(("<invalid>".to_owned(), ph_si));
        callback.on_execution_error(EraRuntimeErrorInfo {
            file_name,
            src_info,
            is_error,
            msg: msg.into(),
        });
    }
}
