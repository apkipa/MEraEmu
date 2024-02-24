use crate::{
    bytecode::{
        EraBytecodePrimaryType, FlatValue, PadStringFlags, PrintExtendedFlags, SourcePosInfo,
        StrValue, Value, ValueKind,
    },
    compiler::{EraBytecodeChunk, EraBytecodeCompilation, EraFuncBytecodeInfo},
    util::*,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::atomic::{AtomicBool, Ordering},
};

struct EraErrReportContext<'a> {
    chunks: &'a [EraBytecodeChunk],
    callback: &'a mut dyn EraVirtualMachineCallback,
    stack: &'a mut Vec<EraTrappableValue>,
    cur_frame: &'a mut EraFuncExecFrame,
    cur_chunk: &'a EraBytecodeChunk,
}
impl EraErrReportContext<'_> {
    fn report_err<V: Into<String>>(&mut self, is_error: bool, msg: V) {
        EraVirtualMachine::report_err(
            self.callback,
            self.chunks,
            Some(self.cur_frame),
            is_error,
            msg,
        );
    }
    #[must_use]
    fn chunk_read_u8(&mut self, offset: usize) -> Option<u8> {
        match self.cur_chunk.read_u8(offset) {
            Some(x) => Some(x),
            None => {
                self.report_err(true, "ip exceeds bounds of chunk");
                None
            }
        }
    }
    #[must_use]
    fn chunk_read_u16(&mut self, offset: usize) -> Option<u16> {
        match self.cur_chunk.read_u16(offset) {
            Some(x) => Some(x),
            None => {
                self.report_err(true, "ip exceeds bounds of chunk");
                None
            }
        }
    }
    #[must_use]
    fn chunk_read_u32(&mut self, offset: usize) -> Option<u32> {
        match self.cur_chunk.read_u32(offset) {
            Some(x) => Some(x),
            None => {
                self.report_err(true, "ip exceeds bounds of chunk");
                None
            }
        }
    }
    #[must_use]
    fn pop_stack<const N: usize>(&mut self) -> Option<[EraTrappableValue; N]> {
        match self.stack.len().checked_sub(N) {
            Some(new_len) => {
                let mut it = self.stack.drain(new_len..);
                let res: [_; N] = std::array::from_fn(|_| it.next().unwrap());
                assert!(matches!(it.next(), None));
                Some(res)
            }
            None => {
                self.report_err(true, "too few elements in stack");
                None
            }
        }
    }
    #[must_use]
    fn pop_stack_dyn(
        &mut self,
        count: usize,
    ) -> Option<impl Iterator<Item = EraTrappableValue> + '_> {
        match self.stack.len().checked_sub(count) {
            Some(new_len) => Some(self.stack.drain(new_len..)),
            None => {
                self.report_err(true, "too few elements in stack");
                None
            }
        }
    }
    #[must_use]
    fn unpack_int(&mut self, value: Value) -> Option<crate::bytecode::IntValue> {
        match value.into_unpacked() {
            FlatValue::Int(x) => Some(x),
            _ => {
                self.report_err(true, "value must be an integer");
                None
            }
        }
    }
    #[must_use]
    fn unpack_str(&mut self, value: Value) -> Option<Rc<crate::bytecode::StrValue>> {
        match value.into_unpacked() {
            FlatValue::Str(x) => Some(x),
            _ => {
                self.report_err(true, "value must be a string");
                None
            }
        }
    }
    #[must_use]
    fn unpack_arrint(&mut self, value: Value) -> Option<Rc<RefCell<crate::bytecode::ArrIntValue>>> {
        match value.into_unpacked() {
            FlatValue::ArrInt(x) => Some(x),
            _ => {
                self.report_err(true, "value must be an array of integer");
                None
            }
        }
    }
    #[must_use]
    fn unpack_arrstr(&mut self, value: Value) -> Option<Rc<RefCell<crate::bytecode::ArrStrValue>>> {
        match value.into_unpacked() {
            FlatValue::ArrStr(x) => Some(x),
            _ => {
                self.report_err(true, "value must be an array of string");
                None
            }
        }
    }
}

// TODO: Use a JIT backend to improve performance

// TODO: Introduce report error context to simplify invoking
macro_rules! vm_report_err {
    ($ctx:expr, None, $is_error:expr, $msg:expr) => {
        Self::report_err($ctx.callback, $ctx.chunks, None, $is_error, $msg);
    };
    ($ctx:expr, $frame:expr, $is_error:expr, $msg:expr) => {
        Self::report_err($ctx.callback, $ctx.chunks, Some($frame), $is_error, $msg);
    };
    ($ctx:expr, $is_error:expr, $msg:expr) => {
        Self::report_err(
            $ctx.callback,
            $ctx.chunks,
            Some($ctx.cur_frame),
            $is_error,
            $msg,
        );
    };
}
macro_rules! bail_opt {
    ($ctx:expr, None, $is_error:expr, $msg:expr) => {{
        vm_report_err!($ctx, None, $is_error, $msg);
        return None;
    }};
    ($ctx:expr, $frame:expr, $is_error:expr, $msg:expr) => {{
        vm_report_err!($ctx, $frame, $is_error, $msg);
        return None;
    }};
    ($ctx:expr, $is_error:expr, $msg:expr) => {{
        vm_report_err!($ctx, $ctx.cur_frame, $is_error, $msg);
        return None;
    }};
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
        // unsafe { Some(*$chunk.bytecode.get_unchecked($offset)) }
    };
}
macro_rules! vm_read_chunk_u16 {
    ($ctx:expr, $frame:expr, $chunk:expr, $offset:expr) => {
        match $chunk.read_u16($offset) {
            Some(x) => Some(x),
            None => {
                vm_report_err!($ctx, $frame, true, "ip exceeds bounds of chunk");
                None
            }
        }
    };
}
macro_rules! vm_read_chunk_u32 {
    ($ctx:expr, $frame:expr, $chunk:expr, $offset:expr) => {
        match $chunk.read_u32($offset) {
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
    pub val: Value,
    pub is_const: bool,
    pub is_charadata: bool,
}

impl EraVarPool {
    pub fn new() -> Self {
        EraVarPool {
            var_names: HashMap::new(),
            vars: Vec::new(),
        }
    }
    #[must_use]
    pub fn add_var(&mut self, name: &str, val: Value) -> Option<usize> {
        self.add_var_ex(name, val, false, false)
    }
    #[must_use]
    pub fn add_var_ex(
        &mut self,
        name: &str,
        val: Value,
        is_const: bool,
        is_charadata: bool,
    ) -> Option<usize> {
        let name: Rc<CaselessStr> = CaselessStr::new(name).into();
        let var_idx = self.vars.len();
        match self.var_names.entry(name.clone()) {
            std::collections::hash_map::Entry::Occupied(e) => return None,
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(var_idx);
            }
        }
        self.vars.push(EraVarInfo {
            name,
            val,
            is_const,
            is_charadata,
        });
        Some(var_idx)
    }
    #[must_use]
    pub fn get_var(&self, name: &str) -> Option<&Value> {
        self.var_names
            .get(CaselessStr::new(name))
            .map(|x| &self.vars[*x].val)
    }
    #[must_use]
    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.var_names
            .get_mut(CaselessStr::new(name))
            .map(|x| &mut self.vars[*x].val)
    }
    #[must_use]
    pub fn get_var_by_idx(&mut self, idx: usize) -> Option<&Value> {
        self.vars.get(idx).map(|x| &x.val)
    }
    #[must_use]
    pub fn get_var_by_idx_mut(&mut self, idx: usize) -> Option<&mut Value> {
        self.vars.get_mut(idx).map(|x| &mut x.val)
    }
    #[must_use]
    pub fn get_var_idx(&self, name: &str) -> Option<usize> {
        self.var_names.get(CaselessStr::new(name)).copied()
    }
    #[must_use]
    pub fn get_var_info(&self, idx: usize) -> Option<&EraVarInfo> {
        self.vars.get(idx)
    }
    #[must_use]
    pub fn get_var_info_by_name(&self, name: &str) -> Option<&EraVarInfo> {
        self.get_var_idx(name).and_then(|idx| self.vars.get(idx))
    }
    pub fn iter(&self) -> impl Iterator<Item = &EraVarInfo> {
        self.vars.iter()
    }
}

// TODO: Variables requiring callback should be sent to the compiler for better codegen,
//       avoiding the need for trap representations
#[derive(Debug, Clone)]
struct EraTrappableValue {
    is_trap: bool,
    val: Value,
}
impl From<Value> for EraTrappableValue {
    fn from(value: Value) -> Self {
        EraTrappableValue {
            is_trap: false,
            val: value,
        }
    }
}
impl From<EraTrappableValue> for Value {
    fn from(value: EraTrappableValue) -> Self {
        value.val
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EraExecIp {
    pub chunk: usize,
    pub offset: usize,
}

#[derive(Debug)]
struct EraFuncExecFrame {
    stack_start: usize,
    ip: EraExecIp,
    ret_ip: EraExecIp,
    ignore_return_value: bool,
}

pub struct EraVirtualMachine {
    func_names: HashMap<Rc<CaselessStr>, usize>,
    funcs: Vec<EraFuncBytecodeInfo>,
    chunks: Vec<EraBytecodeChunk>,
    global_vars: EraVarPool,
    stack: Vec<EraTrappableValue>,
    frames: Vec<EraFuncExecFrame>,
    is_halted: bool,
    uniform_gen: SimpleUniformGenerator,
    // Variable address -> metadata index
    trap_vars: HashMap<*const (), usize>,
}

pub struct EraRuntimeErrorInfo {
    pub file_name: String,
    pub src_info: SourcePosInfo,
    pub is_error: bool,
    pub msg: String,
}

// Reference: https://learn.microsoft.com/en-us/dotnet/api/system.drawing.imaging.colormatrix
pub struct EraColorMatrix {
    pub m00: f32,
    pub m01: f32,
    pub m02: f32,
    pub m03: f32,
    pub m04: f32,
    pub m10: f32,
    pub m11: f32,
    pub m12: f32,
    pub m13: f32,
    pub m14: f32,
    pub m20: f32,
    pub m21: f32,
    pub m22: f32,
    pub m23: f32,
    pub m24: f32,
    pub m30: f32,
    pub m31: f32,
    pub m32: f32,
    pub m33: f32,
    pub m34: f32,
    pub m40: f32,
    pub m41: f32,
    pub m42: f32,
    pub m43: f32,
    pub m44: f32,
}
impl Default for EraColorMatrix {
    fn default() -> Self {
        EraColorMatrix {
            m00: 1.0,
            m01: 0.0,
            m02: 0.0,
            m03: 0.0,
            m04: 0.0,
            m10: 0.0,
            m11: 1.0,
            m12: 0.0,
            m13: 0.0,
            m14: 0.0,
            m20: 0.0,
            m21: 0.0,
            m22: 1.0,
            m23: 0.0,
            m24: 0.0,
            m30: 0.0,
            m31: 0.0,
            m32: 0.0,
            m33: 1.0,
            m34: 0.0,
            m40: 0.0,
            m41: 0.0,
            m42: 0.0,
            m43: 0.0,
            m44: 1.0,
        }
    }
}

pub trait EraVirtualMachineCallback {
    fn on_execution_error(&mut self, error: EraRuntimeErrorInfo);
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    // TODO: Debug is a global flag inside VM?
    fn on_debugprint(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags);
    fn on_html_print(&mut self, content: &str);
    fn on_wait(&mut self, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: i64,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<i64>;
    fn on_input_str(
        &mut self,
        default_value: &str,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<String>;
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error>;
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error>;
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error>;
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error>;
    // Graphics subsystem
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64;
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64;
    fn on_gdispose(&mut self, gid: i64) -> i64;
    fn on_gcreated(&mut self, gid: i64) -> i64;
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&EraColorMatrix>,
    ) -> i64;
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64;
    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64;
    fn on_spritedispose(&mut self, name: &str) -> i64;
    fn on_spritecreated(&mut self, name: &str) -> i64;
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64;
    fn on_spriteanimeaddframe(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64;
}

impl EraVirtualMachine {
    pub fn new(compilation: EraBytecodeCompilation) -> Self {
        let mut this = EraVirtualMachine {
            func_names: compilation.func_names,
            funcs: compilation.funcs,
            chunks: compilation.chunks,
            global_vars: compilation.global_vars,
            stack: Vec::new(),
            frames: Vec::new(),
            is_halted: false,
            uniform_gen: SimpleUniformGenerator::new(),
            trap_vars: HashMap::new(),
        };
        this
    }
    pub fn reset_exec_and_ip(&mut self, ip: EraExecIp) {
        // TODO: Verify input
        self.is_halted = false;
        self.stack.clear();
        self.stack.push(Value::new_int(0).into()); // Stub value
        self.frames.clear();
        self.frames.push(EraFuncExecFrame {
            stack_start: self.stack.len(),
            ip,
            ret_ip: EraExecIp {
                chunk: 0,
                offset: 0,
            },
            ignore_return_value: true,
        });
        self.uniform_gen = SimpleUniformGenerator::new();
    }
    pub fn get_is_halted(&self) -> bool {
        self.is_halted
    }
    pub fn register_var_callback(&mut self, name: &str) -> Option<()> {
        // TODO: When transitioning to JIT, we should mark the trap bit (i.e. the LSB) of
        //       the data pointer directly; It's OK for trapping to be slow as long as it
        //       does not affect the normal path too much.
        let idx = self.global_vars.get_var_idx(name)?;
        let var = self.global_vars.get_var_by_idx(idx)?;
        let ptr = match var.clone().into_unpacked() {
            FlatValue::Int(_) | FlatValue::Str(_) => return None,
            FlatValue::ArrInt(x) => Rc::as_ptr(&x) as _,
            FlatValue::ArrStr(x) => Rc::as_ptr(&x) as _,
        };
        self.trap_vars.insert(ptr, idx);
        Some(())
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

        macro_rules! make_ctx {
            ($self:expr, $ctx:expr) => {{
                $ctx = match loop {
                    break Ok(EraErrReportContext {
                        chunks: $self.chunks.as_slice(),
                        callback,
                        stack: &mut $self.stack,
                        cur_frame: match $self.frames.last_mut() {
                            Some(x) => x,
                            None => {
                                break Err("unexpected end of execution frame");
                            }
                        },
                        cur_chunk: match $self.chunks.first() {
                            Some(x) => x,
                            None => {
                                break Err("no chunks loaded");
                            }
                        },
                    });
                } {
                    Ok(ctx) => ctx,
                    Err(e) => {
                        Self::report_err(callback, &$self.chunks, None, true, e);
                        return None;
                    }
                };
                $ctx.cur_chunk = match self.chunks.get($ctx.cur_frame.ip.chunk) {
                    Some(x) => x,
                    None => {
                        $ctx.report_err(true, "ip exceeds bounds of chunk");
                        return None;
                    }
                };
            }};
        }

        let mut ctx;
        make_ctx!(self, ctx);

        let Some(FlatValue::ArrInt(vresult)) = self
            .global_vars
            .get_var("RESULT")
            .map(|x| x.clone().into_unpacked())
        else {
            bail_opt!(ctx, None, true, "variable RESULT not properly defined");
        };
        let Some(FlatValue::ArrStr(vresults)) = self
            .global_vars
            .get_var("RESULTS")
            .map(|x| x.clone().into_unpacked())
        else {
            bail_opt!(ctx, None, true, "variable RESULTS not properly defined");
        };

        for _ in 0..max_inst_cnt {
            if stop_flag.load(Ordering::Relaxed) {
                break;
            }

            let mut ip_offset_delta: isize = 1;
            // let primary_bytecode = unsafe { *cur_chunk.bytecode.get_unchecked(cur_frame.ip.offset) };
            // let primary_bytecode = unsafe { std::mem::transmute(primary_bytecode) };
            let primary_bytecode = ctx.chunk_read_u8(ctx.cur_frame.ip.offset)?;
            let primary_bytecode = match EraBytecodePrimaryType::try_from_i(primary_bytecode) {
                Some(x) => x,
                None => bail_opt!(
                    ctx,
                    true,
                    format!(
                        "invalid bytecode `{}` (memory corrupted?)",
                        primary_bytecode
                    )
                ),
            };
            match primary_bytecode {
                DebugBreak => break,
                Quit => return None,
                InvalidWithMessage => {
                    let msg = ctx.stack.pop().and_then(|x| match x.val.into_unpacked() {
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
                    bail_opt!(ctx, true, format!("invalid bytecode: {msg}"));
                }
                Pop => {
                    [_] = ctx.pop_stack()?;
                }
                ReturnVoid => {
                    ctx.stack.drain(ctx.cur_frame.stack_start..);
                    self.frames.pop();
                    make_ctx!(self, ctx);
                    ip_offset_delta = 0;
                }
                ReturnInteger | ReturnString => {
                    let [ret_val] = ctx.pop_stack()?;
                    ctx.stack.drain(ctx.cur_frame.stack_start..);
                    let ignore_return_value = self.frames.pop().unwrap().ignore_return_value;
                    make_ctx!(self, ctx);
                    if !ignore_return_value {
                        ctx.stack.push(ret_val);
                    }
                    ip_offset_delta = 0;
                }
                FunCall => {
                    let args_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)? as _;
                    ip_offset_delta += 1;
                    let [entry] = ctx.pop_stack()?;
                    // NOTE: We ignore the return value is we are looking up the callee
                    //       dynamically (i.e. by string instead of index)
                    let ignore_return_value;
                    let func_info = match entry.val.into_unpacked() {
                        // TODO: Check index overflow
                        FlatValue::Int(x) => {
                            ignore_return_value = false;
                            self.funcs.get(x.val as usize)
                        }
                        FlatValue::Str(x) => {
                            ignore_return_value = true;
                            self.func_names
                                .get(CaselessStr::new(&x.val))
                                .map(|&x| &self.funcs[x])
                        }
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    let func_info = match func_info {
                        Some(x) => x,
                        None => bail_opt!(ctx, true, "invalid index into function pool"),
                    };
                    if func_info.args.len() != args_cnt {
                        bail_opt!(
                            ctx,
                            true,
                            "runtime function argument mismatch (broken codegen?)"
                        );
                    }
                    ctx.cur_chunk = &self.chunks[func_info.chunk_idx as usize];
                    // TODO: Check whether stack_start exceeds bounds of current frame
                    let stack_start = match ctx.stack.len().checked_sub(args_cnt as _) {
                        Some(x) => x,
                        None => bail_opt!(ctx, true, "too few arguments for function call"),
                    };
                    let ip = EraExecIp {
                        chunk: func_info.chunk_idx as _,
                        offset: func_info.offset as _,
                    };
                    let ret_ip = EraExecIp {
                        chunk: ctx.cur_frame.ip.chunk,
                        offset: ctx.cur_frame.ip.offset.wrapping_add_signed(ip_offset_delta),
                    };
                    // HACK: Apply ret_ip immediately
                    ctx.cur_frame.ip = ret_ip;
                    ip_offset_delta = 0;

                    // Check call stack depth
                    const MAX_CALL_DEPTH: usize = 1024;
                    self.frames.push(EraFuncExecFrame {
                        stack_start,
                        ip,
                        ret_ip,
                        ignore_return_value,
                    });
                    let call_depth = self.frames.len();
                    make_ctx!(self, ctx);
                    if call_depth > MAX_CALL_DEPTH {
                        bail_opt!(
                            ctx,
                            true,
                            format!("call stack exceeds limit of depth {MAX_CALL_DEPTH}")
                        );
                    }
                }
                TryFunCall => {
                    let args_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)? as _;
                    ip_offset_delta += 1;
                    let [entry] = ctx.pop_stack()?;
                    // NOTE: We unconditionally ignore return values
                    let func_info = match entry.val.into_unpacked() {
                        // TODO: Check index overflow
                        FlatValue::Int(x) => self.funcs.get(x.val as usize),
                        FlatValue::Str(x) => self
                            .func_names
                            .get(CaselessStr::new(&x.val))
                            .map(|&x| &self.funcs[x]),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    if let Some(func_info) = func_info {
                        if func_info.args.len() != args_cnt {
                            bail_opt!(ctx, true, "runtime function argument mismatch");
                        }
                        // TODO: Check whether stack_start exceeds bounds of current frame
                        // Transform args pack into ordinary forms
                        let args_pack = ctx.pop_stack_dyn(args_cnt * 2)?.collect::<Vec<_>>();

                        // Function exists
                        ctx.stack.push(Value::new_int(1).into());

                        let stack_start = ctx.stack.len();
                        for (arg_pack, param) in
                            args_pack.chunks_exact(2).zip(func_info.args.iter())
                        {
                            let param_kind = param.kind();
                            match param_kind {
                                ValueKind::ArrInt | ValueKind::ArrStr => {
                                    if arg_pack[0].val.kind() != param_kind {
                                        bail_opt!(ctx, true, "wrong arg pack");
                                    }
                                    ctx.stack.push(arg_pack[0].val.clone().into());
                                }
                                ValueKind::Int | ValueKind::Str => {
                                    match (
                                        arg_pack[0].val.clone().into_unpacked(),
                                        arg_pack[1].val.clone().into_unpacked(),
                                    ) {
                                        (FlatValue::ArrInt(x), FlatValue::Int(y))
                                            if param_kind == ValueKind::Int =>
                                        {
                                            let x = x.borrow();
                                            let Some(val) = x.flat_get(y.val as _) else {
                                                bail_opt!(ctx, true, "wrong arg pack");
                                            };
                                            ctx.stack.push(Value::new_int_obj(val.clone()).into());
                                        }
                                        (FlatValue::ArrStr(x), FlatValue::Int(y))
                                            if param_kind == ValueKind::Str =>
                                        {
                                            let x = x.borrow();
                                            let Some(val) = x.flat_get(y.val as _) else {
                                                bail_opt!(ctx, true, "wrong arg pack");
                                            };
                                            ctx.stack.push(Value::new_str_rc(val.clone()).into());
                                        }
                                        (_, FlatValue::Int(y)) if param_kind == ValueKind::Int => {
                                            ctx.stack.push(Value::new_int_obj(y.clone()).into());
                                        }
                                        (_, FlatValue::Str(y)) if param_kind == ValueKind::Str => {
                                            ctx.stack.push(Value::new_str_rc(y.clone()).into());
                                        }
                                        _ => bail_opt!(ctx, true, "wrong arg pack"),
                                    }
                                }
                            }
                        }
                        ctx.cur_chunk = &self.chunks[func_info.chunk_idx as usize];
                        let ip = EraExecIp {
                            chunk: func_info.chunk_idx as _,
                            offset: func_info.offset as _,
                        };
                        let ret_ip = EraExecIp {
                            chunk: ctx.cur_frame.ip.chunk,
                            offset: ctx.cur_frame.ip.offset.wrapping_add_signed(ip_offset_delta),
                        };
                        // HACK: Apply ret_ip immediately
                        ctx.cur_frame.ip = ret_ip;
                        ip_offset_delta = 0;

                        // Check call stack depth
                        const MAX_CALL_DEPTH: usize = 1024;
                        self.frames.push(EraFuncExecFrame {
                            stack_start,
                            ip,
                            ret_ip,
                            ignore_return_value: true,
                        });
                        let call_depth = self.frames.len();
                        make_ctx!(self, ctx);
                        if call_depth > MAX_CALL_DEPTH {
                            bail_opt!(
                                ctx,
                                true,
                                format!("call stack exceeds limit of depth {MAX_CALL_DEPTH}")
                            );
                        }
                    } else {
                        // Function does not exist
                        ctx.stack.push(Value::new_int(0).into());
                    }
                }
                LoadConst | LoadConstW | LoadConstWW => {
                    let imm_ip = ctx.cur_frame.ip.offset + 1;
                    let offset_delta;
                    let entry: usize = match primary_bytecode {
                        LoadConst => {
                            offset_delta = 1;
                            ctx.chunk_read_u8(imm_ip)?.into()
                        }
                        LoadConstW => {
                            offset_delta = 2;
                            ctx.chunk_read_u16(imm_ip)?.into()
                        }
                        LoadConstWW => {
                            offset_delta = 4;
                            ctx.chunk_read_u32(imm_ip)? as _
                        }
                        _ => unreachable!(),
                    };
                    ip_offset_delta += offset_delta;
                    let value = match ctx.cur_chunk.get_constant(entry as usize) {
                        Some(x) => x,
                        None => bail_opt!(
                            ctx,
                            true,
                            format!(
                                "invalid index {} into constant pool of size {}",
                                entry,
                                ctx.cur_chunk.get_constants_cnt()
                            )
                        ),
                    };
                    ctx.stack.push(value.clone().into());
                }
                LoadIntegerImm8 => {
                    let imm = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)? as i8;
                    ip_offset_delta += 1;
                    ctx.stack.push(Value::new_int(imm as _).into());
                }
                BuildString => {
                    let count = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let mut new_str = String::new();
                    let mut err_msg = "";
                    for value in ctx.pop_stack_dyn(count.into())? {
                        match value.val.into_unpacked() {
                            FlatValue::Str(x) => new_str += &x.val,
                            _ => {
                                err_msg = "BuildString requires string values as operands";
                                break;
                            }
                        }
                    }
                    if !err_msg.is_empty() {
                        bail_opt!(ctx, true, err_msg);
                    }
                    let new_str = Value::new_str(new_str);
                    ctx.stack.push(new_str.into());
                }
                PadString => {
                    use unicode_width::UnicodeWidthStr;

                    let flags: PadStringFlags =
                        ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?.into();
                    ip_offset_delta += 1;
                    let [value, width] = ctx.pop_stack()?;
                    let width: usize = match width.val.into_unpacked() {
                        FlatValue::Int(x) => x.val.try_into().unwrap_or_default(),
                        _ => bail_opt!(ctx, true, "pad width must be an integer"),
                    };
                    let value = match value.val.into_unpacked() {
                        FlatValue::Str(x) => {
                            if x.val.width() >= width {
                                Value::new_str_rc(x)
                            } else {
                                let x = &x.val;
                                Value::new_str(match (flags.left_pad(), flags.right_pad()) {
                                    (true, false) => format!("{x:<width$}"),
                                    (false, true) | _ => format!("{x:>width$}"),
                                })
                            }
                        }
                        _ => bail_opt!(ctx, true, "pad target must be a string"),
                    };
                    ctx.stack.push(value.into());
                }
                ConvertToString => {
                    let [value] = ctx.pop_stack()?;
                    let value = match value.val.into_unpacked() {
                        FlatValue::Int(x) => Value::new_str(x.val.to_string()),
                        FlatValue::Str(x) => Value::new_str_rc(x),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    ctx.stack.push(value.into());
                }
                ConvertToInteger => {
                    let [value] = ctx.pop_stack()?;
                    let value = match value.val.into_unpacked() {
                        FlatValue::Int(x) => Value::new_int_obj(x),
                        FlatValue::Str(x) => {
                            let x = match x.val.parse() {
                                Ok(x) => x,
                                Err(_) => bail_opt!(
                                    ctx,
                                    true,
                                    format!(
                                        "string `{}` cannot be converted to a valid integer",
                                        x.val
                                    )
                                ),
                            };
                            Value::new_int(x)
                        }
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    ctx.stack.push(value.into());
                }
                Print | PrintExtended => {
                    let [value] = ctx.pop_stack()?;
                    let flags = match primary_bytecode {
                        Print => PrintExtendedFlags::new(),
                        PrintExtended => {
                            let flags = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?.into();
                            ip_offset_delta += 1;
                            flags
                        }
                        _ => unreachable!(),
                    };
                    match value.val.into_unpacked() {
                        FlatValue::Int(x) => ctx.callback.on_print(&x.val.to_string(), flags),
                        FlatValue::Str(x) => ctx.callback.on_print(&x.val, flags),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    }
                }
                Add => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                        (FlatValue::Int(lhs), FlatValue::Int(rhs)) => {
                            // TODO: Strict integer arithmetic check
                            Value::new_int(lhs.val.wrapping_add(rhs.val))
                        }
                        (FlatValue::Str(lhs), FlatValue::Str(rhs)) => {
                            Value::new_str(lhs.val.clone() + &rhs.val)
                        }
                        x => {
                            bail_opt!(ctx,
                                true,
                                format!("expected primitive values of same type as operands, found {x:?}")
                            );
                        }
                    };
                    ctx.stack.push(result.into());
                }
                Subtract => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                        (FlatValue::Int(lhs), FlatValue::Int(rhs)) => {
                            // TODO: Strict integer arithmetic check
                            Value::new_int(lhs.val.wrapping_sub(rhs.val))
                        }
                        x => bail_opt!(
                            ctx,
                            true,
                            format!("expected integer values as operands, found {x:?}")
                        ),
                    };
                    ctx.stack.push(result.into());
                }
                Multiply => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                        (FlatValue::Int(lhs), FlatValue::Int(rhs)) => {
                            // TODO: Strict integer arithmetic check
                            Value::new_int(lhs.val.wrapping_mul(rhs.val))
                        }
                        // TODO: String * Int
                        x => bail_opt!(
                            ctx,
                            true,
                            format!("expected <integer, integer> or <integer, string> as operands, found {x:?}")
                        ),
                    };
                    ctx.stack.push(result.into());
                }
                Divide => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                        (FlatValue::Int(lhs), FlatValue::Int(rhs)) => {
                            // TODO: Strict integer arithmetic check
                            if rhs.val == 0 {
                                bail_opt!(ctx, true, "division by zero");
                            }
                            Value::new_int(lhs.val.wrapping_div(rhs.val))
                        }
                        x => bail_opt!(
                            ctx,
                            true,
                            format!("expected integer values as operands, found {x:?}")
                        ),
                    };
                    ctx.stack.push(result.into());
                }
                Modulo => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                        (FlatValue::Int(lhs), FlatValue::Int(rhs)) => {
                            // TODO: Strict integer arithmetic check
                            if rhs.val == 0 {
                                bail_opt!(ctx, true, "modulus by zero");
                            }
                            // TODO: Determine the proper remainder operation?
                            Value::new_int(lhs.val.wrapping_rem(rhs.val))
                        }
                        x => bail_opt!(
                            ctx,
                            true,
                            format!("expected integer values as operands, found {x:?}")
                        ),
                    };
                    ctx.stack.push(result.into());
                }
                Negate => {
                    let [value] = ctx.pop_stack()?;
                    let result = match value.val.into_unpacked() {
                        FlatValue::Int(x) => Value::new_int(x.val.wrapping_neg()),
                        _ => bail_opt!(ctx, true, "expected integer values as operands"),
                    };
                    ctx.stack.push(result.into());
                }
                BitAnd | BitOr | BitXor | BitShiftL | BitShiftR => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result =
                        Value::new_int(match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                            (FlatValue::Int(lhs), FlatValue::Int(rhs)) => match primary_bytecode {
                                BitAnd => lhs.val & rhs.val,
                                BitOr => lhs.val | rhs.val,
                                BitXor => lhs.val ^ rhs.val,
                                BitShiftL => lhs.val.wrapping_shl(rhs.val as _),
                                BitShiftR => lhs.val.wrapping_shr(rhs.val as _),
                                _ => unreachable!(),
                            },
                            _ => bail_opt!(ctx, true, "expected integer values as operands"),
                        });
                    ctx.stack.push(result.into());
                }
                CompareL | CompareEq | CompareLEq => {
                    let [lhs, rhs] = ctx.pop_stack()?;
                    let result = Value::new_int(
                        match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                            (FlatValue::Int(lhs), FlatValue::Int(rhs)) => match primary_bytecode {
                                CompareL => lhs.val < rhs.val,
                                CompareEq => lhs.val == rhs.val,
                                CompareLEq => lhs.val <= rhs.val,
                                _ => unreachable!(),
                            },
                            (FlatValue::Str(lhs), FlatValue::Str(rhs)) => match primary_bytecode {
                                CompareL => lhs.val < rhs.val,
                                CompareEq => lhs.val == rhs.val,
                                CompareLEq => lhs.val <= rhs.val,
                                _ => unreachable!(),
                            },
                            _ => bail_opt!(
                                ctx,
                                true,
                                "expected <integer, integer> or <string, string> as operands"
                            ),
                        }
                        .into(),
                    );
                    ctx.stack.push(result.into());
                }
                LogicalAnd | LogicalOr => {
                    // TODO: Remove this
                    todo!()
                    // let [lhs, rhs] = vm_pop_stack!(ctx, cur_frame, &mut self.stack, 2)?;
                    // let result = Value::new_int(match (lhs.val.into_unpacked(), rhs.val.into_unpacked()) {
                    //     (FlatValue::Int(lhs), FlatValue::Int(rhs)) => match primary_bytecode {
                    //         LogicalAnd => lhs.val && rhs.val,
                    //         LogicalOr => lhs.val || rhs.val,
                    //         _ => unreachable!(),
                    //     },
                    //     _ => {
                    //         vm_report_err!(
                    //             ctx,
                    //             cur_frame,
                    //             true,
                    //             "expected integer values as operands"
                    //         );
                    //         return None;
                    //     }
                    // });
                    // ctx.stack.push(result);
                }
                LogicalNot => {
                    let [value] = ctx.pop_stack()?;
                    let result = Value::new_int(match value.val.into_unpacked() {
                        FlatValue::Int(x) => (x.val == 0) as _,
                        _ => bail_opt!(ctx, true, "expected integer values as operands"),
                    });
                    ctx.stack.push(result.into());
                }
                Duplicate => {
                    let [value] = ctx.pop_stack()?;
                    ctx.stack.push(value.clone());
                    ctx.stack.push(value);
                }
                DuplicateN => {
                    let count = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    match ctx.stack.len().checked_sub(count as _) {
                        Some(new_len) => {
                            for i in new_len..ctx.stack.len() {
                                let cloned = ctx.stack[i].clone();
                                ctx.stack.push(cloned);
                            }
                        }
                        None => bail_opt!(ctx, true, "too few elements in stack"),
                    }
                }
                DuplicateOneN => {
                    let count = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    if count <= 0 {
                        bail_opt!(ctx, true, "invalid count for DuplicateOneN");
                    }
                    match ctx.stack.len().checked_sub(count as _) {
                        Some(new_idx) => {
                            let cloned = ctx.stack[new_idx].clone();
                            ctx.stack.push(cloned);
                        }
                        None => bail_opt!(ctx, true, "too few elements in stack"),
                    }
                }
                DeepClone => {
                    let [value] = ctx.pop_stack()?;
                    ctx.stack.push(value.val.deep_clone().into());
                }
                GetGlobal => {
                    let [value] = ctx.pop_stack()?;
                    let value = match value.val.into_unpacked() {
                        // TODO: Check if index is in function frame
                        FlatValue::Int(x) => self.global_vars.get_var_by_idx(x.val as _),
                        FlatValue::Str(x) => self.global_vars.get_var(&x.val),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    let value = match value {
                        Some(x) => x,
                        None => bail_opt!(ctx, true, "invalid index into global variable pool"),
                    };
                    ctx.stack.push(EraTrappableValue {
                        is_trap: Self::check_trap_var(&self.trap_vars, value).is_some(),
                        val: value.clone(),
                    });
                }
                SetGlobal => {
                    let [index, src_value] = ctx.pop_stack()?;
                    let value = match index.val.into_unpacked() {
                        // TODO: Check if index is in function frame
                        FlatValue::Int(x) => self.global_vars.get_var_by_idx_mut(x.val as _),
                        FlatValue::Str(x) => self.global_vars.get_var_mut(&x.val),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    let value = match value {
                        Some(x) => x,
                        None => bail_opt!(ctx, true, "invalid index into global variable pool"),
                    };
                    *value = src_value.into();
                }
                GetLocal => {
                    let [index] = ctx.pop_stack()?;
                    let value = match index.val.into_unpacked() {
                        FlatValue::Int(x) => {
                            // TODO: Check if index is in bounds
                            match ctx
                                .stack
                                .get(ctx.cur_frame.stack_start.saturating_add_signed(x.val as _))
                            {
                                Some(x) => x,
                                None => bail_opt!(
                                    ctx,
                                    true,
                                    "invalid index into function execution frame"
                                ),
                            }
                        }
                        _ => bail_opt!(ctx, true, "expected integer values as operands"),
                    };
                    ctx.stack.push(value.clone());
                }
                SetLocal => {
                    let [index, src_value] = ctx.pop_stack()?;
                    let value = match index.val.into_unpacked() {
                        FlatValue::Int(x) => {
                            // TODO: Check if index is in bounds
                            match ctx.stack.get_mut(
                                ctx.cur_frame.stack_start.saturating_add_signed(x.val as _),
                            ) {
                                Some(x) => x,
                                None => bail_opt!(
                                    ctx,
                                    true,
                                    "invalid index into function execution frame"
                                ),
                            }
                        }
                        _ => bail_opt!(ctx, true, "expected integers as operands"),
                    };
                    *value = src_value.clone();
                    ctx.stack.push(src_value);
                }
                GetMDArrayVal => {
                    let idxs_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let idxs = ctx
                        .pop_stack_dyn(idxs_cnt as _)?
                        .map(|x| match x.val.into_unpacked() {
                            // TODO: Check overflow
                            FlatValue::Int(x) => Some(x.val as u32),
                            _ => None,
                        })
                        .collect::<Option<Vec<_>>>();
                    let Some(idxs) = idxs else {
                        bail_opt!(ctx, true, "expected integers as operands");
                    };
                    let [arr] = ctx.pop_stack()?;
                    let value = match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let x_ptr = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if arr.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&x_ptr).unwrap())
                                    .unwrap();
                                if let Some(xi) = x.get_mut(&idxs) {
                                    let v = match ctx.callback.on_var_get_int(
                                        trap_var_info.name.as_str(),
                                        idxs.last().copied().unwrap_or_default() as _,
                                    ) {
                                        Ok(v) => v,
                                        Err(e) => bail_opt!(
                                            ctx,
                                            true,
                                            format!("trap handler failed: {e}")
                                        ),
                                    };
                                    xi.val = v;
                                    Some(Value::new_int_obj(xi.clone()))
                                } else {
                                    None
                                }
                            } else {
                                x.get(&idxs).map(|x| Value::new_int_obj(x.clone()))
                            }
                            //x.borrow().get(&idxs).map(|x| Value::new_int_obj(x.clone()))
                        }
                        FlatValue::ArrStr(x) => {
                            let x_ptr = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if arr.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&x_ptr).unwrap())
                                    .unwrap();
                                if let Some(xi) = x.get_mut(&idxs) {
                                    let v = match ctx.callback.on_var_get_str(
                                        trap_var_info.name.as_str(),
                                        idxs.last().copied().unwrap_or_default() as _,
                                    ) {
                                        Ok(v) => v,
                                        Err(e) => bail_opt!(
                                            ctx,
                                            true,
                                            format!("trap handler failed: {e}")
                                        ),
                                    };
                                    *xi = Rc::new(StrValue { val: v });
                                    Some(Value::new_str_rc(xi.clone()))
                                } else {
                                    None
                                }
                            } else {
                                x.get(&idxs).map(|x| Value::new_str_rc(x.clone()))
                            }
                            //x.borrow().get(&idxs).map(|x| Value::new_str_rc(x.clone()))
                        }
                        _ => {
                            vm_report_err!(ctx, true, "expected arrays as operands");
                            return None;
                        }
                    };
                    let value = match value {
                        Some(x) => x,
                        None => {
                            vm_report_err!(ctx, true, "invalid indices into array");
                            return None;
                        }
                    };
                    ctx.stack.push(value.into());
                }
                SetMDArrayVal => {
                    let idxs_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let [src_value] = ctx.pop_stack()?;
                    let idxs = ctx
                        .pop_stack_dyn(idxs_cnt as _)?
                        .map(|x| match x.val.into_unpacked() {
                            // TODO: Check overflow
                            FlatValue::Int(x) => Some(x.val as u32),
                            _ => None,
                        })
                        .collect::<Option<Vec<_>>>();
                    let Some(idxs) = idxs else {
                        bail_opt!(ctx, true, "expected integers as operands");
                    };
                    let [dst_value] = ctx.pop_stack()?;
                    let is_trap = dst_value.is_trap;
                    match (
                        dst_value.val.into_unpacked(),
                        src_value.clone().val.into_unpacked(),
                    ) {
                        (FlatValue::ArrInt(dst), FlatValue::Int(src)) => {
                            let dst_ptr = Rc::as_ptr(&dst) as _;
                            let mut dst = dst.borrow_mut();
                            match dst.get_mut(&idxs) {
                                Some(x) => x.val = src.val,
                                None => bail_opt!(ctx, true, "invalid indices into array"),
                            }
                            if is_trap {
                                assert_eq!(
                                    dst.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&dst_ptr).unwrap())
                                    .unwrap();
                                match ctx.callback.on_var_set_int(
                                    trap_var_info.name.as_str(),
                                    *idxs.last().unwrap() as _,
                                    src.val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        bail_opt!(ctx, true, format!("trap handler failed: {e}"))
                                    }
                                }
                            }
                        }
                        (FlatValue::ArrStr(dst), FlatValue::Str(src)) => {
                            let dst_ptr = Rc::as_ptr(&dst) as _;
                            let mut dst = dst.borrow_mut();
                            match dst.get_mut(&idxs) {
                                Some(x) => *x = src.clone(),
                                None => bail_opt!(ctx, true, "invalid indices into array"),
                            }
                            if is_trap {
                                assert_eq!(
                                    dst.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&dst_ptr).unwrap())
                                    .unwrap();
                                match ctx.callback.on_var_set_str(
                                    trap_var_info.name.as_str(),
                                    *idxs.last().unwrap() as _,
                                    &src.val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        bail_opt!(ctx, true, format!("trap handler failed: {e}"))
                                    }
                                }
                            }
                        }
                        (FlatValue::ArrInt(_) | FlatValue::ArrStr(_), _) => {
                            bail_opt!(ctx, true, "value type mismatches array element type")
                        }
                        _ => bail_opt!(ctx, true, "destination is not an array"),
                    }
                    ctx.stack.push(src_value);
                }
                GetArrayVal => {
                    let [dst, idx] = ctx.pop_stack()?;
                    let FlatValue::Int(idx) = idx.val.into_unpacked() else {
                        bail_opt!(ctx, true, "invalid indices into array");
                    };
                    // TODO: Check overflow
                    let idx = idx.val as usize;
                    // let value = match dst.val.into_unpacked() {
                    //     FlatValue::ArrInt(x) => x
                    //         .borrow()
                    //         .flat_get(idx)
                    //         .map(|x| Value::new_int_obj(x.clone())),
                    //     FlatValue::ArrStr(x) => x
                    //         .borrow()
                    //         .flat_get(idx)
                    //         .map(|x| Value::new_str_rc(x.clone())),
                    //     _ => bail_opt!(ctx, cur_frame, true, "expected arrays as operands"),
                    // };
                    let value = match dst.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let x_ptr = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if dst.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&x_ptr).unwrap())
                                    .unwrap();
                                if let Some(xi) = x.flat_get_mut(idx) {
                                    let v = match ctx
                                        .callback
                                        .on_var_get_int(trap_var_info.name.as_str(), idx as _)
                                    {
                                        Ok(v) => v,
                                        Err(e) => bail_opt!(
                                            ctx,
                                            true,
                                            format!("trap handler failed: {e}")
                                        ),
                                    };
                                    xi.val = v;
                                    Some(Value::new_int_obj(xi.clone()))
                                } else {
                                    None
                                }
                            } else {
                                x.flat_get(idx).map(|x| Value::new_int_obj(x.clone()))
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let x_ptr = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if dst.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&x_ptr).unwrap())
                                    .unwrap();
                                if let Some(xi) = x.flat_get_mut(idx) {
                                    let v = match ctx
                                        .callback
                                        .on_var_get_str(trap_var_info.name.as_str(), idx as _)
                                    {
                                        Ok(v) => v,
                                        Err(e) => bail_opt!(
                                            ctx,
                                            true,
                                            format!("trap handler failed: {e}")
                                        ),
                                    };
                                    *xi = Rc::new(StrValue { val: v });
                                    Some(Value::new_str_rc(xi.clone()))
                                } else {
                                    None
                                }
                            } else {
                                x.flat_get(idx).map(|x| Value::new_str_rc(x.clone()))
                            }
                        }
                        _ => bail_opt!(ctx, true, "expected arrays as operands"),
                    };
                    let Some(value) = value else {
                        bail_opt!(ctx, true, "invalid indices into array");
                    };
                    ctx.stack.push(value.into());
                }
                SetArrayVal => {
                    let [dst, idx, src] = ctx.pop_stack()?;
                    let FlatValue::Int(idx) = idx.val.into_unpacked() else {
                        bail_opt!(ctx, true, "invalid indices into array");
                    };
                    // TODO: Check overflow
                    let idx = idx.val as usize;
                    let is_trap = dst.is_trap;
                    match (dst.val.into_unpacked(), src.clone().val.into_unpacked()) {
                        (FlatValue::ArrInt(dst), FlatValue::Int(src)) => {
                            let dst_ptr = Rc::as_ptr(&dst) as _;
                            let mut dst = dst.borrow_mut();
                            match dst.flat_get_mut(idx) {
                                Some(x) => x.val = src.val,
                                None => {
                                    bail_opt!(ctx, true, "invalid indices into array")
                                }
                            }
                            if is_trap {
                                assert_eq!(
                                    dst.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&dst_ptr).unwrap())
                                    .unwrap();
                                match ctx.callback.on_var_set_int(
                                    trap_var_info.name.as_str(),
                                    idx,
                                    src.val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        bail_opt!(ctx, true, format!("trap handler failed: {e}"))
                                    }
                                }
                            }
                        }
                        (FlatValue::ArrStr(dst), FlatValue::Str(src)) => {
                            let dst_ptr = Rc::as_ptr(&dst) as _;
                            let mut dst = dst.borrow_mut();
                            match dst.flat_get_mut(idx) {
                                Some(x) => *x = src.clone(),
                                None => {
                                    bail_opt!(ctx, true, "invalid indices into array")
                                }
                            }
                            if is_trap {
                                assert_eq!(
                                    dst.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = self
                                    .global_vars
                                    .get_var_info(*self.trap_vars.get(&dst_ptr).unwrap())
                                    .unwrap();
                                match ctx.callback.on_var_set_str(
                                    trap_var_info.name.as_str(),
                                    idx,
                                    &src.val,
                                ) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        bail_opt!(ctx, true, format!("trap handler failed: {e}"))
                                    }
                                }
                            }
                        }
                        (FlatValue::ArrInt(_) | FlatValue::ArrStr(_), _) => {
                            bail_opt!(ctx, true, "value type mismatches array element type")
                        }
                        _ => bail_opt!(ctx, true, "destination is not an array"),
                    }
                    ctx.stack.push(src);
                }
                CopyArrayContent => {
                    // TODO: Support trapped arrays???
                    let [dst, src] = ctx.pop_stack()?;
                    match (dst.val.into_unpacked(), src.val.into_unpacked()) {
                        (FlatValue::ArrInt(dst), FlatValue::ArrInt(src)) => {
                            // We must not borrow the same array twice
                            if !Rc::ptr_eq(&dst, &src) {
                                let mut dst = dst.borrow_mut();
                                let src = src.borrow();
                                if dst.dims != src.dims {
                                    bail_opt!(
                                        ctx,
                                        true,
                                        "expected arrays of same dimensions as operands"
                                    );
                                }
                                src.vals.clone_into(&mut dst.vals);
                            }
                        }
                        (FlatValue::ArrStr(dst), FlatValue::ArrStr(src)) => {
                            // We must not borrow the same array twice
                            if !Rc::ptr_eq(&dst, &src) {
                                let mut dst = dst.borrow_mut();
                                let src = src.borrow();
                                if dst.dims != src.dims {
                                    bail_opt!(
                                        ctx,
                                        true,
                                        "expected arrays of same dimensions as operands"
                                    );
                                }
                                src.vals.clone_into(&mut dst.vals);
                            }
                        }
                        _ => bail_opt!(ctx, true, "expected arrays of same type as operands"),
                    }
                }
                GetRandomMax => {
                    let [upper] = ctx.pop_stack()?;
                    let upper = match upper.val.into_unpacked() {
                        FlatValue::Int(x) if x.val > 0 => x.val as _,
                        _ => bail_opt!(ctx, true, "expected positive integers as operands"),
                    };

                    let result = self
                        .uniform_gen
                        .gen_range(0..upper, || ctx.callback.on_get_rand());
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                Jump | JumpW => {
                    let imm_ip = ctx.cur_frame.ip.offset + 1;
                    let offset: isize = match primary_bytecode {
                        Jump => ctx.chunk_read_u8(imm_ip)? as i8 as _,
                        JumpW => ctx.chunk_read_u16(imm_ip)? as i16 as _,
                        _ => unreachable!(),
                    };
                    ip_offset_delta = offset;
                }
                JumpCond | JumpCondW => {
                    let imm_ip = ctx.cur_frame.ip.offset + 1;
                    let offset_delta;
                    let offset: isize = match primary_bytecode {
                        JumpCond => {
                            offset_delta = 1;
                            ctx.chunk_read_u8(imm_ip)? as i8 as _
                        }
                        JumpCondW => {
                            offset_delta = 2;
                            ctx.chunk_read_u16(imm_ip)? as i16 as _
                        }
                        _ => unreachable!(),
                    };
                    ip_offset_delta += offset_delta;
                    let [value] = ctx.pop_stack()?;
                    match value.val.into_unpacked() {
                        FlatValue::Int(x) => {
                            if x.val != 0 {
                                ip_offset_delta = offset;
                            }
                        }
                        FlatValue::Str(_) => ip_offset_delta = offset,
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    }
                }
                Throw => {
                    let msg = ctx.stack.pop().and_then(|x| match x.val.into_unpacked() {
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
                    bail_opt!(ctx, true, format!("THROW: {msg}"));
                }
                MaximumInt => {
                    let [a1, a2] = ctx.pop_stack()?;
                    let a1 = ctx.unpack_int(a1.into())?;
                    let a2 = ctx.unpack_int(a2.into())?;
                    ctx.stack.push(Value::new_int(a1.val.max(a2.val)).into());
                }
                MinimumInt => {
                    let [a1, a2] = ctx.pop_stack()?;
                    let a1 = ctx.unpack_int(a1.into())?;
                    let a2 = ctx.unpack_int(a2.into())?;
                    ctx.stack.push(Value::new_int(a1.val.min(a2.val)).into());
                }
                ClampInt => {
                    let [a, amax, amin] = ctx.pop_stack()?;
                    let a = ctx.unpack_int(a.into())?;
                    let amax = ctx.unpack_int(amax.into())?;
                    let amin = ctx.unpack_int(amin.into())?;
                    if amin.val > amax.val {
                        bail_opt!(ctx, true, "precondition min <= max was not satisfied");
                    }
                    ctx.stack
                        .push(Value::new_int(a.val.clamp(amin.val, amax.val)).into());
                }
                InRangeInt => {
                    let [a, amax, amin] = ctx.pop_stack()?;
                    let a = ctx.unpack_int(a.into())?;
                    let amax = ctx.unpack_int(amax.into())?;
                    let amin = ctx.unpack_int(amin.into())?;
                    if amin.val > amax.val {
                        bail_opt!(ctx, true, "precondition min <= max was not satisfied");
                    }
                    let in_range = (amin.val..=amax.val).contains(&a.val);
                    ctx.stack.push(Value::new_int(in_range.into()).into());
                }
                GetBit => {
                    let [val, bit] = ctx.pop_stack()?;
                    let val = ctx.unpack_int(val.into())?;
                    let bit = ctx.unpack_int(bit.into())?;
                    if !(0..64).contains(&bit.val) {
                        bail_opt!(ctx, true, "precondition 0 <= bit < 64 was not satisfied");
                    }
                    let is_bit_set = (val.val & (1 << bit.val)) != 0;
                    ctx.stack.push(Value::new_int(is_bit_set.into()).into());
                }
                GCreate => {
                    let [gid, width, height] = ctx.pop_stack()?;
                    let gid = ctx.unpack_int(gid.into())?;
                    let width = ctx.unpack_int(width.into())?;
                    let height = ctx.unpack_int(height.into())?;
                    let ret = ctx.callback.on_gcreate(gid.val, width.val, height.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                GCreateFromFile => {
                    let [gid, file_path] = ctx.pop_stack()?;
                    let gid = ctx.unpack_int(gid.into())?;
                    let file_path = ctx.unpack_str(file_path.into())?;
                    let ret = ctx.callback.on_gcreatefromfile(gid.val, &file_path.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                Invalid | _ => bail_opt!(
                    ctx,
                    true,
                    format!("invalid or unimplemented bytecode `{primary_bytecode:?}`")
                ),
            }

            ctx.cur_frame.ip.offset =
                match ctx.cur_frame.ip.offset.checked_add_signed(ip_offset_delta) {
                    Some(x) => x,
                    None => bail_opt!(ctx, true, "ip exceeds bounds of chunk"),
                }
        }

        Some(())
    }
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
    fn check_trap_var(trap_vars: &HashMap<*const (), usize>, value: &Value) -> Option<usize> {
        let addr = match value.clone().into_unpacked() {
            FlatValue::Int(_) | FlatValue::Str(_) => return None,
            FlatValue::ArrInt(x) => Rc::as_ptr(&x) as _,
            FlatValue::ArrStr(x) => Rc::as_ptr(&x) as _,
        };
        trap_vars.get(&addr).copied()
    }
}

struct SimpleUniformGenerator {
    entropy: u128,
    entropy_size: u32,
}
impl SimpleUniformGenerator {
    fn new() -> Self {
        SimpleUniformGenerator {
            entropy: 0,
            entropy_size: 0,
        }
    }
    fn gen_range(
        &mut self,
        range: impl std::ops::RangeBounds<u64>,
        mut feeder: impl FnMut() -> u64,
    ) -> u64 {
        use std::ops::Bound;

        let mut get_bits_fn = |bits: u32| -> u64 {
            if bits == 0 {
                return 0;
            }
            if bits > 64 {
                panic!("invalid bits count");
            }
            if self.entropy_size < bits {
                self.entropy += (feeder() as u128) << self.entropy_size;
                self.entropy_size += 64;
            }
            let result = self.entropy & ((1u128 << bits) - 1);
            self.entropy >>= bits;
            self.entropy_size -= bits;
            result as _
        };

        let low = match range.start_bound() {
            Bound::Unbounded => u64::MIN,
            Bound::Excluded(&x) => x.checked_add(1).expect("invalid range"),
            Bound::Included(&x) => x,
        };
        let len = match range.end_bound() {
            Bound::Unbounded => match (u64::MAX - low).checked_add(1) {
                Some(x) => x,
                None => return get_bits_fn(64),
            },
            Bound::Excluded(&x) if x > low => x - low,
            Bound::Included(&x) if x >= low => match (x - low).checked_add(1) {
                Some(x) => x,
                None => return get_bits_fn(64),
            },
            _ => panic!("invalid range"),
        };

        // Rejection sampling
        let bits_cnt = (len as u128).next_power_of_two().trailing_zeros();
        loop {
            let data = get_bits_fn(bits_cnt);
            if data < len {
                break low + data;
            }
        }
    }
}
