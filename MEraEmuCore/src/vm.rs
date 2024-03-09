use either::Either;
use itertools::Itertools;

use crate::{
    bytecode::{
        ArrIntValue, ArrStrValue, EraBytecodePrimaryType, EraCharaInitTemplate,
        EraCsvGetProp2SubBytecodeType, EraInputSubBytecodeType, FlatValue, IntValue,
        PadStringFlags, PrintExtendedFlags, SourcePosInfo, StrValue, Value, ValueKind,
    },
    compiler::{EraBytecodeChunk, EraBytecodeCompilation, EraFuncBytecodeInfo},
    util::*,
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    future::Future,
    mem::MaybeUninit,
    rc::Rc,
    sync::atomic::{AtomicBool, Ordering},
};

struct EraVirtualMachineContext<'a> {
    chunks: &'a [EraBytecodeChunk],
    callback: &'a mut dyn EraVirtualMachineCallback,
    stack: &'a mut Vec<EraTrappableValue>,
    cur_frame: &'a mut EraFuncExecFrame,
    cur_chunk: &'a EraBytecodeChunk,
    global_vars: &'a mut EraVarPool,
}
impl EraVirtualMachineContext<'_> {
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn pop_stack<const N: usize>(&mut self) -> Option<[EraTrappableValue; N]> {
        match self.stack.len().checked_sub(N) {
            Some(new_len) => {
                // let mut it = self.stack.drain(new_len..);
                // let res: [_; N] = std::array::from_fn(|_| it.next().unwrap());
                // assert!(matches!(it.next(), None));
                // TODO: SAFETY
                let res = unsafe {
                    let mut res: MaybeUninit<[EraTrappableValue; N]> = MaybeUninit::uninit();
                    (res.as_mut_ptr() as *mut EraTrappableValue)
                        .copy_from(self.stack.as_ptr().add(new_len) as _, N);
                    self.stack.set_len(new_len);
                    res.assume_init()
                };
                Some(res)
            }
            None => {
                self.report_err(true, "too few elements in stack");
                None
            }
        }
    }
    // #[must_use]
    // #[inline(always)]
    // fn pop_stack_dyn(
    //     &mut self,
    //     count: usize,
    // ) -> Option<smallvec::SmallVec<[EraTrappableValue; 3]>> {
    //     const INLINE_LEN: usize = 3;
    //     match self.stack.len().checked_sub(count) {
    //         Some(new_len) => {
    //             let res = if count <= INLINE_LEN {
    //                 // TODO: SAFETY
    //                 unsafe {
    //                     let mut res: MaybeUninit<[EraTrappableValue; INLINE_LEN]> =
    //                         MaybeUninit::uninit();
    //                     (res.as_mut_ptr() as *mut EraTrappableValue)
    //                         .copy_from(self.stack.as_ptr().add(new_len) as _, count);
    //                     self.stack.set_len(new_len);
    //                     smallvec::SmallVec::from_buf_and_len_unchecked(res, count)
    //                 }
    //             } else {
    //                 let mut res: smallvec::SmallVec<[EraTrappableValue; INLINE_LEN]> =
    //                     smallvec::SmallVec::with_capacity(count);
    //                 // TODO: SAFETY
    //                 unsafe {
    //                     res.as_mut_ptr()
    //                         .copy_from(self.stack.as_ptr().add(new_len) as _, count);
    //                     self.stack.set_len(new_len);
    //                     res.set_len(count);
    //                 }
    //                 res
    //             };
    //             Some(res)
    //         }
    //         None => {
    //             self.report_err(true, "too few elements in stack");
    //             None
    //         }
    //     }
    // }
    #[must_use]
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn unpack_arrstr(&mut self, value: Value) -> Option<Rc<RefCell<crate::bytecode::ArrStrValue>>> {
        match value.into_unpacked() {
            FlatValue::ArrStr(x) => Some(x),
            _ => {
                self.report_err(true, "value must be an array of string");
                None
            }
        }
    }
    #[must_use]
    #[inline(always)]
    fn get_var_arrint(&mut self, name: &str) -> Option<Rc<RefCell<crate::bytecode::ArrIntValue>>> {
        let Some(FlatValue::ArrInt(var)) = self
            .global_vars
            .get_var(name)
            .map(|x| x.clone().into_unpacked())
        else {
            self.report_err(true, format!("variable `{name}` not properly defined"));
            return None;
        };
        Some(var)
    }
    #[must_use]
    #[inline(always)]
    fn get_var_arrstr(&mut self, name: &str) -> Option<Rc<RefCell<crate::bytecode::ArrStrValue>>> {
        let Some(FlatValue::ArrStr(var)) = self
            .global_vars
            .get_var(name)
            .map(|x| x.clone().into_unpacked())
        else {
            self.report_err(true, format!("variable `{name}` not properly defined"));
            return None;
        };
        Some(var)
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

macro_rules! unpack_int {
    ($ctx:expr, $value:ident) => {
        let $value = $ctx.unpack_int($value.into())?;
    };
}
macro_rules! unpack_str {
    ($ctx:expr, $value:ident) => {
        let $value = $ctx.unpack_str($value.into())?;
    };
}
macro_rules! unpack_arrint_mut {
    ($ctx:expr, $value:ident) => {
        let $value = $ctx.unpack_arrint($value.into())?;
        let mut $value = $value.borrow_mut();
    };
}
macro_rules! unpack_arrstr_mut {
    ($ctx:expr, $value:ident) => {
        let $value = $ctx.unpack_arrstr($value.into())?;
        let mut $value = $value.borrow_mut();
    };
}
macro_rules! unpack_arrint_idx_mut {
    ($ctx:expr, $value:ident, $index:expr) => {
        let $value = $ctx.unpack_arrint($value.into())?;
        let mut $value = $value.borrow_mut();
        let Some($value) = $value.flat_get_mut($index as _) else {
            bail_opt!($ctx, true, "invalid indices into array");
        };
    };
}
macro_rules! arr_idx_mut {
    ($ctx:expr, $value:ident, $index:expr) => {
        let Some($value) = $value.flat_get_mut($index as _) else {
            bail_opt!($ctx, true, "invalid indices into array");
        };
    };
}

macro_rules! pop_stack_inner_unpack {
    ($ctx:expr, $var_name:ident:i) => {
        unpack_int!($ctx, $var_name);
    };
    ($ctx:expr, $var_name:ident:s) => {
        unpack_str!($ctx, $var_name);
    };
    // -----
    ($ctx:expr, $var_name:ident:b) => {
        unpack_int!($ctx, $var_name);
        let $var_name = $var_name.val != 0;
    };
}
macro_rules! pop_stack {
    ($ctx:expr, $($var_name:ident:$type:ident),+) => {
        let [$($var_name),+] = $ctx.pop_stack()?;
        $(
            pop_stack_inner_unpack!($ctx, $var_name:$type);
        )+
    };
}

#[derive(Default)]
pub struct EraVarPool {
    // Mapping from names to indices.
    var_names: HashMap<Rc<CaselessStr>, usize>,
    chara_var_idxs: Vec<usize>,
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
            chara_var_idxs: Vec::new(),
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
        if is_charadata {
            self.chara_var_idxs.push(var_idx);
        }
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
    trap_vars: hashbrown::HashMap<*const (), usize>,
    chara_templates: BTreeMap<u32, EraCharaInitTemplate>,
    charas_count: usize,
}

pub struct EraVirtualMachineStackTraceFrame {
    pub ip: EraExecIp,
}

pub struct EraVirtualMachineStackTrace {
    pub frames: Vec<EraVirtualMachineStackTraceFrame>,
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
impl From<[[f32; 5]; 5]> for EraColorMatrix {
    fn from(value: [[f32; 5]; 5]) -> Self {
        EraColorMatrix {
            m00: value[0][0],
            m01: value[0][1],
            m02: value[0][2],
            m03: value[0][3],
            m04: value[0][4],
            m10: value[1][0],
            m11: value[1][1],
            m12: value[1][2],
            m13: value[1][3],
            m14: value[1][4],
            m20: value[2][0],
            m21: value[2][1],
            m22: value[2][2],
            m23: value[2][3],
            m24: value[2][4],
            m30: value[3][0],
            m31: value[3][1],
            m32: value[3][2],
            m33: value[3][3],
            m34: value[3][4],
            m40: value[4][0],
            m41: value[4][1],
            m42: value[4][2],
            m43: value[4][3],
            m44: value[4][4],
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
    fn on_wait(&mut self, any_key: bool, is_force: bool);
    fn on_twait(&mut self, duration: i64, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<i64>;
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<String>;
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64>;
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String>;
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64>;
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String>;
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64>;
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String>;
    fn on_reuselastline(&mut self, content: &str);
    fn on_clearline(&mut self, count: i64);
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error>;
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error>;
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error>;
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error>;
    fn on_print_button(
        &mut self,
        content: &str,
        value: &str,
        flags: crate::bytecode::PrintExtendedFlags,
    );
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
    fn on_spritewidth(&mut self, name: &str) -> i64;
    fn on_spriteheight(&mut self, name: &str) -> i64;
    // Filesystem subsystem
    fn on_read_file(&mut self, path: &str) -> anyhow::Result<Vec<u8>>;
    fn on_write_file(&mut self, path: &str, data: Vec<u8>) -> anyhow::Result<()>;
    fn on_list_file(&mut self, path: &str) -> anyhow::Result<Vec<String>>;
    // Others
    fn on_check_font(&mut self, font_name: &str) -> i64;
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64>;
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String>;
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
    // Private
    /// Translates strings to indices.
    fn on_csv_get_num(&mut self, name: &str) -> Option<u32>;
}

impl EraVirtualMachine {
    pub fn new(
        compilation: EraBytecodeCompilation,
        chara_templates: BTreeMap<u32, EraCharaInitTemplate>,
    ) -> Self {
        let mut this = EraVirtualMachine {
            func_names: compilation.func_names,
            funcs: compilation.funcs,
            chunks: compilation.chunks,
            global_vars: compilation.global_vars,
            stack: Vec::new(),
            frames: Vec::new(),
            is_halted: false,
            uniform_gen: SimpleUniformGenerator::new(),
            trap_vars: hashbrown::HashMap::new(),
            chara_templates,
            charas_count: 0,
        };
        this
    }
    pub fn reset_exec_and_ip(&mut self, ip: EraExecIp) {
        // TODO: Verify input
        // NOTE: self.charas_count is not affected (?)
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
    // NOTE: Pass in empty data to revert patches
    pub fn patch_bytecode(&mut self, chunk_idx: usize, offset: usize, data: &[u8]) -> bool {
        todo!()
    }
    // NOTE: Returns a future which requires calling execute() on VM to resolve
    //       (calling poll has no effect).
    // NOTE: You can't have more than one concurrent external eval requests,
    //       or the latter will fail.
    pub fn external_eval_any(
        &mut self,
        expr: &str,
    ) -> impl Future<Output = Result<Option<Value>, ()>> {
        struct Fut {
            // TODO...
        }
        impl Future for Fut {
            type Output = Result<Option<Value>, ()>;
            fn poll(
                self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Self::Output> {
                // TODO: poll checks whether VM has done executing eval, or the eval
                //       has been interrupted, or VM itself dropped.
                todo!()
            }
        }
        Fut {}
    }
    pub fn get_stack_trace(&self) -> EraVirtualMachineStackTrace {
        let frames = self
            .frames
            .iter()
            .map(|x| EraVirtualMachineStackTraceFrame { ip: x.ip })
            .collect();
        EraVirtualMachineStackTrace { frames }
    }
    pub fn get_chunk(&self, index: usize) -> Option<&EraBytecodeChunk> {
        self.chunks.get(index)
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
                    break Ok(EraVirtualMachineContext {
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
                        global_vars: &mut $self.global_vars,
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

        let vresult = ctx.get_var_arrint("RESULT")?;
        let vresults = ctx.get_var_arrstr("RESULTS")?;

        for _ in 0..max_inst_cnt {
            if stop_flag.load(Ordering::Relaxed) {
                break;
            }

            // Verify stack size
            const MAX_FUN_FRAME_SIZE: usize = 1024;
            if ctx.stack.len() - ctx.cur_frame.stack_start > MAX_FUN_FRAME_SIZE {
                bail_opt!(
                    ctx,
                    true,
                    format!("execution of one function ran out of stack space. This possibly indicates broken codegen.")
                );
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
            // TODO: Remove this
            // println!(
            //     "{primary_bytecode:?}, {:?}",
            //     ctx.cur_chunk.source_info_at(ctx.cur_frame.ip.offset)
            // );
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
                Jump | JumpW | JumpWW => {
                    let imm_ip = ctx.cur_frame.ip.offset + 1;
                    let offset: isize = match primary_bytecode {
                        Jump => ctx.chunk_read_u8(imm_ip)? as i8 as _,
                        JumpW => ctx.chunk_read_u16(imm_ip)? as i16 as _,
                        JumpWW => ctx.chunk_read_u32(imm_ip)? as i32 as _,
                        _ => unreachable!(),
                    };
                    ip_offset_delta = offset;
                }
                JumpCond | JumpCondW | JumpCondWW => {
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
                        JumpCondWW => {
                            offset_delta = 4;
                            ctx.chunk_read_u32(imm_ip)? as i32 as _
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
                FunCall => {
                    let args_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)? as _;
                    ip_offset_delta += 1;
                    let [entry] = ctx.pop_stack()?;
                    // NOTE: We ignore the return value if we are looking up the callee
                    //       dynamically (i.e. by string instead of index)
                    let ignore_return_value;
                    let func_info = match entry.val.into_unpacked() {
                        // TODO: Check index overflow
                        FlatValue::Int(x) => {
                            ignore_return_value = false;
                            let Some(x) = self.funcs.get(x.val as usize) else {
                                bail_opt!(ctx, true, "invalid index into function pool");
                            };
                            x
                        }
                        FlatValue::Str(x) => {
                            ignore_return_value = true;
                            let func_info = self
                                .func_names
                                .get(CaselessStr::new(&x.val))
                                .map(|&x| &self.funcs[x]);
                            let Some(x) = func_info else {
                                bail_opt!(ctx, true, format!("function `{}` not found", x.val));
                            };
                            x
                        }
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
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
                TryFunCall | TryFunCallForce => {
                    let is_force = matches!(primary_bytecode, TryFunCallForce);

                    let args_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)? as _;
                    ip_offset_delta += 1;
                    let [entry] = ctx.pop_stack()?;
                    // NOTE: We unconditionally ignore return values
                    let entry = match entry.val.into_unpacked() {
                        // TODO: Check index overflow
                        FlatValue::Int(x) => Either::Left(x),
                        FlatValue::Str(x) => Either::Right(x),
                        _ => bail_opt!(ctx, true, "expected primitive values as operands"),
                    };
                    let func_info = match &entry {
                        // TODO: Check index overflow
                        Either::Left(x) => self.funcs.get(x.val as usize),
                        Either::Right(x) => self
                            .func_names
                            .get(CaselessStr::new(&x.val))
                            .map(|&x| &self.funcs[x]),
                    };
                    if let Some(func_info) = func_info {
                        if func_info.args.len() != args_cnt {
                            bail_opt!(ctx, true, "runtime function argument mismatch");
                        }
                        // TODO: Check whether stack_start exceeds bounds of current frame
                        // Transform args pack into ordinary forms
                        let args_pack = ctx.pop_stack_dyn(args_cnt * 2)?.collect::<Vec<_>>();

                        // Function exists
                        if !is_force {
                            ctx.stack.push(Value::new_int(1).into());
                        }

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
                        if is_force {
                            let msg = match entry {
                                Either::Left(x) => format!("function `{}` not found", x.val),
                                Either::Right(x) => format!("function `{}` not found", x.val),
                            };
                            bail_opt!(ctx, true, msg);
                        } else {
                            ctx.stack.push(Value::new_int(0).into());
                        }
                    }
                }
                // FunExists => {
                //     // TODO...
                // }
                RestartExecAtFun => {
                    pop_stack!(ctx, entry:i);
                    let Some(entry_info) = self.funcs.get(entry.val as usize) else {
                        bail_opt!(ctx, true, format!("function `{}` not found", entry.val));
                    };
                    self.reset_exec_and_ip(EraExecIp {
                        chunk: entry_info.chunk_idx as _,
                        offset: entry_info.offset as _,
                    });
                    ip_offset_delta = 0;
                    make_ctx!(self, ctx);
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
                Pop => {
                    [_] = ctx.pop_stack()?;
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
                        FlatValue::Int(x) => ctx.global_vars.get_var_by_idx(x.val as _),
                        FlatValue::Str(x) => ctx.global_vars.get_var(&x.val),
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
                        FlatValue::Int(x) => ctx.global_vars.get_var_by_idx_mut(x.val as _),
                        FlatValue::Str(x) => ctx.global_vars.get_var_mut(&x.val),
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
                        .into_iter()
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
                            let x_ptr: *const () = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if arr.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = ctx
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
                            let x_ptr: *const () = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if arr.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = ctx
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
                        .into_iter()
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
                            let dst_ptr: *const () = Rc::as_ptr(&dst) as _;
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
                                let trap_var_info = ctx
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
                            let dst_ptr: *const () = Rc::as_ptr(&dst) as _;
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
                                let trap_var_info = ctx
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
                            let x_ptr: *const () = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if dst.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = ctx
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
                            let x_ptr: *const () = Rc::as_ptr(&x) as _;
                            let mut x = x.borrow_mut();
                            if dst.is_trap {
                                assert_eq!(
                                    x.dims.len(),
                                    1,
                                    "multi-dimensional arrays must not be trapped"
                                );
                                let trap_var_info = ctx
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
                            let dst_ptr: *const () = Rc::as_ptr(&dst) as _;
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
                                let trap_var_info = ctx
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
                            let dst_ptr: *const () = Rc::as_ptr(&dst) as _;
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
                                let trap_var_info = ctx
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
                        // (FlatValue::ArrInt(dst), src) => {
                        //     // TODO: Remove this branch
                        //     dbg!(dst, src);
                        //     bail_opt!(ctx, true, "value type mismatches array element type")
                        // }
                        // (FlatValue::ArrStr(dst), src) => {
                        //     // TODO: Remove this branch
                        //     dbg!(dst, src);
                        //     bail_opt!(ctx, true, "value type mismatches array element type")
                        // }
                        (FlatValue::ArrInt(_) | FlatValue::ArrStr(_), _) => {
                            bail_opt!(ctx, true, "value type mismatches array element type")
                        }
                        _ => bail_opt!(ctx, true, "destination is not an array"),
                    }
                    ctx.stack.push(src);
                }
                BuildArrayIndexFromMD => {
                    let idxs_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let idxs = ctx
                        .pop_stack_dyn(idxs_cnt as _)?
                        .into_iter()
                        .map(|x| match x.val.into_unpacked() {
                            // TODO: Check overflow
                            FlatValue::Int(x) => Some(x.val as u32),
                            _ => None,
                        })
                        .collect::<Option<smallvec::SmallVec<[_; 3]>>>();
                    let Some(idxs) = idxs else {
                        bail_opt!(ctx, true, "expected integers as operands");
                    };
                    let [arr] = ctx.pop_stack()?;
                    let index = match arr.val.clone().into_unpacked() {
                        FlatValue::ArrInt(x) => x.borrow().calc_idx(&idxs),
                        FlatValue::ArrStr(x) => x.borrow().calc_idx(&idxs),
                        _ => bail_opt!(ctx, true, "expected arrays as operands"),
                    };
                    let Some(index) = index else {
                        bail_opt!(ctx, true, "invalid indices into array");
                    };
                    ctx.stack.push(arr);
                    ctx.stack.push(Value::new_int(index as _).into());
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
                BitNot => {
                    let [x] = ctx.pop_stack()?;
                    let x = ctx.unpack_int(x.into())?;
                    ctx.stack.push(Value::new_int(!x.val).into());
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
                GetRandomRange => {
                    let [lower, upper] = ctx.pop_stack()?;
                    let lower = ctx.unpack_int(lower.into())?;
                    let upper = ctx.unpack_int(upper.into())?;
                    if lower.val >= upper.val {
                        bail_opt!(ctx, true, "random range is empty");
                    }
                    let range_len = (upper.val - lower.val) as _;
                    let rand_num = self
                        .uniform_gen
                        .gen_range(0..range_len, || ctx.callback.on_get_rand());
                    let result = lower.val.wrapping_add_unsigned(rand_num);
                    ctx.stack.push(Value::new_int(result).into());
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
                SplitString => {
                    // TODO: Make split count as return value of SplitString instead
                    let [input, separator, dest, dest_i, dest_count, dest_count_i] =
                        ctx.pop_stack()?;
                    unpack_str!(ctx, input);
                    unpack_str!(ctx, separator);
                    unpack_int!(ctx, dest_count_i);
                    unpack_arrstr_mut!(ctx, dest);
                    unpack_arrint_idx_mut!(ctx, dest_count, dest_count_i.val);
                    let mut count = 0;
                    for part in input.val.split(&separator.val) {
                        arr_idx_mut!(ctx, dest, count);
                        *dest = Rc::new(StrValue {
                            val: part.to_owned(),
                        });
                        count += 1;
                    }
                    dest_count.val = count;
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
                GetBit | SetBit | ClearBit | InvertBit => {
                    let [val, bit] = ctx.pop_stack()?;
                    let val = ctx.unpack_int(val.into())?;
                    let bit = ctx.unpack_int(bit.into())?;
                    if !(0..64).contains(&bit.val) {
                        bail_opt!(ctx, true, "precondition 0 <= bit < 64 was not satisfied");
                    }
                    let result = match primary_bytecode {
                        GetBit => (val.val >> bit.val) & 1,
                        SetBit => val.val | (1 << bit.val),
                        ClearBit => val.val & !(1 << bit.val),
                        InvertBit => val.val ^ (1 << bit.val),
                        _ => unreachable!(),
                    };
                    ctx.stack.push(Value::new_int(result).into());
                }
                TimesFloat => {
                    let [target, target_idx, factor] = ctx.pop_stack()?;
                    let target = ctx.unpack_arrint(target.into())?;
                    let target_idx = ctx.unpack_int(target_idx.into())?.val;
                    let factor = f64::from_bits(ctx.unpack_int(factor.into())?.val as _);
                    let mut target = target.borrow_mut();
                    let Some(target) = target.flat_get_mut(target_idx as _) else {
                        bail_opt!(ctx, true, "invalid indices into array");
                    };
                    target.val = (target.val as f64 * factor) as _;
                }
                ReplaceString => {
                    let [haystack, needle, replace_with] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_str!(ctx, needle);
                    unpack_str!(ctx, replace_with);
                    let re = match regex::Regex::new(&needle.val) {
                        Ok(re) => re,
                        Err(e) => bail_opt!(ctx, true, format!("failed to compile regex: {e}")),
                    };
                    let result = re
                        .replace_all(&haystack.val, &replace_with.val)
                        .into_owned();
                    ctx.stack.push(Value::new_str(result).into());
                }
                RepeatString => {
                    let [a1, a2] = ctx.pop_stack()?;
                    let (haystack, count) = match (a1.val.into_unpacked(), a2.val.into_unpacked()) {
                        (FlatValue::Int(count), FlatValue::Str(haystack)) => (haystack, count),
                        (FlatValue::Str(haystack), FlatValue::Int(count)) => (haystack, count),
                        _ => bail_opt!(ctx, true, "operands must be (int, str)"),
                    };
                    let result = haystack.val.repeat(count.val.max(0) as _);
                    ctx.stack.push(Value::new_str(result).into());
                }
                SubString | SubStringU => {
                    let [haystack, start_pos, length] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_int!(ctx, start_pos);
                    unpack_int!(ctx, length);
                    let haystack = &haystack.val;
                    let start_pos = start_pos.val.max(0) as usize;
                    let length = if length.val < 0 {
                        usize::MAX
                    } else {
                        length.val as _
                    };
                    let result = if length <= 0 {
                        String::new()
                    } else {
                        let haystack_len = haystack.len();
                        let idx_fn = |(index, _)| index;
                        let mut it = haystack.char_indices();
                        let start_byte_pos = it.nth(start_pos).map_or(haystack_len, &idx_fn);
                        let end_byte_pos = it.nth(length - 1).map_or(haystack_len, &idx_fn);
                        // TODO: SAFETY
                        unsafe {
                            haystack
                                .get_unchecked(start_byte_pos..end_byte_pos)
                                .to_owned()
                        }
                    };
                    ctx.stack.push(Value::new_str(result).into());
                }
                StrFind | StrFindU => {
                    let [haystack, needle, start_pos] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_str!(ctx, needle);
                    unpack_int!(ctx, start_pos);
                    let haystack = &haystack.val;
                    let needle = &needle.val;
                    let start_pos = start_pos.val.max(0) as usize;
                    let haystack_len = haystack.len();
                    let start_byte_pos = haystack
                        .char_indices()
                        .nth(start_pos)
                        .map_or(haystack_len, |(index, _)| index);
                    // TODO: SAFETY
                    let result = unsafe {
                        let haystack = haystack.get_unchecked(start_byte_pos..haystack_len);
                        haystack.find(needle).map_or(-1, |pos| {
                            haystack
                                .char_indices()
                                .position(|x| x.0 == pos)
                                .unwrap_unchecked() as _
                        })
                    };
                    ctx.stack.push(Value::new_int(result).into());
                }
                StrLen | StrLenU => {
                    let [haystack] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    let result = haystack.val.chars().count() as _;
                    ctx.stack.push(Value::new_int(result).into());
                }
                CountSubString => {
                    let [haystack, needle] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_str!(ctx, needle);
                    let result = haystack.val.matches(&needle.val).count() as _;
                    ctx.stack.push(Value::new_int(result).into());
                }
                StrCharAtU => {
                    let [haystack, pos] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_int!(ctx, pos);
                    let haystack = &haystack.val;
                    let pos = pos.val;
                    let result = if pos < 0 {
                        String::new()
                    } else {
                        let pos = pos as _;
                        haystack
                            .chars()
                            .nth(pos)
                            .map_or(String::new(), |x| x.to_string())
                    };
                    ctx.stack.push(Value::new_str(result).into());
                }
                FormatIntToStr => {
                    use crate::util::number::formatting::csharp_format_i64;

                    let [value, format] = ctx.pop_stack()?;
                    let value = ctx.unpack_int(value.into())?;
                    let format = ctx.unpack_str(format.into())?;
                    let result = match csharp_format_i64(value.val, &format.val) {
                        Ok(x) => x,
                        Err(e) => bail_opt!(ctx, true, format!("failed to format integer: {e}")),
                    };
                    ctx.stack.push(Value::new_str(result).into());
                }
                StringIsValidInteger => {
                    let [value] = ctx.pop_stack()?;
                    unpack_str!(ctx, value);
                    let result = value.val.parse::<i64>().is_ok().into();
                    ctx.stack.push(Value::new_int(result).into());
                }
                StringToUpper | StringToLower | StringToHalf | StringToFull => {
                    use full2half::CharacterWidth;

                    let [value] = ctx.pop_stack()?;
                    let value = ctx.unpack_str(value.into())?;
                    let result = match primary_bytecode {
                        StringToUpper => value.val.to_ascii_uppercase(),
                        StringToLower => value.val.to_ascii_lowercase(),
                        StringToHalf => value.val.to_half_width(),
                        StringToFull => value.val.to_full_width(),
                        _ => unreachable!(),
                    };
                    ctx.stack.push(Value::new_str(result).into());
                }
                BuildBarString => {
                    use muldiv::MulDiv;

                    let [value, max_value, length] = ctx.pop_stack()?;
                    unpack_int!(ctx, value);
                    unpack_int!(ctx, max_value);
                    unpack_int!(ctx, length);
                    if length.val < 0 {
                        bail_opt!(ctx, true, "bar string length cannot be negative");
                    }
                    if length.val > 1024 {
                        bail_opt!(ctx, true, "bar string length too long");
                    }
                    let length = length.val as u32;
                    let max_value = max_value.val.max(0) as u32;
                    let value = (value.val.max(0) as u32).min(max_value);
                    let Some(fill_cnt) = value.mul_div_floor(length, max_value) else {
                        unreachable!();
                        // TODO: Strict integer arithmetic check
                        // bail_opt!(ctx, true, "unexpected arithmetic overflow");
                    };
                    let rest_cnt = length - fill_cnt;
                    let mut result = String::with_capacity(length as usize + 2);
                    result.push('[');
                    result += &"*".repeat(fill_cnt as _);
                    result += &".".repeat(rest_cnt as _);
                    result.push(']');
                    ctx.stack.push(Value::new_str(result).into());
                }
                EscapeRegexStr => {
                    let [value] = ctx.pop_stack()?;
                    unpack_str!(ctx, value);
                    let result = regex::escape(&value.val);
                    ctx.stack.push(Value::new_str(result).into());
                }
                EncodeToUnicode => {
                    let [haystack, pos] = ctx.pop_stack()?;
                    unpack_str!(ctx, haystack);
                    unpack_int!(ctx, pos);
                    let Ok(pos) = pos.val.try_into() else {
                        bail_opt!(ctx, true, "invalid index into string");
                    };
                    let Some(result) = haystack.val.chars().nth(pos) else {
                        bail_opt!(ctx, true, "invalid index into string");
                    };
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                UnicodeToStr => {
                    let [value] = ctx.pop_stack()?;
                    unpack_int!(ctx, value);
                    let Some(result) = value.val.try_into().map(char::from_u32).ok().flatten()
                    else {
                        bail_opt!(ctx, true, "value is not a valid Unicode scalar value");
                    };
                    ctx.stack.push(Value::new_str(result.to_string()).into());
                }
                IntToStrWithBase => {
                    let [value, base] = ctx.pop_stack()?;
                    unpack_int!(ctx, value);
                    unpack_int!(ctx, base);
                    let value = value.val;
                    let base = base.val;
                    let result = match base {
                        2 => format!("{value:b}"),
                        8 => format!("{value:o}"),
                        10 => format!("{value}"),
                        16 => format!("{value:x}"),
                        _ => bail_opt!(ctx, true, format!("{} is not a valid base", base)),
                    };
                    ctx.stack.push(Value::new_str(result).into());
                }
                HtmlTagSplit => {
                    let [html, tags, count] = ctx.pop_stack()?;
                    unpack_str!(ctx, html);
                    unpack_arrstr_mut!(ctx, tags);
                    unpack_arrint_idx_mut!(ctx, count, 0);
                    // TODO: Honor trapped variables
                    let mut parts_count: usize = 0;
                    for part in crate::util::html::split_html_tags(&html.val) {
                        let Ok(part) = part else {
                            bail_opt!(ctx, true, "found invalid html tag while parsing");
                        };
                        arr_idx_mut!(ctx, tags, parts_count);
                        *tags = Rc::new(StrValue {
                            val: part.to_owned(),
                        });
                        parts_count += 1;
                    }
                    count.val = parts_count as _;
                }
                HtmlToPlainText => {
                    let [html] = ctx.pop_stack()?;
                    unpack_str!(ctx, html);
                    let result = nanohtml2text::html2text(&html.val);
                    ctx.stack.push(Value::new_str(result).into());
                }
                PowerInt => {
                    let [base, exponent] = ctx.pop_stack()?;
                    unpack_int!(ctx, base);
                    unpack_int!(ctx, exponent);
                    let result = base.val.wrapping_pow(exponent.val as _);
                    ctx.stack.push(Value::new_int(result).into());
                }
                SqrtInt | CbrtInt | LogInt | Log10Int | ExponentInt | AbsInt => {
                    use num_integer::Roots;

                    let [value] = ctx.pop_stack()?;
                    unpack_int!(ctx, value);
                    let v = value.val;
                    let result = match primary_bytecode {
                        SqrtInt => v.sqrt(),
                        CbrtInt => v.cbrt(),
                        // FIXME: Use f128 when https://github.com/rust-lang/rust/issues/116909 lands.
                        LogInt => (v as f64).ln() as _,
                        Log10Int => v.ilog10().into(),
                        ExponentInt => (v as f64).exp() as _,
                        AbsInt => v.wrapping_abs(),
                        _ => unreachable!(),
                    };
                    ctx.stack.push(Value::new_int(result).into());
                }
                GroupMatch => {
                    let args_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let mut it = ctx.pop_stack_dyn(args_cnt as usize + 1)?;
                    let mut count = 0;
                    match it.next().unwrap().val.into_unpacked() {
                        FlatValue::Int(x) => {
                            for y in it {
                                let FlatValue::Int(y) = y.val.into_unpacked() else {
                                    count = -1;
                                    break;
                                };
                                if x.val == y.val {
                                    count += 1;
                                }
                            }
                        }
                        FlatValue::Str(x) => {
                            for y in it {
                                let FlatValue::Str(y) = y.val.into_unpacked() else {
                                    count = -1;
                                    break;
                                };
                                if x.val == y.val {
                                    count += 1;
                                }
                            }
                        }
                        _ => {
                            drop(it);
                            bail_opt!(ctx, true, "value must not be arrays");
                        }
                    }
                    if count < 0 {
                        bail_opt!(ctx, true, "invalid GroupMatch operands");
                    }
                    ctx.stack.push(Value::new_int(count).into());
                }
                ArrayCountMatches | CArrayCountMatches => {
                    let [arr, arr_i, val, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_int!(ctx, arr_i);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let arr_i = arr_i.val as usize;
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    let is_chara_mode = matches!(primary_bytecode, CArrayCountMatches);
                    let (dim_pos, end_idx) = if is_chara_mode {
                        (0, end_idx.min(self.charas_count))
                    } else {
                        (usize::MAX, end_idx)
                    };
                    let result = match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let x = x.borrow();
                            unpack_int!(ctx, val);
                            // NOTE: We strip extra index and re-apply provided index range
                            x.stride_iter(arr_i, dim_pos, start_idx, end_idx)
                                .filter(|x| x.val == val.val)
                                .count()
                        }
                        FlatValue::ArrStr(x) => {
                            let x = x.borrow();
                            unpack_str!(ctx, val);
                            // NOTE: We strip extra index and re-apply provided index range
                            x.stride_iter(arr_i, dim_pos, start_idx, end_idx)
                                .filter(|x| x.val == val.val)
                                .count()
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    };
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                SumArray | SumCArray | MaxArray | MaxCArray | MinArray | MinCArray => {
                    let [arr, arr_i, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_arrint_mut!(ctx, arr);
                    unpack_int!(ctx, arr_i);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let arr_i = arr_i.val as usize;
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    let is_chara_mode =
                        matches!(primary_bytecode, SumCArray | MaxCArray | MinCArray);
                    let (dim_pos, end_idx) = if is_chara_mode {
                        (0, end_idx.min(self.charas_count))
                    } else {
                        (usize::MAX, end_idx)
                    };
                    let it = arr
                        .stride_iter(arr_i, dim_pos, start_idx, end_idx)
                        .map(|x| x.val);
                    let result = match primary_bytecode {
                        SumArray | SumCArray => Some(it.sum()),
                        MaxArray | MaxCArray => it.max(),
                        MinArray | MinCArray => it.min(),
                        _ => unreachable!(),
                    };
                    let Some(result) = result else {
                        bail_opt!(ctx, true, "cannot produce a result for empty array range");
                    };
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                InRangeArray | InRangeCArray => {
                    let [arr, arr_i, lower, upper, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_arrint_mut!(ctx, arr);
                    unpack_int!(ctx, arr_i);
                    unpack_int!(ctx, lower);
                    unpack_int!(ctx, upper);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let arr_i = arr_i.val as usize;
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    let is_chara_mode = matches!(primary_bytecode, InRangeCArray);
                    let (dim_pos, end_idx) = if is_chara_mode {
                        (0, end_idx.min(self.charas_count))
                    } else {
                        (usize::MAX, end_idx)
                    };
                    let it = arr
                        .stride_iter(arr_i, dim_pos, start_idx, end_idx)
                        .map(|x| x.val);
                    let result = it.filter(|x| (lower.val..upper.val).contains(x)).count();
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                ArrayRemove => {
                    let [arr, start_idx, count] = ctx.pop_stack()?;
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, count);
                    let start_idx = start_idx.val.max(0) as usize;
                    let count = if count.val < 0 {
                        usize::MAX
                    } else {
                        count.val as _
                    };
                    match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.saturating_sub(start_idx).min(count);
                            if count > 0 {
                                let orig_len = x.vals.len();
                                x.vals.drain(start_idx..(start_idx + count));
                                x.vals.resize(orig_len, Default::default());
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.saturating_sub(start_idx).min(count);
                            if count > 0 {
                                let orig_len = x.vals.len();
                                x.vals.drain(start_idx..(start_idx + count));
                                x.vals.resize(orig_len, Default::default());
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                ArraySortAsc | ArraySortDesc => {
                    let [arr, start_idx, count] = ctx.pop_stack()?;
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, count);
                    let start_idx = start_idx.val.max(0) as usize;
                    let count = if count.val < 0 {
                        usize::MAX
                    } else {
                        count.val as _
                    };
                    let is_ascending = matches!(primary_bytecode, ArraySortAsc);
                    match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.saturating_sub(start_idx).min(count);
                            if count > 0 {
                                x.vals[start_idx..(start_idx + count)].sort_by(|a, b| {
                                    if is_ascending {
                                        a.val.cmp(&b.val)
                                    } else {
                                        b.val.cmp(&a.val)
                                    }
                                });
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.saturating_sub(start_idx).min(count);
                            if count > 0 {
                                x.vals[start_idx..(start_idx + count)].sort_by(|a, b| {
                                    if is_ascending {
                                        a.val.cmp(&b.val)
                                    } else {
                                        b.val.cmp(&a.val)
                                    }
                                });
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                ArrayMSort => {
                    let subs_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let mut it = ctx.pop_stack_dyn(subs_cnt as usize + 1)?;
                    let prim_arr = it.next().unwrap();
                    let mut interrupted = false;
                    let mut err_msg = "";
                    match prim_arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let vals = &mut x.vals[..dim_size];
                            // Generate sort indices
                            let mut sort_idxs = Vec::from_iter(
                                vals.iter()
                                    .take_while(|x| x.val != 0)
                                    .enumerate()
                                    .map(|(i, _)| i),
                            );
                            sort_idxs.sort_by_key(|k| &vals[*k].val);
                            // Sort original array
                            let sorted_vals =
                                Vec::from_iter(sort_idxs.iter().map(|x| vals[*x].clone()));
                            for (d, s) in vals.iter_mut().zip(sorted_vals.into_iter()) {
                                *d = s;
                            }
                            // Sort sub arrays
                            for sub_arr in it {
                                let sub_arr = match sub_arr.val.into_unpacked() {
                                    FlatValue::ArrInt(x) => x,
                                    _ => {
                                        err_msg = "value must be an array of integer";
                                        break;
                                    }
                                };
                                let mut sub_arr = sub_arr.borrow_mut();
                                if sort_idxs.len() > *sub_arr.dims.first().unwrap() as usize {
                                    interrupted = true;
                                    break;
                                }
                                let stride: usize =
                                    sub_arr.dims.iter().map(|x| *x as usize).skip(1).product();
                                let mut sorted_vals = Vec::with_capacity(sort_idxs.len() * stride);
                                for i in sort_idxs.iter().copied() {
                                    let offset = i * stride;
                                    sorted_vals.extend_from_slice(
                                        &sub_arr.vals[offset..(offset + stride)],
                                    );
                                }
                                for (d, s) in sub_arr.vals.iter_mut().zip(sorted_vals.into_iter()) {
                                    *d = s;
                                }
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let vals = &mut x.vals[..dim_size];
                            // Generate sort indices
                            let mut sort_idxs = Vec::from_iter(
                                vals.iter()
                                    .take_while(|x| !x.val.is_empty())
                                    .enumerate()
                                    .map(|(i, _)| i),
                            );
                            sort_idxs.sort_by_key(|k| &vals[*k].val);
                            // Sort original array
                            let sorted_vals =
                                Vec::from_iter(sort_idxs.iter().map(|x| vals[*x].clone()));
                            for (d, s) in vals.iter_mut().zip(sorted_vals.into_iter()) {
                                *d = s;
                            }
                            // Sort sub arrays
                            for sub_arr in it {
                                let sub_arr = match sub_arr.val.into_unpacked() {
                                    FlatValue::ArrStr(x) => x,
                                    _ => {
                                        err_msg = "value must be an array of string";
                                        break;
                                    }
                                };
                                let mut sub_arr = sub_arr.borrow_mut();
                                if sort_idxs.len() > *sub_arr.dims.first().unwrap() as usize {
                                    interrupted = true;
                                    break;
                                }
                                let stride: usize =
                                    sub_arr.dims.iter().map(|x| *x as usize).skip(1).product();
                                let mut sorted_vals = Vec::with_capacity(sort_idxs.len() * stride);
                                for i in sort_idxs.iter().copied() {
                                    let offset = i * stride;
                                    sorted_vals.extend_from_slice(
                                        &sub_arr.vals[offset..(offset + stride)],
                                    );
                                }
                                for (d, s) in sub_arr.vals.iter_mut().zip(sorted_vals.into_iter()) {
                                    *d = s;
                                }
                            }
                        }
                        _ => {
                            drop(it);
                            bail_opt!(ctx, true, "value must be arrays");
                        }
                    }
                    if !err_msg.is_empty() {
                        bail_opt!(ctx, true, err_msg);
                    }
                    // Write to RESULT:0
                    vresult.borrow_mut().vals[0].val = (!interrupted).into();
                }
                ArrayCopy => {
                    let [afrom, ato] = ctx.pop_stack()?;
                    match afrom.val.into_unpacked() {
                        FlatValue::ArrInt(afrom) => {
                            let afrom = afrom.borrow();
                            unpack_arrint_mut!(ctx, ato);
                            if afrom.dims.len() != ato.dims.len() {
                                bail_opt!(ctx, true, "array dimension mismatch");
                            }
                            let upper_dims: Vec<_> = afrom
                                .dims
                                .iter()
                                .zip(ato.dims.iter())
                                .map(|(a, b)| *a.min(b))
                                .collect();
                            let mut idxs = vec![0; upper_dims.len()];
                            loop {
                                *ato.get_mut(&idxs).unwrap() = afrom.get(&idxs).unwrap().clone();
                                // Increment index
                                *idxs.last_mut().unwrap() += 1;
                                for i in (1..idxs.len()).rev() {
                                    if idxs[i] >= upper_dims[i] {
                                        idxs[i] -= upper_dims[i];
                                        idxs[i - 1] += 1;
                                    }
                                }
                                if idxs[0] >= upper_dims[0] {
                                    break;
                                }
                            }
                        }
                        FlatValue::ArrStr(afrom) => {
                            let afrom = afrom.borrow();
                            unpack_arrstr_mut!(ctx, ato);
                            if afrom.dims.len() != ato.dims.len() {
                                bail_opt!(ctx, true, "array dimension mismatch");
                            }
                            let upper_dims: Vec<_> = afrom
                                .dims
                                .iter()
                                .zip(ato.dims.iter())
                                .map(|(a, b)| *a.min(b))
                                .collect();
                            let mut idxs = vec![0; upper_dims.len()];
                            loop {
                                *ato.get_mut(&idxs).unwrap() = afrom.get(&idxs).unwrap().clone();
                                // Increment index
                                *idxs.last_mut().unwrap() += 1;
                                for i in (1..idxs.len()).rev() {
                                    if idxs[i] >= upper_dims[i] {
                                        idxs[i] -= upper_dims[i];
                                        idxs[i - 1] += 1;
                                    }
                                }
                                if idxs[0] >= upper_dims[0] {
                                    break;
                                }
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                ArrayShift => {
                    let [arr, shift_count, value, start_idx, target_count] = ctx.pop_stack()?;
                    unpack_int!(ctx, shift_count);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, target_count);
                    let abs_shift_count = shift_count.val.unsigned_abs() as usize;
                    let start_idx = start_idx.val.max(0) as usize;
                    let target_count = if target_count.val < 0 {
                        usize::MAX
                    } else {
                        target_count.val as _
                    };
                    match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            unpack_int!(ctx, value);
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let target_count = dim_size.saturating_sub(start_idx).min(target_count);
                            let target_vals = &mut x.vals[start_idx..(start_idx + target_count)];
                            if abs_shift_count >= target_count {
                                target_vals.fill(value);
                            } else {
                                if shift_count.val < 0 {
                                    target_vals[..abs_shift_count].fill(value);
                                    target_vals.rotate_left(abs_shift_count);
                                } else {
                                    target_vals.rotate_right(abs_shift_count);
                                    target_vals[..abs_shift_count].fill(value);
                                }
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            unpack_str!(ctx, value);
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let target_count = dim_size.saturating_sub(start_idx).min(target_count);
                            let target_vals = &mut x.vals[start_idx..(start_idx + target_count)];
                            if abs_shift_count >= target_count {
                                target_vals.fill(value);
                            } else {
                                if shift_count.val < 0 {
                                    target_vals[..abs_shift_count].fill(value);
                                    target_vals.rotate_left(abs_shift_count);
                                } else {
                                    target_vals.rotate_right(abs_shift_count);
                                    target_vals[..abs_shift_count].fill(value);
                                }
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                Print | PrintLine | PrintExtended => {
                    let [value] = ctx.pop_stack()?;
                    let flags = match primary_bytecode {
                        Print => PrintExtendedFlags::new(),
                        PrintLine => PrintExtendedFlags::new().with_is_line(true),
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
                HtmlPrint => {
                    let [html] = ctx.pop_stack()?;
                    unpack_str!(ctx, html);
                    ctx.callback.on_html_print(&html.val);
                }
                PrintButton => {
                    let flags = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    let [content, value] = ctx.pop_stack()?;
                    unpack_str!(ctx, content);
                    unpack_str!(ctx, value);
                    ctx.callback
                        .on_print_button(&content.val, &value.val, flags.into());
                }
                Wait => {
                    let [any_key, is_force] = ctx.pop_stack()?;
                    unpack_int!(ctx, any_key);
                    unpack_int!(ctx, is_force);
                    let any_key = any_key.val != 0;
                    let is_force = is_force.val != 0;
                    ctx.callback.on_wait(any_key, is_force);
                }
                TWait => {
                    let [duration, is_force] = ctx.pop_stack()?;
                    unpack_int!(ctx, duration);
                    unpack_int!(ctx, is_force);
                    let is_force = is_force.val != 0;
                    ctx.callback.on_twait(duration.val, is_force);
                }
                PrintImg | PrintImg4 => {
                    todo!("PrintImg")
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
                GDispose => {
                    let [gid] = ctx.pop_stack()?;
                    unpack_int!(ctx, gid);
                    let ret = ctx.callback.on_gdispose(gid.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                GCreated => {
                    let [gid] = ctx.pop_stack()?;
                    unpack_int!(ctx, gid);
                    let ret = ctx.callback.on_gcreated(gid.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                GDrawSprite | GDrawSpriteWithColorMatrix => {
                    let (gid, sprite_name, dest_x, dest_y, dest_width, dest_height, color_matrix);
                    match primary_bytecode {
                        GDrawSprite => {
                            [gid, sprite_name, dest_x, dest_y, dest_width, dest_height] =
                                ctx.pop_stack()?;
                            color_matrix = None;
                        }
                        GDrawSpriteWithColorMatrix => {
                            let acolor_matrix;
                            [
                                gid,
                                sprite_name,
                                dest_x,
                                dest_y,
                                dest_width,
                                dest_height,
                                acolor_matrix,
                            ] = ctx.pop_stack()?;
                            unpack_arrint_mut!(ctx, acolor_matrix);
                            // Parse ColorMatrix
                            // TODO: EmuEra uses VariableTerm to convey indices info,
                            //       support this by adding full indices as a single
                            //       VM stack value.
                            if acolor_matrix.dims.len() != 2 {
                                bail_opt!(
                                    ctx,
                                    true,
                                    "ColorMatrix array has too few or unsupported dimensions"
                                );
                            }
                            let mut clr_mat = [[0.0; 5]; 5];
                            for i in 0..5 {
                                for j in 0..5 {
                                    let idxs = &[i as _, j as _];
                                    let Some(val) = acolor_matrix.get(idxs) else {
                                        bail_opt!(ctx, true, "invalid indices into array");
                                    };
                                    clr_mat[i][j] = val.val as f32 / 256.0;
                                }
                            }
                            color_matrix = Some(EraColorMatrix::from(clr_mat));
                        }
                        _ => unreachable!(),
                    }
                    unpack_int!(ctx, gid);
                    unpack_str!(ctx, sprite_name);
                    unpack_int!(ctx, dest_x);
                    unpack_int!(ctx, dest_y);
                    unpack_int!(ctx, dest_width);
                    unpack_int!(ctx, dest_height);
                    let ret = ctx.callback.on_gdrawsprite(
                        gid.val,
                        &sprite_name.val,
                        dest_x.val,
                        dest_y.val,
                        dest_width.val,
                        dest_height.val,
                        color_matrix.as_ref(),
                    );
                    ctx.stack.push(Value::new_int(ret).into());
                }
                GClear => {
                    let [gid, color] = ctx.pop_stack()?;
                    unpack_int!(ctx, gid);
                    unpack_int!(ctx, color);
                    let ret = ctx.callback.on_gclear(gid.val, color.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteCreate => {
                    let [sprite_name, gid, x, y, width, height] = ctx.pop_stack()?;
                    unpack_str!(ctx, sprite_name);
                    unpack_int!(ctx, gid);
                    unpack_int!(ctx, x);
                    unpack_int!(ctx, y);
                    unpack_int!(ctx, width);
                    unpack_int!(ctx, height);
                    let ret = ctx.callback.on_spritecreate(
                        &sprite_name.val,
                        gid.val,
                        x.val,
                        y.val,
                        width.val,
                        height.val,
                    );
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteDispose => {
                    let [name] = ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    let ret = ctx.callback.on_spritedispose(&name.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteCreated => {
                    let [name] = ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    let ret = ctx.callback.on_spritecreated(&name.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteAnimeCreate => {
                    let [name, width, height] = ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    unpack_int!(ctx, width);
                    unpack_int!(ctx, height);
                    let ret = ctx
                        .callback
                        .on_spriteanimecreate(&name.val, width.val, height.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteAnimeAddFrame => {
                    let [name, gid, x, y, width, height, offset_x, offset_y, delay] =
                        ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    unpack_int!(ctx, gid);
                    unpack_int!(ctx, x);
                    unpack_int!(ctx, y);
                    unpack_int!(ctx, width);
                    unpack_int!(ctx, height);
                    unpack_int!(ctx, offset_x);
                    unpack_int!(ctx, offset_y);
                    unpack_int!(ctx, delay);
                    let ret = ctx.callback.on_spriteanimeaddframe(
                        &name.val,
                        gid.val,
                        x.val,
                        y.val,
                        width.val,
                        height.val,
                        offset_x.val,
                        offset_y.val,
                        delay.val,
                    );
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteWidth => {
                    let [name] = ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    let ret = ctx.callback.on_spritewidth(&name.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SpriteHeight => {
                    let [name] = ctx.pop_stack()?;
                    unpack_str!(ctx, name);
                    let ret = ctx.callback.on_spriteheight(&name.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                CheckFont => {
                    let [font_name] = ctx.pop_stack()?;
                    unpack_str!(ctx, font_name);
                    let ret = ctx.callback.on_check_font(&font_name.val);
                    ctx.stack.push(Value::new_int(ret).into());
                }
                SaveText => {
                    // TODO...
                    bail_opt!(ctx, true, "SaveText not implemented");
                }
                LoadText => {
                    // TODO...
                    bail_opt!(ctx, true, "LoadText not implemented");
                }
                FindElement | FindLastElement => {
                    let [var, value, start_idx, end_idx, complete_match] = ctx.pop_stack()?;
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    unpack_int!(ctx, complete_match);
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    let is_reverse = matches!(primary_bytecode, FindLastElement);
                    let result = match var.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let x = x.borrow();
                            unpack_int!(ctx, value);
                            let (dim_pos, end_idx) = (usize::MAX, end_idx);
                            let mut it =
                                x.stride_iter(0, dim_pos, start_idx, end_idx).map(|x| x.val);
                            let result = if is_reverse {
                                it.rposition(|x| x == value.val)
                            } else {
                                it.position(|x| x == value.val)
                            };
                            result.map(|x| (x + start_idx) as i64).unwrap_or(-1)
                        }
                        FlatValue::ArrStr(x) => {
                            let x = x.borrow();
                            unpack_str!(ctx, value);
                            let Ok(mut re) = regex::Regex::new(&value.val) else {
                                bail_opt!(ctx, true, "invalid regex");
                            };
                            if complete_match.val != 0 {
                                re = regex::Regex::new(&format!("^(?:{})$", value.val)).unwrap()
                            }
                            let (dim_pos, end_idx) = (usize::MAX, end_idx);
                            let mut it = x
                                .stride_iter(0, dim_pos, start_idx, end_idx)
                                .map(|x| &x.val);
                            let result = if is_reverse {
                                it.rposition(|x| re.is_match(x))
                            } else {
                                it.position(|x| re.is_match(x))
                            };
                            result.map(|x| (x + start_idx) as i64).unwrap_or(-1)
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    };
                    ctx.stack.push(Value::new_int(result).into());
                }
                FindChara | FindLastChara => {
                    let [var, var_i, value, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_int!(ctx, var_i);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let var_i = var_i.val as usize;
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    let is_reverse = matches!(primary_bytecode, FindLastChara);
                    let result = match var.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let x = x.borrow();
                            unpack_int!(ctx, value);
                            let (dim_pos, end_idx) = (0, end_idx.min(self.charas_count));
                            let mut it = x
                                .stride_iter(var_i, dim_pos, start_idx, end_idx)
                                .map(|x| x.val);
                            let result = if is_reverse {
                                it.rposition(|x| x == value.val)
                            } else {
                                it.position(|x| x == value.val)
                            };
                            result.map(|x| (x + start_idx) as i64).unwrap_or(-1)
                        }
                        FlatValue::ArrStr(x) => {
                            let x = x.borrow();
                            unpack_str!(ctx, value);
                            let (dim_pos, end_idx) = (0, end_idx.min(self.charas_count));
                            let mut it = x
                                .stride_iter(var_i, dim_pos, start_idx, end_idx)
                                .map(|x| &x.val);
                            let result = if is_reverse {
                                it.rposition(|x| x == &value.val)
                            } else {
                                it.position(|x| x == &value.val)
                            };
                            result.map(|x| (x + start_idx) as i64).unwrap_or(-1)
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    };
                    ctx.stack.push(Value::new_int(result).into());
                }
                VarSet => {
                    let [arr, value, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            unpack_int!(ctx, value);
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.min(end_idx).saturating_sub(start_idx);
                            if count > 0 {
                                x.vals[start_idx..(start_idx + count)].fill(value);
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            unpack_str!(ctx, value);
                            let dim_size = *x.dims.last().unwrap() as usize;
                            let count = dim_size.min(end_idx).saturating_sub(start_idx);
                            if count > 0 {
                                x.vals[start_idx..(start_idx + count)].fill(value);
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                CVarSet => {
                    let [arr, index, value, start_idx, end_idx] = ctx.pop_stack()?;
                    unpack_int!(ctx, index);
                    unpack_int!(ctx, start_idx);
                    unpack_int!(ctx, end_idx);
                    let start_idx = start_idx.val.max(0) as usize;
                    let end_idx = if end_idx.val < 0 {
                        usize::MAX
                    } else {
                        end_idx.val as _
                    };
                    match arr.val.into_unpacked() {
                        FlatValue::ArrInt(x) => {
                            let mut x = x.borrow_mut();
                            unpack_int!(ctx, value);
                            if !(0..x.dims.last().map(|x| *x as i64).unwrap()).contains(&index.val)
                            {
                                bail_opt!(ctx, true, "invalid indices into array");
                            }
                            let index = index.val as usize;
                            let dim_size = self.charas_count;
                            let count = dim_size.min(end_idx).saturating_sub(start_idx);
                            let stride: usize =
                                x.dims.iter().skip(1).map(|x| *x as usize).product();
                            if count > 0 {
                                for i in start_idx..(start_idx + count) {
                                    x.vals[i * stride + index] = value.clone();
                                }
                            }
                        }
                        FlatValue::ArrStr(x) => {
                            let mut x = x.borrow_mut();
                            unpack_str!(ctx, value);
                            if !(0..x.dims.last().map(|x| *x as i64).unwrap()).contains(&index.val)
                            {
                                bail_opt!(ctx, true, "invalid indices into array");
                            }
                            let index = index.val as usize;
                            let dim_size = self.charas_count;
                            let count = dim_size.min(end_idx).saturating_sub(start_idx);
                            let stride: usize =
                                x.dims.iter().skip(1).map(|x| *x as usize).product();
                            if count > 0 {
                                for i in start_idx..(start_idx + count) {
                                    x.vals[i * stride + index] = value.clone();
                                }
                            }
                        }
                        _ => bail_opt!(ctx, true, "value must be arrays"),
                    }
                }
                GetCharaNum => {
                    ctx.stack
                        .push(Value::new_int(self.charas_count as _).into());
                }
                GetHostTimeRaw => {
                    let result = ctx.callback.on_get_host_time();
                    ctx.stack.push(Value::new_int(result as _).into());
                }
                GetHostTime => {
                    use chrono::*;
                    // NOTE: Native time zone info is used
                    let result = ctx.callback.on_get_host_time();
                    let t = DateTime::from_timestamp_millis(result as _).unwrap();
                    let t: DateTime<Local> = t.into();
                    let mut result = t.year() as i64;
                    result = result * 100 + t.month() as i64;
                    result = result * 100 + t.day() as i64;
                    result = result * 100 + t.hour() as i64;
                    result = result * 100 + t.minute() as i64;
                    result = result * 100 + t.second() as i64;
                    result = result * 1000 + (t.nanosecond() as i64) / 1_000_000;
                    ctx.stack.push(Value::new_int(result).into());
                }
                GetHostTimeS => {
                    use chrono::*;
                    // NOTE: Native time zone info is used
                    let result = ctx.callback.on_get_host_time();
                    let t = DateTime::from_timestamp_millis(result as _).unwrap();
                    let t: DateTime<Local> = t.into();
                    let result = t.format("%Y/%m/%d %H:%M:%S");
                    ctx.stack.push(Value::new_str(result.to_string()).into());
                }
                Input => {
                    let sub_bc = EraInputSubBytecodeType::from(
                        ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?,
                    );
                    ip_offset_delta += 1;
                    macro_rules! handle_default {
                        ($ctx:expr, $sub_bc:expr, $default_value:ident:i) => {
                            let $default_value = if $sub_bc.has_default_value() {
                                pop_stack!(ctx, $default_value:i);
                                Some($default_value.val)
                            } else {
                                None
                            };
                        };
                        ($ctx:expr, $sub_bc:expr, $default_value:ident:s) => {
                            let $default_value = if $sub_bc.has_default_value() {
                                pop_stack!(ctx, $default_value:s);
                                Some($default_value)
                            } else {
                                None
                            };
                            let $default_value = $default_value.as_ref().map(|x| x.val.as_str());
                        };
                    }
                    macro_rules! handle_result {
                        ($vresult:ident:i, $result:expr) => {
                            if let Some(r) = $result {
                                $vresult.borrow_mut().vals[0] = IntValue { val: r };
                            }
                        };
                        ($vresult:ident:s, $result:expr) => {
                            if let Some(r) = $result {
                                $vresult.borrow_mut().vals[0] = Rc::new(StrValue { val: r });
                            }
                        };
                    }
                    match (sub_bc.is_string(), sub_bc.is_one(), sub_bc.is_timed()) {
                        (false, false, false) => {
                            pop_stack!(ctx, can_click:b, allow_skip:b);
                            handle_default!(ctx, sub_bc, default_value:i);
                            let r = ctx
                                .callback
                                .on_input_int(default_value, can_click, allow_skip);
                            handle_result!(vresult:i, r);
                        }
                        (true, false, false) => {
                            pop_stack!(ctx, can_click:b, allow_skip:b);
                            handle_default!(ctx, sub_bc, default_value:s);
                            let r = ctx
                                .callback
                                .on_input_str(default_value, can_click, allow_skip);
                            handle_result!(vresults:s, r);
                        }
                        (false, false, true) => {
                            pop_stack!(ctx, time_limit:i, default_value:i, show_prompt:b, expiry_msg:s, can_click:b);
                            let r = ctx.callback.on_tinput_int(
                                time_limit.val,
                                default_value.val,
                                show_prompt,
                                &expiry_msg.val,
                                can_click,
                            );
                            handle_result!(vresult:i, r);
                        }
                        (true, false, true) => {
                            pop_stack!(ctx, time_limit:i, default_value:s, show_prompt:b, expiry_msg:s, can_click:b);
                            let r = ctx.callback.on_tinput_str(
                                time_limit.val,
                                &default_value.val,
                                show_prompt,
                                &expiry_msg.val,
                                can_click,
                            );
                            handle_result!(vresults:s, r);
                        }
                        (false, true, false) => {
                            handle_default!(ctx, sub_bc, default_value:i);
                            let r = ctx.callback.on_oneinput_int(default_value);
                            handle_result!(vresult:i, r);
                        }
                        (true, true, false) => {
                            handle_default!(ctx, sub_bc, default_value:s);
                            let r = ctx.callback.on_oneinput_str(default_value);
                            handle_result!(vresults:s, r);
                        }
                        (false, true, true) => {
                            pop_stack!(ctx, time_limit:i, default_value:i, show_prompt:b, expiry_msg:s, can_click:b);
                            let r = ctx.callback.on_toneinput_int(
                                time_limit.val,
                                default_value.val,
                                show_prompt,
                                &expiry_msg.val,
                                can_click,
                            );
                            handle_result!(vresult:i, r);
                        }
                        (true, true, true) => {
                            pop_stack!(ctx, time_limit:i, default_value:s, show_prompt:b, expiry_msg:s, can_click:b);
                            let r = ctx.callback.on_toneinput_str(
                                time_limit.val,
                                &default_value.val,
                                show_prompt,
                                &expiry_msg.val,
                                can_click,
                            );
                            handle_result!(vresults:s, r);
                        }
                    }
                }
                ReuseLastLine => {
                    pop_stack!(ctx, content:s);
                    ctx.callback.on_reuselastline(&content.val);
                }
                ClearLine => {
                    pop_stack!(ctx, count:i);
                    ctx.callback.on_clearline(count.val);
                }
                CsvGetProp2 => {
                    use EraCsvGetProp2SubBytecodeType::*;
                    let Some(sub_bc) = EraCsvGetProp2SubBytecodeType::try_from_i(
                        ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?,
                    ) else {
                        bail_opt!(
                            ctx,
                            true,
                            "invalid EraCsvGetProp2SubBytecodeType (memory corrupted?)"
                        );
                    };
                    ip_offset_delta += 1;
                    pop_stack!(ctx, chara_no:i, index:i);
                    let Some(chara_template) = self.chara_templates.get(&(chara_no.val as _))
                    else {
                        bail_opt!(ctx, true, "no such character template");
                    };
                    let index = index.val as u32;
                    match sub_bc {
                        CsvName | CsvCallName | CsvNickName | CsvMasterName | CsvCStr => {
                            // Return string
                            let r = match sub_bc {
                                CsvName => Some(&chara_template.name),
                                CsvCallName => Some(&chara_template.callname),
                                CsvNickName => Some(&chara_template.nickname),
                                CsvMasterName => Some(&chara_template.mastername),
                                CsvCStr => chara_template.cstr.get(&index),
                                _ => unreachable!(),
                            };
                            let r = r.map(|x| x.to_owned()).unwrap_or_default();
                            ctx.stack.push(Value::new_str(r).into());
                        }
                        _ => {
                            // Return integer
                            let r = match sub_bc {
                                CsvBase => chara_template.maxbase.get(&index),
                                CsvAbl => chara_template.abl.get(&index),
                                CsvTalent => chara_template.talent.get(&index),
                                CsvMark => chara_template.mark.get(&index),
                                CsvExp => chara_template.exp.get(&index),
                                CsvRelation => chara_template.relation.get(&index),
                                CsvJuel => chara_template.juel.get(&index),
                                CsvEquip => chara_template.equip.get(&index),
                                CsvCFlag => chara_template.cflag.get(&index),
                                _ => unreachable!(),
                            };
                            let r = r.map(|x| x.to_owned()).unwrap_or_default();
                            ctx.stack.push(Value::new_int(r).into());
                        }
                    }
                }
                CsvGetNum => {
                    let [name] = ctx.pop_stack()?;
                    let name = ctx.unpack_str(name.into())?;
                    let result = ctx
                        .callback
                        .on_csv_get_num(&name.val)
                        .map(|x| x as _)
                        .unwrap_or(-1);
                    ctx.stack.push(Value::new_int(result).into());
                }
                CharaCsvExists => {
                    pop_stack!(ctx, chara_no:i);
                    let result = self.chara_templates.contains_key(&(chara_no.val as _));
                    ctx.stack.push(Value::new_int(result.into()).into());
                }
                GetPalamLv | GetExpLv => {
                    let [value, max_lv] = ctx.pop_stack()?;
                    let value = ctx.unpack_int(value.into())?;
                    let max_lv = ctx.unpack_int(max_lv.into())?;
                    let target_arr = match primary_bytecode {
                        GetPalamLv => "PALAMLV",
                        GetExpLv => "EXPLV",
                        _ => unreachable!(),
                    };
                    let target_arr = ctx.get_var_arrint(target_arr)?;
                    let target_arr = target_arr.borrow();
                    let mut result = 0;
                    while result < max_lv.val {
                        let Some(limit) = target_arr.vals.get((result + 1) as usize) else {
                            break;
                        };
                        if value.val < limit.val {
                            break;
                        }
                        result += 1;
                    }
                    ctx.stack.push(Value::new_int(result).into());
                }
                AddChara => {
                    pop_stack!(ctx, chara_no:i);
                    let Some(chara_template) = self.chara_templates.get(&(chara_no.val as _))
                    else {
                        bail_opt!(ctx, true, "no such character template");
                    };
                    let chara_idx = self.charas_count;
                    if self.charas_count + 1 >= crate::engine::MAX_CHARA_COUNT as usize {
                        bail_opt!(ctx, true, "character count exceeds limit");
                    }
                    self.charas_count += 1;
                    for chara_var_idx in ctx.global_vars.chara_var_idxs.iter().copied() {
                        let chara_var = &mut ctx.global_vars.vars[chara_var_idx];
                        match chara_var.val.clone().into_unpacked() {
                            FlatValue::ArrInt(x) => {
                                let mut x = x.borrow_mut();
                                let stride: usize =
                                    x.dims.iter().skip(1).map(|x| *x as usize).product();
                                let start_idx = chara_idx * stride;
                                let end_idx = (chara_idx + 1) * stride;
                                match chara_var.name.as_str() {
                                    "NO" => {
                                        x.vals[start_idx] = chara_no.clone();
                                    }
                                    _ => {
                                        let empty_map = Default::default();
                                        let src = match chara_var.name.as_str() {
                                            "MAXBASE" => &chara_template.maxbase,
                                            "MARK" => &chara_template.mark,
                                            "EXP" => &chara_template.exp,
                                            "TALENT" => &chara_template.talent,
                                            "RELATION" => &chara_template.relation,
                                            "CFLAG" => &chara_template.cflag,
                                            "EQUIP" => &chara_template.equip,
                                            "JUEL" => &chara_template.juel,
                                            _ => &empty_map,
                                        };
                                        x.vals[start_idx..end_idx].fill(Default::default());
                                        for (&sk, &sv) in src {
                                            x.vals[start_idx + sk as usize] = IntValue { val: sv };
                                        }
                                    }
                                }
                            }
                            FlatValue::ArrStr(x) => {
                                let mut x = x.borrow_mut();
                                let stride: usize =
                                    x.dims.iter().skip(1).map(|x| *x as usize).product();
                                let start_idx = chara_idx * stride;
                                let end_idx = (chara_idx + 1) * stride;
                                match chara_var.name.as_str() {
                                    "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => {
                                        let src = match chara_var.name.as_str() {
                                            "NAME" => &chara_template.name,
                                            "CALLNAME" => &chara_template.callname,
                                            "NICKNAME" => &chara_template.nickname,
                                            "MASTERNAME" => &chara_template.mastername,
                                            _ => unreachable!(),
                                        };
                                        x.vals[start_idx] = Rc::new(StrValue {
                                            val: src.to_owned(),
                                        });
                                    }
                                    _ => {
                                        let empty_map = Default::default();
                                        let src = match chara_var.name.as_str() {
                                            "CFLAG" => &chara_template.cstr,
                                            _ => &empty_map,
                                        };
                                        x.vals[start_idx..end_idx].fill(Default::default());
                                        for (&sk, sv) in src {
                                            x.vals[start_idx + sk as usize] =
                                                Rc::new(StrValue { val: sv.to_owned() });
                                        }
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                AddVoidChara => {
                    todo!("AddVoidChara")
                }
                PickUpChara => {
                    let charas_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    // FIXME: Adjust TARGET:0 & MASTER:0 accordingly
                    // NOTE: We need to deduplicate numbers without changing the order
                    let pickup_charas = ctx
                        .pop_stack_dyn(charas_cnt as _)?
                        .map(|x| match x.val.into_unpacked() {
                            FlatValue::Int(x) if (0..self.charas_count as _).contains(&x.val) => {
                                Ok(x.val as usize)
                            }
                            _ => Err(()),
                        })
                        .process_results(|x| x.unique().collect::<Vec<_>>())
                        .ok();
                    let Some(mut pickup_charas) = pickup_charas else {
                        bail_opt!(ctx, true, "invalid character number");
                    };
                    self.charas_count = pickup_charas.len();
                    for orig_idx in 0..pickup_charas.len() {
                        let pickup_idx = pickup_charas[orig_idx];
                        if orig_idx == pickup_idx {
                            continue;
                        }
                        // Swap array content
                        for &chara_var_idx in &ctx.global_vars.chara_var_idxs {
                            let chara_var = &mut ctx.global_vars.vars[chara_var_idx];
                            match chara_var.val.clone().into_unpacked() {
                                FlatValue::ArrInt(x) => {
                                    let mut x = x.borrow_mut();
                                    let stride: usize =
                                        x.dims.iter().skip(1).map(|x| *x as usize).product();
                                    assert!(orig_idx < pickup_idx);
                                    let (left, right) = x.vals.split_at_mut(pickup_idx * stride);
                                    left[(orig_idx * stride)..][..stride]
                                        .swap_with_slice(&mut right[..stride]);
                                }
                                FlatValue::ArrStr(x) => {
                                    let mut x = x.borrow_mut();
                                    let stride: usize =
                                        x.dims.iter().skip(1).map(|x| *x as usize).product();
                                    assert!(orig_idx < pickup_idx);
                                    let (left, right) = x.vals.split_at_mut(pickup_idx * stride);
                                    left[(orig_idx * stride)..][..stride]
                                        .swap_with_slice(&mut right[..stride]);
                                }
                                _ => unreachable!(),
                            }
                        }
                        // Update indices as the consequence of swapping
                        if let Some(idx) = pickup_charas.iter().position(|&x| x == orig_idx) {
                            pickup_charas.swap(orig_idx, idx);
                        }
                    }
                }
                DeleteChara => {
                    let charas_cnt = ctx.chunk_read_u8(ctx.cur_frame.ip.offset + 1)?;
                    ip_offset_delta += 1;
                    // NOTE: We need to deduplicate numbers without changing the order
                    let del_charas = ctx
                        .pop_stack_dyn(charas_cnt as _)?
                        .map(|x| match x.val.into_unpacked() {
                            FlatValue::Int(x) if (0..self.charas_count as _).contains(&x.val) => {
                                Some(x.val as usize)
                            }
                            _ => None,
                        })
                        .collect::<Option<hashbrown::HashSet<_>>>();
                    let Some(del_charas) = del_charas else {
                        bail_opt!(ctx, true, "invalid character number");
                    };
                    self.charas_count -= del_charas.len();
                    for &chara_var_idx in &ctx.global_vars.chara_var_idxs {
                        let chara_var = &mut ctx.global_vars.vars[chara_var_idx];
                        match chara_var.val.clone().into_unpacked() {
                            FlatValue::ArrInt(x) => {
                                let mut x = x.borrow_mut();
                                let stride: usize =
                                    x.dims.iter().skip(1).map(|x| *x as usize).product();
                                let mut index = 0;
                                let old_len = x.vals.len();
                                x.vals.retain(|_| {
                                    index += 1;
                                    !del_charas.contains(&((index - 1) / stride))
                                });
                                x.vals.extend(
                                    std::iter::repeat(Default::default())
                                        .take(del_charas.len() * stride),
                                );
                                assert_eq!(old_len, x.vals.len());
                            }
                            FlatValue::ArrStr(x) => {
                                let mut x = x.borrow_mut();
                                let stride: usize =
                                    x.dims.iter().skip(1).map(|x| *x as usize).product();
                                let mut index = 0;
                                let old_len = x.vals.len();
                                x.vals.retain(|_| {
                                    index += 1;
                                    !del_charas.contains(&((index - 1) / stride))
                                });
                                x.vals.extend(
                                    std::iter::repeat(Default::default())
                                        .take(del_charas.len() * stride),
                                );
                                assert_eq!(old_len, x.vals.len());
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                SwapChara => {
                    pop_stack!(ctx, chara1:i, chara2:i);
                    for i in [chara1.val, chara2.val] {
                        if !(0..self.charas_count as i64).contains(&i) {
                            bail_opt!(ctx, true, "invalid character number");
                        }
                    }
                    let chara1 = chara1.val as usize;
                    let chara2 = chara2.val as usize;
                    if chara1 != chara2 {
                        let (chara1, chara2) = (chara1.min(chara2), chara1.max(chara2));
                        for &chara_var_idx in &ctx.global_vars.chara_var_idxs {
                            let chara_var = &mut ctx.global_vars.vars[chara_var_idx];
                            match chara_var.val.clone().into_unpacked() {
                                FlatValue::ArrInt(x) => {
                                    let mut x = x.borrow_mut();
                                    let stride: usize =
                                        x.dims.iter().skip(1).map(|x| *x as usize).product();
                                    let mut it = x.vals.chunks_exact_mut(stride);
                                    it.nth(chara1)
                                        .unwrap()
                                        .swap_with_slice(it.nth(chara2 - chara1 - 1).unwrap());
                                }
                                FlatValue::ArrStr(x) => {
                                    let mut x = x.borrow_mut();
                                    let stride: usize =
                                        x.dims.iter().skip(1).map(|x| *x as usize).product();
                                    let mut it = x.vals.chunks_exact_mut(stride);
                                    it.nth(chara1)
                                        .unwrap()
                                        .swap_with_slice(it.nth(chara2 - chara1 - 1).unwrap());
                                }
                                _ => unreachable!(),
                            }
                        }
                    }
                }
                AddCopyChara => {
                    todo!("AddCopyChara")
                }
                SaveData => {
                    pop_stack!(ctx, save_id:i, save_info:s);
                    // TODO...
                    ctx.report_err(true, "SaveData not yet implemented");
                }
                GetCharaRegNum => {
                    pop_stack!(ctx, chara_no:i);
                    let Some(nos) = ctx.global_vars.get_var("NO") else {
                        bail_opt!(ctx, true, "variable `NO` not properly defined");
                    };
                    let FlatValue::ArrInt(nos) = nos.clone().into_unpacked() else {
                        bail_opt!(ctx, true, "variable `NO` not properly defined");
                    };
                    let nos = nos.borrow();
                    let result = nos
                        .stride_iter(0, 0, 0, self.charas_count)
                        .position(|x| x.val == chara_no.val)
                        .map(|x| x as _)
                        .unwrap_or(-1);
                    ctx.stack.push(Value::new_int(result).into());
                }
                LoadGlobal => {
                    // TODO...
                    ctx.report_err(true, "LoadGlobal not yet implemented");
                    ctx.stack.push(Value::new_int(0).into());
                }
                SaveGlobal => {
                    // TODO...
                    ctx.report_err(true, "SaveGlobal not yet implemented");
                    ctx.stack.push(Value::new_int(0).into());
                }
                // TODO...
                Invalid => bail_opt!(ctx, true, "invalid bytecode"),
                _ => bail_opt!(
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
    fn check_trap_var(
        trap_vars: &hashbrown::HashMap<*const (), usize>,
        value: &Value,
    ) -> Option<usize> {
        let addr: *const () = match value.clone().into_unpacked() {
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

trait EraArrayExtendAccess<'a> {
    type Item: 'a;
    fn stride_iter(
        &'a self,
        idx: usize,
        dim_pos: usize,
        start_idx: usize,
        end_idx: usize,
    ) -> impl Iterator<Item = Self::Item>;
}

impl<'a> EraArrayExtendAccess<'a> for ArrIntValue {
    type Item = &'a IntValue;
    fn stride_iter(
        &'a self,
        idx: usize,
        dim_pos: usize,
        start_idx: usize,
        end_idx: usize,
    ) -> impl Iterator<Item = Self::Item> + DoubleEndedIterator + ExactSizeIterator {
        struct Iter<'a> {
            src: &'a ArrIntValue,
            offset: usize,
            stride: usize,
            count: usize,
        }
        impl<'a> Iterator for Iter<'a> {
            type Item = &'a IntValue;
            fn next(&mut self) -> Option<Self::Item> {
                if self.count == 0 {
                    return None;
                }
                let Some(item) = self.src.flat_get(self.offset) else {
                    unreachable!();
                    // self.count = 0;
                    // return None;
                };
                self.count -= 1;
                self.offset += self.stride;
                Some(item)
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count, Some(self.count))
            }
        }
        impl DoubleEndedIterator for Iter<'_> {
            fn next_back(&mut self) -> Option<Self::Item> {
                if self.count == 0 {
                    return None;
                }
                self.count -= 1;
                let item = self
                    .src
                    .flat_get(self.offset + self.count * self.stride)
                    .unwrap();
                Some(item)
            }
        }
        impl ExactSizeIterator for Iter<'_> {}

        let dim_pos = dim_pos.min(self.dims.len() - 1);
        let dim_size = self.dims.get(dim_pos).copied().unwrap() as usize;
        let stride = self
            .dims
            .iter()
            .skip(dim_pos + 1)
            .map(|&x| x as usize)
            .product();

        let count = end_idx.min(dim_size).saturating_sub(start_idx);
        let offset = if count <= 0 {
            0
        } else {
            let low = idx % stride;
            let factor = dim_size * stride;
            let high = idx / factor * factor;
            low + high + start_idx * stride
        };

        Iter {
            src: self,
            offset,
            stride,
            count,
        }
    }
}

impl<'a> EraArrayExtendAccess<'a> for ArrStrValue {
    type Item = &'a Rc<StrValue>;
    fn stride_iter(
        &'a self,
        idx: usize,
        dim_pos: usize,
        start_idx: usize,
        end_idx: usize,
    ) -> impl Iterator<Item = Self::Item> + DoubleEndedIterator + ExactSizeIterator {
        struct Iter<'a> {
            src: &'a ArrStrValue,
            offset: usize,
            stride: usize,
            count: usize,
        }
        impl<'a> Iterator for Iter<'a> {
            type Item = &'a Rc<StrValue>;
            fn next(&mut self) -> Option<Self::Item> {
                if self.count == 0 {
                    return None;
                }
                let Some(item) = self.src.flat_get(self.offset) else {
                    unreachable!();
                    // self.count = 0;
                    // return None;
                };
                self.count -= 1;
                self.offset += self.stride;
                Some(item)
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count, Some(self.count))
            }
        }
        impl DoubleEndedIterator for Iter<'_> {
            fn next_back(&mut self) -> Option<Self::Item> {
                if self.count == 0 {
                    return None;
                }
                self.count -= 1;
                let item = self
                    .src
                    .flat_get(self.offset + self.count * self.stride)
                    .unwrap();
                Some(item)
            }
        }
        impl ExactSizeIterator for Iter<'_> {}

        let dim_pos = dim_pos.min(self.dims.len() - 1);
        let dim_size = self.dims.get(dim_pos).copied().unwrap() as usize;
        let stride = self
            .dims
            .iter()
            .skip(dim_pos + 1)
            .map(|&x| x as usize)
            .product();

        let count = end_idx.min(dim_size).saturating_sub(start_idx);
        let offset = if count <= 0 {
            0
        } else {
            let low = idx % stride;
            let factor = dim_size * stride;
            let high = idx / factor * factor;
            low + high + start_idx * stride
        };

        Iter {
            src: self,
            offset,
            stride,
            count,
        }
    }
}

fn place_at_indices<T>(original: &mut [T], indices: &mut [usize]) {
    for i in 0..indices.len() {
        while i != indices[i] {
            let new_i = indices[i];
            indices.swap(i, new_i);
            original.swap(i, new_i);
        }
    }
}
