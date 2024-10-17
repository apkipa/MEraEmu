mod bytecode;
mod compiler;
mod csv;
mod engine;
mod lexer;
mod parser;
mod routine;
pub mod types;
mod util;
pub mod v2;
mod vm;

use std::sync::atomic::AtomicBool;

pub use bytecode::PrintExtendedFlags;
use engine::ExecSourceInfo;
pub use engine::{
    EraScriptErrorInfo, MEraEngine, MEraEngineConfig, MEraEngineError, MEraEngineSysCallback,
};
use safer_ffi::{prelude::*, slice, string};
pub use vm::{EraColorMatrix, MEraEngineFileSeekMode};

#[cfg(all(not(miri), not(feature = "dhat-heap")))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

// #[global_allocator]
// static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

// NOTE: Used by safer_ffi
#[cfg(feature = "headers")]
pub fn generate_headers() -> ::std::io::Result<()> {
    let builder = ::safer_ffi::headers::builder();
    if let Some(filename) = ::std::env::args_os().nth(1) {
        builder.to_file(&filename)?.generate()
    } else {
        builder.to_writer(::std::io::stdout()).generate()
    }
}

#[ffi_export]
fn delete_rust_string(s: string::String) {
    drop(s);
}
#[derive_ReprC]
#[repr(C)]
struct FfiOption<T> {
    has_value: bool,
    value: T,
}
impl<T: Default, U> From<Option<T>> for FfiOption<U>
where
    T: Into<U>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => FfiOption {
                has_value: true,
                value: x.into(),
            },
            None => FfiOption {
                has_value: false,
                value: T::default().into(),
            },
        }
    }
}
// impl<'a> From<Option<&'a str>> for FfiOption<string::str_ref<'a>> {
//     fn from(value: Option<&'a str>) -> Self {
//         match value {
//             Some(x) => FfiOption { has_value: true, value: x.into() },
//             None => FfiOption { has_value: false, value: string::str_ref:: }
//         }
//     }
// }
impl<T> From<FfiOption<T>> for Option<T> {
    fn from(value: FfiOption<T>) -> Self {
        if value.has_value {
            Some(value.value)
        } else {
            None
        }
    }
}
impl<T> FfiOption<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Option<U> {
        if self.has_value {
            Some(f(self.value))
        } else {
            None
        }
    }
}

#[derive_ReprC]
#[repr(C)]
struct MEraEngineFfiResult<T> {
    ok: FfiOption<T>,
    err: char_p::Raw,
}
impl<T> From<MEraEngineFfiResult<T>> for anyhow::Result<T> {
    fn from(value: MEraEngineFfiResult<T>) -> Self {
        if value.ok.has_value {
            Ok(value.ok.value)
        } else {
            Err(unsafe { anyhow::anyhow!("{}", value.err.as_ref().to_str()) })
        }
    }
}
impl<T> MEraEngineFfiResult<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> anyhow::Result<U> {
        if self.ok.has_value {
            Ok(f(self.ok.value))
        } else {
            Err(unsafe { anyhow::anyhow!("{}", self.err.as_ref().to_str()) })
        }
    }
}

#[derive_ReprC(dyn)]
trait MEraEngineSysCallbackInterop {
    fn on_compile_error(&mut self, info: &EraScriptErrorInfoInterop<'_>);
    fn on_execute_error(&mut self, info: &EraScriptErrorInfoInterop<'_>);
    fn on_get_rand(&mut self) -> u64;
    fn on_print(&mut self, content: string::str_ref<'_>, flags: u8);
    fn on_html_print(&mut self, content: string::str_ref<'_>);
    fn on_wait(&mut self, any_key: bool, is_force: bool);
    fn on_twait(&mut self, duration: i64, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: FfiOption<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> FfiOption<i64>;
    // FIXME: Current design requires "leaking" string to the engine.
    //        Figure out a better design (either Rust side owns the
    //        string, or reads from C side before returning).
    fn on_input_str(
        &mut self,
        default_value: Option<string::str_ref<'_>>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<char_p::Raw>;
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> FfiOption<i64>;
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: string::str_ref<'_>,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> Option<char_p::Raw>;
    fn on_oneinput_int(&mut self, default_value: FfiOption<i64>) -> FfiOption<i64>;
    fn on_oneinput_str(
        &mut self,
        default_value: Option<string::str_ref<'_>>,
    ) -> Option<char_p::Raw>;
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> FfiOption<i64>;
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: string::str_ref<'_>,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> Option<char_p::Raw>;
    fn on_reuselastline(&mut self, content: string::str_ref<'_>);
    fn on_clearline(&mut self, count: i64);
    fn on_var_get_int(&mut self, name: string::str_ref<'_>, idx: u32) -> MEraEngineFfiResult<i64>;
    fn on_var_get_str(
        &mut self,
        name: string::str_ref<'_>,
        idx: u32,
    ) -> MEraEngineFfiResult<char_p::Raw>;
    fn on_var_set_int(
        &mut self,
        name: string::str_ref<'_>,
        idx: u32,
        val: i64,
    ) -> MEraEngineFfiResult<()>;
    fn on_var_set_str(
        &mut self,
        name: string::str_ref<'_>,
        idx: u32,
        val: string::str_ref<'_>,
    ) -> MEraEngineFfiResult<()>;
    fn on_print_button(
        &mut self,
        content: string::str_ref<'_>,
        value: string::str_ref<'_>,
        flags: u8,
    );
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64;
    fn on_gcreatefromfile(&mut self, gid: i64, path: string::str_ref<'_>) -> i64;
    fn on_gdispose(&mut self, gid: i64) -> i64;
    fn on_gcreated(&mut self, gid: i64) -> i64;
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: string::str_ref<'_>,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&crate::vm::EraColorMatrix>,
    ) -> i64;
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64;
    fn on_spritecreate(
        &mut self,
        name: string::str_ref<'_>,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64;
    fn on_spritedispose(&mut self, name: string::str_ref<'_>) -> i64;
    fn on_spritecreated(&mut self, name: string::str_ref<'_>) -> i64;
    fn on_spriteanimecreate(&mut self, name: string::str_ref<'_>, width: i64, height: i64) -> i64;
    fn on_spriteanimeaddframe(
        &mut self,
        name: string::str_ref<'_>,
        gid: i64,
        x: i64,
        y: i64,
        dimension: Pair<i64, i64>,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64;
    fn on_spritewidth(&mut self, name: string::str_ref<'_>) -> i64;
    fn on_spriteheight(&mut self, name: string::str_ref<'_>) -> i64;
    fn on_open_host_file(
        &mut self,
        path: string::str_ref<'_>,
        can_write: bool,
    ) -> MEraEngineFfiResult<VirtualPtr<dyn MEraEngineHostFileInterop>>;
    fn on_check_host_file_exists(&mut self, path: string::str_ref<'_>)
        -> MEraEngineFfiResult<bool>;
    fn on_delete_host_file(&mut self, path: string::str_ref<'_>) -> MEraEngineFfiResult<()>;
    fn on_list_host_file(
        &mut self,
        path: string::str_ref<'_>,
    ) -> MEraEngineFfiResult<VirtualPtr<dyn MEraEngineHostFileListingInterop>>;
    fn on_check_font(&mut self, font_name: string::str_ref<'_>) -> i64;
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: string::str_ref<'_>) -> MEraEngineFfiResult<i64>;
    fn on_get_config_str(&mut self, name: string::str_ref<'_>) -> MEraEngineFfiResult<char_p::Raw>;
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
}

#[derive_ReprC(dyn)]
trait MEraEngineHostFileInterop {
    fn read(&mut self, buf: slice::Mut<'_, u8>) -> MEraEngineFfiResult<u64>;
    fn write(&mut self, buf: slice::Ref<'_, u8>) -> MEraEngineFfiResult<()>;
    fn flush(&mut self) -> MEraEngineFfiResult<()>;
    fn truncate(&mut self) -> MEraEngineFfiResult<()>;
    fn seek(&mut self, pos: i64, mode: MEraEngineFileSeekMode) -> MEraEngineFfiResult<()>;
    fn tell(&mut self) -> MEraEngineFfiResult<u64>;
}

#[derive_ReprC]
#[repr(C)]
struct MEraEngineHostFileListingEntryInterop<'a> {
    name: Option<char_p::Ref<'a>>,
    is_file: bool,
    is_dir: bool,
}

#[derive_ReprC(dyn)]
trait MEraEngineHostFileListingInterop {
    fn next(&mut self) -> MEraEngineHostFileListingEntryInterop<'_>;
}

#[derive_ReprC]
#[repr(C)]
struct Pair<T1, T2> {
    pub a: T1,
    pub b: T2,
}

#[derive_ReprC]
#[repr(C)]
#[derive(Default, Debug)]
struct EraExecSourceInfoInterop {
    pub line: u32,
    pub column: u32,
}
#[derive_ReprC]
#[repr(C)]
struct EraScriptErrorInfoInterop<'a> {
    pub filename: string::str_ref<'a>,
    pub src_info: EraExecSourceInfoInterop,
    pub is_error: bool,
    pub msg: string::str_ref<'a>,
}
#[derive_ReprC]
#[repr(C)]
struct MEraEngineErrorInterop {
    msg: string::String,
}
#[derive_ReprC]
#[repr(C)]
struct MEraEngineResultInterop<T> {
    ok: FfiOption<T>,
    err: MEraEngineErrorInterop,
}

#[derive_ReprC]
#[repr(C)]
#[derive(Debug)]
struct MEraEngineStackTraceFrameInterop<'a> {
    pub file_name: string::str_ref<'a>,
    pub func_name: string::str_ref<'a>,
    pub ip: engine::EraExecIpInfo,
    pub src_info: EraExecSourceInfoInterop,
}
#[derive_ReprC]
#[repr(C)]
#[derive(Debug)]
struct MEraEngineStackTraceInterop<'a> {
    pub frames: repr_c::Vec<MEraEngineStackTraceFrameInterop<'a>>,
}
impl Default for MEraEngineStackTraceInterop<'_> {
    fn default() -> Self {
        Self {
            frames: Vec::new().into(),
        }
    }
}
impl<'a> From<engine::EngineStackTrace<'a>> for MEraEngineStackTraceInterop<'a> {
    fn from(value: engine::EngineStackTrace<'a>) -> Self {
        Self {
            frames: value
                .frames
                .into_iter()
                .map(|x| MEraEngineStackTraceFrameInterop {
                    file_name: x.file_name.into(),
                    func_name: x.func_name.into(),
                    ip: x.ip,
                    src_info: x.src_info.into(),
                })
                .collect::<Vec<_>>()
                .into(),
        }
    }
}

#[derive_ReprC]
#[repr(opaque)]
struct MEraEngineInterop {
    /// Inner engine object.
    i: MEraEngine<'static>,
}

#[ffi_export]
fn helper_get_string_width(s: char_p::Ref<'_>) -> u64 {
    use unicode_width::UnicodeWidthStr;
    s.to_str().width() as _
}
#[ffi_export]
fn helper_get_wstring_width(s: safer_ffi::ptr::NonNullRef<u16>) -> u64 {
    use unicode_width::UnicodeWidthChar;
    let mut width = 0;
    // SAFETY: Caller must provide null-terminated wchar_t strings
    unsafe {
        let s = s.as_ptr();
        let s = ptr_iter::ConstPtrIter::read_until_default(s);
        let s = char::decode_utf16(s);
        for ch in s.map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER)) {
            width += ch.width().unwrap_or(0) as u64;
        }
    }
    width
}

#[ffi_export]
fn delete_engine_error(e: MEraEngineErrorInterop) {
    drop(e);
}
#[ffi_export]
fn delete_engine_stack_trace(value: MEraEngineStackTraceInterop<'_>) {
    drop(value);
}
#[ffi_export]
fn new_engine() -> repr_c::Box<MEraEngineInterop> {
    let engine = MEraEngineInterop {
        i: MEraEngine::new(),
    };
    Box::new(engine).into()
}
#[ffi_export]
fn delete_engine(engine: repr_c::Box<MEraEngineInterop>) {
    drop(engine);
}
#[ffi_export]
fn engine_install_sys_callback(
    engine: &mut MEraEngineInterop,
    callback: VirtualPtr<dyn MEraEngineSysCallbackInterop>,
) {
    engine.i.install_sys_callback(Box::new(callback));
}
#[ffi_export]
fn engine_load_csv(
    engine: &mut MEraEngineInterop,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
    kind: engine::EraCsvLoadKind,
) -> MEraEngineResultInterop<()> {
    engine.i.load_csv(filename.to_str(), &content, kind).into()
}
#[ffi_export]
fn engine_load_erh(
    engine: &mut MEraEngineInterop,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
) -> MEraEngineResultInterop<u32> {
    engine
        .i
        .load_erh(filename.to_str(), &content)
        .map(|x| x as _)
        .into()
}
#[ffi_export]
fn engine_load_erb(
    engine: &mut MEraEngineInterop,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
) -> MEraEngineResultInterop<u32> {
    engine
        .i
        .load_erb(filename.to_str(), &content)
        .map(|x| x as _)
        .into()
}
#[ffi_export]
fn engine_register_global_var(
    engine: &mut MEraEngineInterop,
    name: char_p::Ref<'_>,
    is_string: bool,
    dimension: u32,
    watch: bool,
) -> MEraEngineResultInterop<()> {
    engine
        .i
        .register_global_var(name.to_str(), is_string, dimension as _, watch)
        .into()
}
#[ffi_export]
fn engine_finialize_load_srcs(engine: &mut MEraEngineInterop) -> MEraEngineResultInterop<()> {
    engine.i.finialize_load_srcs().into()
}
#[ffi_export]
fn engine_do_execution(
    engine: &mut MEraEngineInterop,
    stop_flag: *mut bool,
    max_inst_cnt: u64,
) -> MEraEngineResultInterop<bool> {
    let stop_flag = unsafe { AtomicBool::from_ptr(stop_flag) };
    engine.i.do_execution(stop_flag, max_inst_cnt).into()
}
#[ffi_export]
fn engine_get_is_halted(engine: &mut MEraEngineInterop) -> bool {
    engine.i.get_is_halted()
}
#[ffi_export]
fn engine_reset_exec_to_ip(
    engine: &mut MEraEngineInterop,
    ip: engine::EraExecIpInfo,
) -> MEraEngineResultInterop<()> {
    engine.i.reset_exec_to_ip(ip).into()
}
#[ffi_export]
fn engine_get_func_info(
    engine: &mut MEraEngineInterop,
    name: char_p::Ref<'_>,
) -> MEraEngineResultInterop<engine::EraFuncInfo> {
    engine.i.get_func_info(name.to_str()).into()
}
#[ffi_export]
fn engine_get_stack_trace(
    engine: &mut MEraEngineInterop,
) -> MEraEngineResultInterop<MEraEngineStackTraceInterop<'_>> {
    engine.i.get_stack_trace().map(Into::into).into()
}
#[ffi_export]
fn engine_get_version() -> string::str_ref<'static> {
    MEraEngine::get_version().into()
}

impl<T: Default> From<Result<T, MEraEngineError>> for MEraEngineResultInterop<T> {
    fn from(value: Result<T, MEraEngineError>) -> Self {
        match value {
            Ok(value) => Self {
                ok: FfiOption {
                    has_value: true,
                    value,
                },
                err: MEraEngineErrorInterop {
                    msg: String::new().into(),
                },
            },
            Err(e) => Self {
                ok: FfiOption {
                    has_value: false,
                    value: Default::default(),
                },
                err: MEraEngineErrorInterop { msg: e.msg.into() },
            },
        }
    }
}
impl From<ExecSourceInfo> for EraExecSourceInfoInterop {
    fn from(value: ExecSourceInfo) -> Self {
        Self {
            line: value.line,
            column: value.column,
        }
    }
}
impl<'a> From<EraScriptErrorInfo<'a>> for EraScriptErrorInfoInterop<'a> {
    fn from(value: EraScriptErrorInfo<'a>) -> Self {
        Self {
            filename: value.filename.into(),
            src_info: value.src_info.into(),
            is_error: value.is_error,
            msg: value.msg.into(),
        }
    }
}

impl MEraEngineSysCallback for VirtualPtr<dyn MEraEngineSysCallbackInterop> {
    fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {
        <Self as MEraEngineSysCallbackInterop>::on_compile_error(self, &info.clone().into())
    }
    fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {
        <Self as MEraEngineSysCallbackInterop>::on_execute_error(self, &info.clone().into())
    }
    fn on_get_rand(&mut self) -> u64 {
        <Self as MEraEngineSysCallbackInterop>::on_get_rand(self)
    }
    fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
        <Self as MEraEngineSysCallbackInterop>::on_print(self, content.into(), flags.into())
    }
    fn on_html_print(&mut self, content: &str) {
        <Self as MEraEngineSysCallbackInterop>::on_html_print(self, content.into())
    }
    fn on_wait(&mut self, any_key: bool, is_force: bool) {
        <Self as MEraEngineSysCallbackInterop>::on_wait(self, any_key, is_force)
    }
    fn on_twait(&mut self, duration: i64, is_force: bool) {
        <Self as MEraEngineSysCallbackInterop>::on_twait(self, duration, is_force)
    }
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<i64> {
        <Self as MEraEngineSysCallbackInterop>::on_input_int(
            self,
            default_value.into(),
            can_click,
            allow_skip,
        )
        .into()
    }
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> Option<String> {
        <Self as MEraEngineSysCallbackInterop>::on_input_str(
            self,
            default_value.map(Into::into),
            can_click,
            allow_skip,
        )
        .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64> {
        <Self as MEraEngineSysCallbackInterop>::on_tinput_int(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .into()
    }
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String> {
        <Self as MEraEngineSysCallbackInterop>::on_tinput_str(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64> {
        <Self as MEraEngineSysCallbackInterop>::on_oneinput_int(self, default_value.into()).into()
    }
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String> {
        <Self as MEraEngineSysCallbackInterop>::on_oneinput_str(self, default_value.map(Into::into))
            .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<i64> {
        <Self as MEraEngineSysCallbackInterop>::on_toneinput_int(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .into()
    }
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: &str,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> Option<String> {
        <Self as MEraEngineSysCallbackInterop>::on_toneinput_str(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_reuselastline(&mut self, content: &str) {
        <Self as MEraEngineSysCallbackInterop>::on_reuselastline(self, content.into())
    }
    fn on_clearline(&mut self, count: i64) {
        <Self as MEraEngineSysCallbackInterop>::on_clearline(self, count)
    }
    fn on_print_button(
        &mut self,
        content: &str,
        value: &str,
        flags: crate::bytecode::PrintExtendedFlags,
    ) {
        <Self as MEraEngineSysCallbackInterop>::on_print_button(
            self,
            content.into(),
            value.into(),
            flags.into(),
        )
    }
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
        <Self as MEraEngineSysCallbackInterop>::on_var_get_int(self, name.into(), idx as _).into()
    }
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
        <Self as MEraEngineSysCallbackInterop>::on_var_get_str(self, name.into(), idx as _)
            .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error> {
        <Self as MEraEngineSysCallbackInterop>::on_var_set_int(self, name.into(), idx as _, val)
            .into()
    }
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error> {
        <Self as MEraEngineSysCallbackInterop>::on_var_set_str(
            self,
            name.into(),
            idx as _,
            val.into(),
        )
        .into()
    }
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gcreate(self, gid, width, height)
    }
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gcreatefromfile(self, gid, path.into())
    }
    fn on_gdispose(&mut self, gid: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gdispose(self, gid)
    }
    fn on_gcreated(&mut self, gid: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gcreated(self, gid)
    }
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&crate::vm::EraColorMatrix>,
    ) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gdrawsprite(
            self,
            gid,
            sprite_name.into(),
            dest_x,
            dest_y,
            dest_width,
            dest_height,
            color_matrix,
        )
    }
    fn on_gclear(&mut self, gid: i64, color: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_gclear(self, gid, color)
    }
    fn on_spritecreate(
        &mut self,
        name: &str,
        gid: i64,
        x: i64,
        y: i64,
        width: i64,
        height: i64,
    ) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spritecreate(
            self,
            name.into(),
            gid,
            x,
            y,
            width,
            height,
        )
    }
    fn on_spritedispose(&mut self, name: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spritedispose(self, name.into())
    }
    fn on_spritecreated(&mut self, name: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spritecreated(self, name.into())
    }
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spriteanimecreate(
            self,
            name.into(),
            width,
            height,
        )
    }
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
    ) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spriteanimeaddframe(
            self,
            name.into(),
            gid,
            x,
            y,
            Pair {
                a: width,
                b: height,
            },
            offset_x,
            offset_y,
            delay,
        )
    }
    fn on_spritewidth(&mut self, name: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spritewidth(self, name.into())
    }
    fn on_spriteheight(&mut self, name: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_spriteheight(self, name.into())
    }
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn crate::vm::EraVirtualMachineHostFile>> {
        <Self as MEraEngineSysCallbackInterop>::on_open_host_file(self, path.into(), can_write)
            .map(|x| -> Box<dyn crate::vm::EraVirtualMachineHostFile> { Box::new(x) })
    }
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
        <Self as MEraEngineSysCallbackInterop>::on_check_host_file_exists(self, path.into()).into()
    }
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
        <Self as MEraEngineSysCallbackInterop>::on_delete_host_file(self, path.into()).into()
    }
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
        let file_listing: anyhow::Result<_> =
            <Self as MEraEngineSysCallbackInterop>::on_list_host_file(self, path.into()).into();
        let mut file_listing = file_listing?;
        let mut files = vec![];
        loop {
            let entry = file_listing.next();
            let MEraEngineHostFileListingEntryInterop {
                name: Some(name),
                is_file,
                is_dir,
            } = entry
            else {
                break;
            };
            if !is_file {
                continue;
            }
            files.push(name.to_string());
        }
        Ok(files)
    }
    fn on_check_font(&mut self, font_name: &str) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_check_font(self, font_name.into())
    }
    fn on_get_host_time(&mut self) -> u64 {
        <Self as MEraEngineSysCallbackInterop>::on_get_host_time(self)
    }
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
        <Self as MEraEngineSysCallbackInterop>::on_get_config_int(self, name.into()).into()
    }
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
        <Self as MEraEngineSysCallbackInterop>::on_get_config_str(self, name.into())
            .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_get_key_state(&mut self, key_code: i64) -> i64 {
        <Self as MEraEngineSysCallbackInterop>::on_get_key_state(self, key_code)
    }
}

impl crate::vm::EraVirtualMachineHostFile for VirtualPtr<dyn MEraEngineHostFileInterop> {
    fn read(&mut self, buf: &mut [u8]) -> anyhow::Result<u64> {
        <Self as MEraEngineHostFileInterop>::read(self, buf.into()).into()
    }
    fn write(&mut self, buf: &[u8]) -> anyhow::Result<()> {
        <Self as MEraEngineHostFileInterop>::write(self, buf.into()).into()
    }
    fn flush(&mut self) -> anyhow::Result<()> {
        <Self as MEraEngineHostFileInterop>::flush(self).into()
    }
    fn truncate(&mut self) -> anyhow::Result<()> {
        <Self as MEraEngineHostFileInterop>::truncate(self).into()
    }
    fn seek(&mut self, pos: i64, mode: MEraEngineFileSeekMode) -> anyhow::Result<()> {
        <Self as MEraEngineHostFileInterop>::seek(self, pos, mode).into()
    }
    fn tell(&mut self) -> anyhow::Result<u64> {
        <Self as MEraEngineHostFileInterop>::tell(self).into()
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, sync::atomic::AtomicBool};

    use colored::{Color, Colorize};
    use indoc::indoc;
    use types::EraPrintExtendedFlags;

    use super::*;

    trait EngineExtension {
        fn reg_int(&mut self, name: &str) -> Result<(), MEraEngineError>;
        fn reg_str(&mut self, name: &str) -> Result<(), MEraEngineError>;
    }

    impl EngineExtension for MEraEngine<'_> {
        fn reg_int(&mut self, name: &str) -> Result<(), MEraEngineError> {
            self.register_global_var(name, false, 1, false)
        }
        fn reg_str(&mut self, name: &str) -> Result<(), MEraEngineError> {
            self.register_global_var(name, true, 1, false)
        }
    }

    trait EngineBuilderExtension {
        fn reg_int(&mut self, name: &str) -> Result<(), v2::engine::MEraEngineError>;
        fn reg_str(&mut self, name: &str) -> Result<(), v2::engine::MEraEngineError>;
    }
    impl<T: v2::engine::MEraEngineSysCallback, U: v2::engine::MEraEngineBuilderCallback>
        EngineBuilderExtension for v2::engine::MEraEngineBuilder<T, U>
    {
        fn reg_int(&mut self, name: &str) -> Result<(), v2::engine::MEraEngineError> {
            self.register_variable(name, false, 1, false)
        }
        fn reg_str(&mut self, name: &str) -> Result<(), v2::engine::MEraEngineError> {
            self.register_variable(name, true, 1, false)
        }
    }

    struct MockEngineCallback<'a> {
        errors: &'a RefCell<String>,
        output: String,
        err_cnt: usize,
        warn_cnt: usize,
        last_msg_ignored: bool,
    }
    impl<'a> MockEngineCallback<'a> {
        fn new(errors: &'a RefCell<String>) -> Self {
            Self {
                errors,
                output: String::new(),
                err_cnt: 0,
                warn_cnt: 0,
                last_msg_ignored: false,
            }
        }
    }
    impl MEraEngineSysCallback for &mut MockEngineCallback<'_> {
        fn on_compile_error(&mut self, info: &EraScriptErrorInfo) {
            // TODO: Stop ignoring these warnings & errors
            if info.msg == "non-compliant use of CHARADATA variable"
                || info
                    .msg
                    .contains("implicit conversion from integer to string is disallowed")
            {
                self.last_msg_ignored = true;
                return;
            }
            let is_note = info.msg.starts_with("note: ");
            if self.last_msg_ignored && is_note {
                self.last_msg_ignored = false;
                return;
            }
            self.last_msg_ignored = false;
            let (noun, color) = if info.is_error {
                if !is_note {
                    self.err_cnt += 1;
                }
                ("error", Color::Red)
            } else {
                if !is_note {
                    self.warn_cnt += 1;
                }
                ("warning", Color::BrightYellow)
            };
            *self.errors.borrow_mut() += &format!(
                "{}({},{}): compile {}: {}\n",
                info.filename, info.src_info.line, info.src_info.column, noun, info.msg
            )
            .color(color)
            .to_string();
        }
        fn on_execute_error(&mut self, info: &EraScriptErrorInfo) {
            panic!(
                "{}({},{}): execute {}: {}\nlast exec output: {}",
                info.filename,
                info.src_info.line,
                info.src_info.column,
                if info.is_error { "error" } else { "warning" },
                info.msg,
                self.output
            );
        }
        fn on_get_rand(&mut self) -> u64 {
            42
        }
        fn on_print(&mut self, content: &str, flags: crate::bytecode::PrintExtendedFlags) {
            self.output += content;
        }
        fn on_html_print(&mut self, content: &str) {
            self.output += content;
        }
        fn on_wait(&mut self, any_key: bool, is_force: bool) {}
        fn on_twait(&mut self, duration: i64, is_force: bool) {}
        fn on_input_int(
            &mut self,
            default_value: Option<i64>,
            can_click: bool,
            allow_skip: bool,
        ) -> Option<i64> {
            None
        }
        fn on_input_str(
            &mut self,
            default_value: Option<&str>,
            can_click: bool,
            allow_skip: bool,
        ) -> Option<String> {
            None
        }
        fn on_tinput_int(
            &mut self,
            time_limit: i64,
            default_value: i64,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> Option<i64> {
            None
        }
        fn on_tinput_str(
            &mut self,
            time_limit: i64,
            default_value: &str,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> Option<String> {
            None
        }
        fn on_oneinput_int(&mut self, default_value: Option<i64>) -> Option<i64> {
            None
        }
        fn on_oneinput_str(&mut self, default_value: Option<&str>) -> Option<String> {
            None
        }
        fn on_toneinput_int(
            &mut self,
            time_limit: i64,
            default_value: i64,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> Option<i64> {
            None
        }
        fn on_toneinput_str(
            &mut self,
            time_limit: i64,
            default_value: &str,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> Option<String> {
            None
        }
        fn on_reuselastline(&mut self, content: &str) {}
        fn on_clearline(&mut self, count: i64) {}
        fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
            Ok(0)
        }
        fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
            Ok(name.to_string())
        }
        fn on_var_set_int(
            &mut self,
            name: &str,
            idx: usize,
            val: i64,
        ) -> Result<(), anyhow::Error> {
            Ok(())
        }
        fn on_var_set_str(
            &mut self,
            name: &str,
            idx: usize,
            val: &str,
        ) -> Result<(), anyhow::Error> {
            println!("Setting variable `{name}` to `{val}`...");
            Ok(())
        }
        fn on_print_button(
            &mut self,
            content: &str,
            value: &str,
            flags: crate::bytecode::PrintExtendedFlags,
        ) {
            self.output += content;
        }
        // Graphics
        fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
            0
        }
        fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
            0
        }
        fn on_gdispose(&mut self, gid: i64) -> i64 {
            0
        }
        fn on_gcreated(&mut self, gid: i64) -> i64 {
            0
        }
        fn on_gdrawsprite(
            &mut self,
            gid: i64,
            sprite_name: &str,
            dest_x: i64,
            dest_y: i64,
            dest_width: i64,
            dest_height: i64,
            color_matrix: Option<&crate::vm::EraColorMatrix>,
        ) -> i64 {
            0
        }
        fn on_gclear(&mut self, gid: i64, color: i64) -> i64 {
            0
        }
        fn on_spritecreate(
            &mut self,
            name: &str,
            gid: i64,
            x: i64,
            y: i64,
            width: i64,
            height: i64,
        ) -> i64 {
            0
        }
        fn on_spritedispose(&mut self, name: &str) -> i64 {
            0
        }
        fn on_spritecreated(&mut self, name: &str) -> i64 {
            0
        }
        fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
            0
        }
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
        ) -> i64 {
            0
        }
        fn on_spritewidth(&mut self, name: &str) -> i64 {
            0
        }
        fn on_spriteheight(&mut self, name: &str) -> i64 {
            0
        }
        // Filesystem subsystem
        fn on_open_host_file(
            &mut self,
            path: &str,
            can_write: bool,
        ) -> anyhow::Result<Box<dyn crate::vm::EraVirtualMachineHostFile>> {
            anyhow::bail!("no file");
        }
        fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
            anyhow::bail!("no file");
        }
        fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
            anyhow::bail!("no file");
        }
        fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
            Ok(vec![])
        }
        // Others
        fn on_check_font(&mut self, font_name: &str) -> i64 {
            0
        }
        fn on_get_host_time(&mut self) -> u64 {
            0
        }
        fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
            anyhow::bail!("no config");
        }
        fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
            anyhow::bail!("no config");
        }
        fn on_get_key_state(&mut self, key_code: i64) -> i64 {
            0
        }
    }

    impl v2::engine::MEraEngineSysCallback for &mut MockEngineCallback<'_> {
        fn on_error(&mut self, diag: &types::DiagnosticProvider) {
            use unicode_width::UnicodeWidthStr;

            let mut errors = self.errors.borrow_mut();

            const PRINT_TO_STDOUT: bool = false;

            for entry in diag.get_entries() {
                let (noun, color) = if entry.level == types::DiagnosticLevel::Error {
                    self.err_cnt += 1;
                    ("error", Color::Red)
                } else if entry.level == types::DiagnosticLevel::Warning {
                    self.warn_cnt += 1;
                    ("warning", Color::BrightYellow)
                } else {
                    ("note", Color::BrightCyan)
                };
                let mut resolved = diag.resolve_src_span(&entry.filename, entry.span).unwrap();
                let mut snippet_prefix_bytes = resolved
                    .snippet
                    .char_indices()
                    .nth(resolved.loc.col as _)
                    .map(|(i, _)| i)
                    .unwrap_or(resolved.snippet.len());
                // Convert tabs to 4 spaces
                snippet_prefix_bytes += resolved.snippet[..snippet_prefix_bytes]
                    .chars()
                    .filter(|&c| c == '\t')
                    .count()
                    * 3;
                resolved.snippet = resolved.snippet.replace("\t", "    ");
                {
                    let msg = format!(
                        "{}({},{}): {}: {}\nSnippet: {}\n",
                        entry.filename,
                        resolved.loc.line,
                        resolved.loc.col + 1,
                        noun,
                        entry.message,
                        resolved.snippet,
                    )
                    .color(color);
                    if PRINT_TO_STDOUT {
                        print!("{}", msg);
                    } else {
                        *errors += &msg.to_string();
                    }
                }
                // Print underlines
                let indent = 9;
                let snippet_prefix_width = resolved.snippet[..snippet_prefix_bytes].width();
                let snippet_target_width = resolved.snippet
                    [snippet_prefix_bytes..snippet_prefix_bytes + resolved.len as usize]
                    .width();
                let underline = " ".repeat(indent + snippet_prefix_width)
                    + &"^".repeat(snippet_target_width)
                    + "\n";
                if PRINT_TO_STDOUT {
                    print!("{}", underline.color(color));
                } else {
                    *errors += &underline.color(color).to_string();
                }
            }
        }

        fn on_get_rand(&mut self) -> u64 {
            0
        }

        fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags) {}

        fn on_html_print(&mut self, content: &str) {}

        fn on_wait(&mut self, any_key: bool, is_force: bool) {}

        fn on_twait(&mut self, duration: i64, is_force: bool) {}

        fn on_input_int(
            &mut self,
            default_value: Option<i64>,
            can_click: bool,
            allow_skip: bool,
        ) -> std::ops::ControlFlow<(), Option<i64>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_input_str(
            &mut self,
            default_value: Option<&str>,
            can_click: bool,
            allow_skip: bool,
        ) -> std::ops::ControlFlow<(), Option<String>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_tinput_int(
            &mut self,
            time_limit: i64,
            default_value: i64,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> std::ops::ControlFlow<(), Option<i64>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_tinput_str(
            &mut self,
            time_limit: i64,
            default_value: &str,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> std::ops::ControlFlow<(), Option<String>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_oneinput_int(
            &mut self,
            default_value: Option<i64>,
        ) -> std::ops::ControlFlow<(), Option<i64>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_oneinput_str(
            &mut self,
            default_value: Option<&str>,
        ) -> std::ops::ControlFlow<(), Option<String>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_toneinput_int(
            &mut self,
            time_limit: i64,
            default_value: i64,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> std::ops::ControlFlow<(), Option<i64>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_toneinput_str(
            &mut self,
            time_limit: i64,
            default_value: &str,
            show_prompt: bool,
            expiry_msg: &str,
            can_click: bool,
        ) -> std::ops::ControlFlow<(), Option<String>> {
            std::ops::ControlFlow::Continue(None)
        }

        fn on_reuselastline(&mut self, content: &str) {}

        fn on_clearline(&mut self, count: i64) {}

        fn on_print_button(&mut self, content: &str, value: &str, flags: EraPrintExtendedFlags) {}

        fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
            Ok(0)
        }

        fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
            Ok(String::new())
        }

        fn on_var_set_int(
            &mut self,
            name: &str,
            idx: usize,
            val: i64,
        ) -> Result<(), anyhow::Error> {
            Ok(())
        }

        fn on_var_set_str(
            &mut self,
            name: &str,
            idx: usize,
            val: &str,
        ) -> Result<(), anyhow::Error> {
            Ok(())
        }

        fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
            0
        }

        fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
            0
        }

        fn on_gdispose(&mut self, gid: i64) -> i64 {
            0
        }

        fn on_gcreated(&mut self, gid: i64) -> i64 {
            0
        }

        fn on_gdrawsprite(
            &mut self,
            gid: i64,
            sprite_name: &str,
            dest_x: i64,
            dest_y: i64,
            dest_width: i64,
            dest_height: i64,
            color_matrix: Option<&types::EraColorMatrix>,
        ) -> i64 {
            0
        }

        fn on_gclear(&mut self, gid: i64, color: i64) -> i64 {
            0
        }

        fn on_spritecreate(
            &mut self,
            name: &str,
            gid: i64,
            x: i64,
            y: i64,
            width: i64,
            height: i64,
        ) -> i64 {
            0
        }

        fn on_spritedispose(&mut self, name: &str) -> i64 {
            0
        }

        fn on_spritecreated(&mut self, name: &str) -> i64 {
            0
        }

        fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
            0
        }

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
        ) -> i64 {
            0
        }

        fn on_spritewidth(&mut self, name: &str) -> i64 {
            0
        }

        fn on_spriteheight(&mut self, name: &str) -> i64 {
            0
        }

        fn on_open_host_file(
            &mut self,
            path: &str,
            can_write: bool,
        ) -> anyhow::Result<Box<dyn types::EraCompilerHostFile>> {
            anyhow::bail!("no such file");
        }

        fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
            Ok(false)
        }

        fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
            anyhow::bail!("no such file");
        }

        fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
            Ok(Vec::new())
        }

        fn on_check_font(&mut self, font_name: &str) -> i64 {
            0
        }

        fn on_get_host_time(&mut self) -> u64 {
            0
        }

        fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
            anyhow::bail!("no such config entry");
        }

        fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
            anyhow::bail!("no such config entry");
        }

        fn on_get_key_state(&mut self, key_code: i64) -> i64 {
            0
        }
    }

    const BASIC_ENGINE_SRC: &str = indoc! {r#"
        ; Simple counter pure function
        @DO_COUNT(iUpper = 100)
            #FUNCTION
            #DIM DYNAMIC iUpper
            #DIM result, 1, 1 = 0
            ;[SKIPSTART]
            ;#DIM cnt = 100
            ;wdwd
            ;[SKIPEND]
            #DIM cnt = 100
            ;iUpper:d():1
            cnt '= iUpper
            ;results = oook
            ;locals@DO_COUNT = oook
            ;PRINTFORM [Start with cnt={cnt},result={result}]
            PRINTFORM [IN {iUpper}]
            result:0:0 = 0
            WHILE cnt > 0
                ;PRINTFORM []
                ;result = result + cnt:0--
                result += cnt:0--
                ;SIF cnt < 98
                ;    BREAK
                ;cnt = cnt - 1
                ;cnt = --cnt
                ;--cnt
            WEND
            ;PRINTFORM [Returning {result}]
            PRINTFORM [Ret]
            RETURNF result
        @BAD_GOTO
            ;$LABEL1
            REPEAT 1
                ;GOTO LABEL1
            REND
        @SYSTEM_TITLE()
            ;#DIM REF xre
            #DIM val = 1 + 1 ;*0
            #DIMS world_str, -2 + 3 = ""
            #DIMS tmpstr = "[" + ";" + "&" + "]"

            ;#DIM cnt = 10000000
            #DIM cnt = 100
            #DIM DYNAMIC sum1
            WHILE cnt > 0
                cnt = cnt - 1
            WEND
            ;cnt = 100
            FOR LOCAL, 100, 0, -1
                sum1 += LOCAL
            NEXT
            PRINTV '(, sum1, ')

            PRINTFORM [{0&&0},{0&&3},{2&&0},{2&&3}]
            PRINTFORM [{0||0},{0||3},{2||0},{2||3}]

            ;world_str = \@ 0 ? aaa # bb{4+1}b \@
            ;world_str '= @"~\@ 0 ? aaa # bb{4+1}b \@~"
            ;PRINTV world_str
            world_str '= @"worl{"d"}"

            val:0 = val + 1
            ; Print hello world
            Printform Hello, {val + 1,2,LEFT}the %world_str%!
            IF 1 + 2 - 3;
                PRINTFORM true
            ELSE
                PRINTFORM false
            ENDIF
            PRINTFORM Done
            ;CALL DO_COUNT
            TRYCCALLFORM DO_%"COUN"+"T"%(50)
                PRINTFORM [OK]
            CATCH
                PRINTFORM [FAIL]
            ENDCATCH
            PRINTV DO_COUNT(0 ? 0 # 10), @"~{-DO_COUNT()}~"
            PRINTV WINDOW_TITLE += "!"

            SELECTCASE 42
                CASE 1, is >= 42
                    PRINTFORM [1]
                    ;THROW "??? {4+1}"
                CASE 43 to 41, 42
                    PRINTFORM [2]
                CASEELSE
                    PRINTFORM [else]
            ENDSELECT

            REPEAT 5
                SIF COUNT == 2
                    CONTINUE
                SIF COUNT == 4
                    BREAK
                PRINTFORM {COUNT}
            REND
            PRINTFORM {COUNT}

            PRINTV tmpstr

            QUIT
            PRINTFORM not printed
    "#};

    #[test]
    fn basic_engine() -> anyhow::Result<()> {
        let main_erb = BASIC_ENGINE_SRC;
        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut engine = MEraEngine::new();
        engine.install_sys_callback(Box::new(&mut callback));
        // engine.register_global_var("COUNT", false, 1, false)?;
        engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        _ = engine.load_erb("main.erb", main_erb.as_bytes());
        _ = engine.finialize_load_srcs();
        let stop_flag = AtomicBool::new(false);
        _ = engine.do_execution(&stop_flag, 1024 * 1024);
        assert!(engine.get_is_halted());
        drop(engine);
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}",
                    callback.output
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        assert_eq!(
            &callback.output,
            "(5050)[0,0,0,3][0,3,2,2]Hello, 4 the world!falseDone[IN 50][Ret][OK][IN 10][Ret][IN 100][Ret]55~-5050~WINDOW_TITLE![1]0135[;&]"
        );

        Ok(())
    }

    #[test]
    fn basic_engine_v2() -> anyhow::Result<()> {
        let main_erb = BASIC_ENGINE_SRC;
        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut builder = v2::engine::MEraEngineBuilder::new(&mut callback);
        builder.register_variable("WINDOW_TITLE", true, 1, true)?;
        // builder.finish_load_csv()?;
        // builder.finish_load_erh()?;
        builder.load_erb("main.erb", main_erb.as_bytes())?;
        let engine = builder.build()?;
        // engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        // _ = engine.load_erb("main.erb", main_erb.as_bytes());
        // _ = engine.finialize_load_srcs();
        // let stop_flag = AtomicBool::new(false);
        // _ = engine.do_execution(&stop_flag, 1024 * 1024);
        // assert!(engine.get_is_halted());
        // drop(engine);
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}",
                    callback.output
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        // assert_eq!(
        //     &callback.output,
        //     "(5050)[0,0,0,3][0,3,2,2]Hello, 4 the world!falseDone[IN 50][Ret][OK][IN 10][Ret][IN 100][Ret]55~-5050~WINDOW_TITLE![1]0135[;&]"
        // );

        Ok(())
    }

    #[test]
    fn assembled_game() -> anyhow::Result<()> {
        // TODO: Redact this
        let game_base_dir = r#"D:\MyData\Games\Others\1\eraTW\TW4.881画蛇添足版（04.07更新）\"#;
        // let game_base_dir = r#"D:\MyData\Games\Others\1\eraTW\eratw-sub-modding-888ab0cd\"#;
        // let game_base_dir = r#"D:\MyData\Games\Others\1\eratohoK\"#;

        #[cfg(feature = "dhat-heap")]
        let _profiler = dhat::Profiler::new_heap();

        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut engine = MEraEngine::new();
        engine.install_sys_callback(Box::new(&mut callback));
        engine.register_global_var("WINDOW_TITLE", true, 1, true)?;
        engine.reg_int("@COLOR")?;
        engine.reg_int("@DEFCOLOR")?;
        engine.reg_int("@BGCOLOR")?;
        engine.reg_int("@DEFBGCOLOR")?;
        engine.reg_int("@FOCUSCOLOR")?;
        engine.reg_int("@STYLE")?;
        engine.reg_str("@FONT")?;
        engine.reg_int("@REDRAW")?;
        engine.reg_int("@ALIGN")?;
        engine.reg_int("@TOOLTIP_DELAY")?;
        engine.reg_int("@TOOLTIP_DURATION")?;
        engine.reg_int("@SKIPDISP")?;
        engine.reg_int("@MESSKIP")?;
        engine.reg_int("@ANIMETIMER")?;
        engine.reg_int("@PRINTCPERLINE")?;
        engine.reg_int("@PRINTCLENGTH")?;
        engine.reg_int("@LINEISEMPTY")?;
        // engine.reg_str("DRAWLINESTR_UNIT")?;
        engine.reg_str("DRAWLINESTR")?;
        engine.reg_int("SCREENWIDTH")?;
        engine.reg_int("LINECOUNT")?;
        engine.reg_str("SAVEDATA_TEXT")?;

        let mut start_time = std::time::Instant::now();

        let mut total_cnt = 0usize;
        let mut pass_cnt = 0usize;
        // Load CSV files
        let mut load_csv = |file_path: &std::path::Path, kind| -> anyhow::Result<()> {
            // let Ok(content) = std::fs::read(file_path) else {
            //     // Proceed if we cannot read the file
            //     return Ok(());
            // };
            let content = std::fs::read(file_path)?;
            let content = content
                .strip_prefix("\u{feff}".as_bytes())
                .unwrap_or(&content);
            engine.load_csv(&file_path.to_string_lossy(), content, kind)?;
            Ok(())
        };
        let mut misc_csvs = Vec::new();
        let mut chara_csvs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}CSV")) {
            use crate::engine::EraCsvLoadKind::*;
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            let path = i.path();
            if file_name.eq_ignore_ascii_case(b"_Rename.csv") {
                load_csv(path, _Rename)?;
            } else if file_name.eq_ignore_ascii_case(b"VariableSize.csv") {
                load_csv(path, VariableSize)?;
            } else {
                let file_name = file_name.to_ascii_uppercase();
                if !file_name.ends_with(b".CSV") {
                    continue;
                }
                if file_name.starts_with(b"CHARA") {
                    chara_csvs.push(path.to_owned());
                } else {
                    misc_csvs.push(path.to_owned());
                }
            }
        }
        for csv in misc_csvs {
            use engine::EraCsvLoadKind::*;
            let csv_name = csv
                .file_name()
                .unwrap()
                .as_encoded_bytes()
                .to_ascii_uppercase();
            let kind = match &csv_name[..] {
                b"ABL.CSV" => Abl,
                b"EXP.CSV" => Exp,
                b"TALENT.CSV" => Talent,
                b"PALAM.CSV" => Palam,
                b"TRAIN.CSV" => Train,
                b"MARK.CSV" => Mark,
                b"ITEM.CSV" => Item,
                b"BASE.CSV" => Base,
                b"SOURCE.CSV" => Source,
                b"EX.CSV" => Ex,
                b"STR.CSV" => Str,
                b"EQUIP.CSV" => Equip,
                b"TEQUIP.CSV" => TEquip,
                b"FLAG.CSV" => Flag,
                b"TFLAG.CSV" => TFlag,
                b"CFLAG.CSV" => CFlag,
                b"TCVAR.CSV" => TCVar,
                b"CSTR.CSV" => CStr,
                b"STAIN.CSV" => Stain,
                b"CDFLAG1.CSV" => CDFlag1,
                b"CDFLAG2.CSV" => CDFlag2,
                b"STRNAME.CSV" => StrName,
                b"TSTR.CSV" => TStr,
                b"SAVESTR.CSV" => SaveStr,
                b"GLOBAL.CSV" => Global,
                b"GLOBALS.CSV" => Globals,
                b"GAMEBASE.CSV" => GameBase,
                _ => continue,
            };
            //load_csv(&csv, kind)?;
            _ = load_csv(&csv, kind);
        }
        for csv in chara_csvs {
            if let Err(e) = load_csv(&csv, engine::EraCsvLoadKind::Chara_) {
                panic!("{}", errors.borrow());
                return Err(e);
            }
        }

        let csv_load_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        let mut erhs = Vec::new();
        let mut erbs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}ERB")) {
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            if file_name.ends_with(b".erb") {
                erbs.push(i.path().to_owned());
            } else if file_name.ends_with(b".erh") {
                erhs.push(i.path().to_owned());
            } else {
                // println!("warn: skipping file `{}`", i.path().display());
            }
        }
        let get_phy_mem = || memory_stats::memory_stats().unwrap().physical_mem;
        let mut last_mem_usage = get_phy_mem();
        let mut file_mem_usages = Vec::new();
        // TODO: Separate erh with erb when calling into engine
        for erb_path in erhs.into_iter().chain(erbs.into_iter()) {
            let mut erb = std::fs::read(&erb_path)?;
            // HACK: Fix missing newlines
            if !erb.ends_with(b"\n") {
                erb.push(b'\n');
            }
            let erb = erb.strip_prefix("\u{feff}".as_bytes()).unwrap_or(&erb);
            if engine.load_erb(&erb_path.to_string_lossy(), erb).is_ok() {
                pass_cnt += 1;
            } else {
                // *errors.borrow_mut() +=
                //     &format!("[File `{}` failed to compile]\n", erb_path.display());
                panic!(
                    "{}\n[File `{}` failed to compile]\n",
                    errors.borrow(),
                    erb_path.display()
                );
            }
            total_cnt += 1;
            let cur_mem_usage = get_phy_mem();
            file_mem_usages.push((
                erb_path.to_string_lossy().into_owned(),
                cur_mem_usage.saturating_sub(last_mem_usage),
            ));
            last_mem_usage = cur_mem_usage;
        }
        println!("Top 10 files with highest memory consumption:");
        file_mem_usages.sort_by_key(|x| x.1);
        for (path, mem_usage) in file_mem_usages.into_iter().rev().take(10) {
            println!("{path}: {}", size::Size::from_bytes(mem_usage));
        }
        *errors.borrow_mut() += &format!(
            "[FINIALIZE, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let erb_parse_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        _ = engine.finialize_load_srcs();
        *errors.borrow_mut() += &format!(
            "[DONE, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let finialize_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        #[cfg(feature = "dhat-heap")]
        drop(_profiler);

        {
            let mem_usage = engine.get_mem_usage().unwrap();
            *errors.borrow_mut() += &format!(
                "[engine reported mem usage: var = {}, code = {}]\n",
                size::Size::from_bytes(mem_usage.var_size),
                size::Size::from_bytes(mem_usage.code_size)
            );
        }
        {
            let mut errors = errors.borrow_mut();
            *errors += &format!("CSV load time: {:?}\n", csv_load_time);
            *errors += &format!("ERB parse time: {:?}\n", erb_parse_time);
            *errors += &format!("ERB finialize time: {:?}\n", finialize_time);
        }
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                drop(engine);
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}\nCompiled {}/{} files.\n{} warning(s), {} error(s) generated.",
                    callback.output, pass_cnt, total_cnt, callback.warn_cnt, callback.err_cnt
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        let stop_flag = AtomicBool::new(false);
        engine.do_execution(&stop_flag, u64::MAX)?;
        assert!(engine.get_is_halted());
        drop(engine);

        Ok(())
    }

    #[test]
    fn assembled_game_v2() -> anyhow::Result<()> {
        // HACK: Grow stack size
        match stacker::remaining_stack() {
            Some(remaining) if remaining < 4 * 1024 * 1024 => {
                return stacker::grow(8 * 1024 * 1024, assembled_game_v2);
            }
            _ => (),
        }

        #[cfg(feature = "dhat-heap")]
        let _profiler = dhat::Profiler::new_heap();

        // TODO: Redact this
        // Read from environment variable
        let config = v2::engine::MEraEngineConfig {
            no_src_map: std::env::var("ERA_NO_SRC_MAP").is_ok(),
            ..Default::default()
        };
        let game_base_dir = std::env::var("ERA_GAME_BASE_DIR").unwrap_or_else(|_| {
            r#"D:\MyData\Games\Others\1\eraTW\TW4.881画蛇添足版（04.07更新）\"#.to_owned()
            // r#"D:\MyData\Games\Others\1\eraTW\eratw-sub-modding-888ab0cd\"#.to_owned()
            // r#"D:\MyData\Games\Others\1\eratohoK\"#.to_owned()
        });

        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut builder = v2::engine::MEraEngineBuilder::new(&mut callback).with_config(config);
        builder.register_variable("WINDOW_TITLE", true, 1, true)?;
        builder.reg_int("@COLOR")?;
        builder.reg_int("@DEFCOLOR")?;
        builder.reg_int("@BGCOLOR")?;
        builder.reg_int("@DEFBGCOLOR")?;
        builder.reg_int("@FOCUSCOLOR")?;
        builder.reg_int("@STYLE")?;
        builder.reg_str("@FONT")?;
        builder.reg_int("@REDRAW")?;
        builder.reg_int("@ALIGN")?;
        builder.reg_int("@TOOLTIP_DELAY")?;
        builder.reg_int("@TOOLTIP_DURATION")?;
        builder.reg_int("@SKIPDISP")?;
        builder.reg_int("@MESSKIP")?;
        builder.reg_int("@ANIMETIMER")?;
        builder.reg_int("@PRINTCPERLINE")?;
        builder.reg_int("@PRINTCLENGTH")?;
        builder.reg_int("@LINEISEMPTY")?;
        // builder.reg_str("DRAWLINESTR_UNIT")?;
        builder.reg_str("DRAWLINESTR")?;
        builder.reg_int("SCREENWIDTH")?;
        builder.reg_int("LINECOUNT")?;
        builder.reg_str("SAVEDATA_TEXT")?;

        *errors.borrow_mut() += &format!(
            "[START BUILD, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let mut start_time = std::time::Instant::now();

        let mut total_cnt = 0usize;
        let mut pass_cnt = 0usize;
        // Load CSV files
        let mut load_csv = |file_path: &std::path::Path, kind| -> anyhow::Result<()> {
            // let Ok(content) = std::fs::read(file_path) else {
            //     // Proceed if we cannot read the file
            //     return Ok(());
            // };
            let content = std::fs::read(file_path)?;
            let content = content
                .strip_prefix("\u{feff}".as_bytes())
                .unwrap_or(&content);
            builder.load_csv(&file_path.to_string_lossy(), content, kind)?;
            Ok(())
        };
        let mut misc_csvs = Vec::new();
        let mut chara_csvs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}CSV")) {
            use v2::engine::EraCsvLoadKind::*;
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            let path = i.path();
            if file_name.eq_ignore_ascii_case(b"_Rename.csv") {
                load_csv(path, _Rename)?;
            } else if file_name.eq_ignore_ascii_case(b"VariableSize.csv") {
                load_csv(path, VariableSize)?;
            } else {
                let file_name = file_name.to_ascii_uppercase();
                if !file_name.ends_with(b".CSV") {
                    continue;
                }
                if file_name.starts_with(b"CHARA") {
                    chara_csvs.push(path.to_owned());
                } else {
                    misc_csvs.push(path.to_owned());
                }
            }
        }
        for csv in misc_csvs {
            use v2::engine::EraCsvLoadKind::*;
            let csv_name = csv
                .file_name()
                .unwrap()
                .as_encoded_bytes()
                .to_ascii_uppercase();
            let kind = match &csv_name[..] {
                b"ABL.CSV" => Abl,
                b"EXP.CSV" => Exp,
                b"TALENT.CSV" => Talent,
                b"PALAM.CSV" => Palam,
                b"TRAIN.CSV" => Train,
                b"MARK.CSV" => Mark,
                b"ITEM.CSV" => Item,
                b"BASE.CSV" => Base,
                b"SOURCE.CSV" => Source,
                b"EX.CSV" => Ex,
                b"STR.CSV" => Str,
                b"EQUIP.CSV" => Equip,
                b"TEQUIP.CSV" => TEquip,
                b"FLAG.CSV" => Flag,
                b"TFLAG.CSV" => TFlag,
                b"CFLAG.CSV" => CFlag,
                b"TCVAR.CSV" => TCVar,
                b"CSTR.CSV" => CStr,
                b"STAIN.CSV" => Stain,
                b"CDFLAG1.CSV" => CDFlag1,
                b"CDFLAG2.CSV" => CDFlag2,
                b"STRNAME.CSV" => StrName,
                b"TSTR.CSV" => TStr,
                b"SAVESTR.CSV" => SaveStr,
                b"GLOBAL.CSV" => Global,
                b"GLOBALS.CSV" => Globals,
                b"GAMEBASE.CSV" => GameBase,
                _ => continue,
            };
            //load_csv(&csv, kind)?;
            _ = load_csv(&csv, kind);
        }
        for csv in chara_csvs {
            if let Err(e) = load_csv(&csv, v2::engine::EraCsvLoadKind::Chara_) {
                panic!("{}", errors.borrow());
                return Err(e);
            }
        }
        builder.finish_load_csv()?;
        *errors.borrow_mut() += &format!(
            "[CSV LOAD FINISH, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let csv_load_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        let mut erhs = Vec::new();
        let mut erbs = Vec::new();
        for i in walkdir::WalkDir::new(format!("{game_base_dir}ERB")) {
            let i = i?;
            if !i.file_type().is_file() {
                continue;
            }
            let file_name = i.file_name().to_ascii_lowercase();
            let file_name = file_name.as_encoded_bytes();
            if file_name.ends_with(b".erb") {
                erbs.push(i.path().to_owned());
            } else if file_name.ends_with(b".erh") {
                erhs.push(i.path().to_owned());
            } else {
                // println!("warn: skipping file `{}`", i.path().display());
            }
        }
        let get_phy_mem = || memory_stats::memory_stats().unwrap().physical_mem;
        let mut last_mem_usage = get_phy_mem();
        let mut file_mem_usages = Vec::new();
        // TODO: Separate erh with erb when calling into engine
        let mut load_erb_fn = |builder: &mut v2::engine::MEraEngineBuilder<
            &mut MockEngineCallback,
            v2::engine::EmptyCallback,
        >,
                               erb_path: std::path::PathBuf,
                               is_header: bool|
         -> anyhow::Result<()> {
            let mut erb = std::fs::read(&erb_path)?;
            let erb = erb.strip_prefix("\u{feff}".as_bytes()).unwrap_or(&erb);
            let result = if is_header {
                builder.load_erh(&erb_path.to_string_lossy(), erb)
            } else {
                builder.load_erb(&erb_path.to_string_lossy(), erb)
            };
            match result {
                Ok(_) => pass_cnt += 1,
                Err(e) => {
                    // *errors.borrow_mut() +=
                    //     &format!("[File `{}` failed to compile]\n", erb_path.display());
                    panic!(
                        "{}\n[File `{}` failed to compile: {}]\n",
                        errors.borrow(),
                        erb_path.display(),
                        e
                    );
                }
            }
            total_cnt += 1;
            let cur_mem_usage = get_phy_mem();
            file_mem_usages.push((
                erb_path.to_string_lossy().into_owned(),
                cur_mem_usage.saturating_sub(last_mem_usage),
            ));
            last_mem_usage = cur_mem_usage;
            Ok(())
        };

        for erb_path in erhs.into_iter() {
            load_erb_fn(&mut builder, erb_path, true)?;
        }
        builder.finish_load_erh()?;
        *errors.borrow_mut() += &format!(
            "[ERH LOAD FINISH, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let erh_parse_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        for erb_path in erbs.into_iter() {
            load_erb_fn(&mut builder, erb_path, false)?;
        }
        println!("Loaded {}/{} files", pass_cnt, total_cnt);
        println!("Top 10 files with highest memory consumption:");
        file_mem_usages.sort_by_key(|x| x.1);
        for (path, mem_usage) in file_mem_usages.into_iter().rev().take(10) {
            println!("{path}: {}", size::Size::from_bytes(mem_usage));
        }

        let erb_parse_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        *errors.borrow_mut() += &format!(
            "[FINIALIZE, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let engine = builder.build()?;
        *errors.borrow_mut() += &format!(
            "[DONE, used mem = {}]\n",
            size::Size::from_bytes(memory_stats::memory_stats().unwrap().physical_mem)
        );

        let finialize_time = start_time.elapsed();
        start_time = std::time::Instant::now();

        #[cfg(feature = "dhat-heap")]
        drop(_profiler);

        // {
        //     let mem_usage = builder.get_mem_usage().unwrap();
        //     *errors.borrow_mut() += &format!(
        //         "[engine reported mem usage: var = {}, code = {}]\n",
        //         size::Size::from_bytes(mem_usage.var_size),
        //         size::Size::from_bytes(mem_usage.code_size)
        //     );
        // }
        {
            let mut errors = errors.borrow_mut();
            *errors += &format!("CSV load time: {:?}\n", csv_load_time);
            *errors += &format!("ERH parse time: {:?}\n", erh_parse_time);
            *errors += &format!("ERB parse time: {:?}\n", erb_parse_time);
            *errors += &format!("ERB finialize time: {:?}\n", finialize_time);
        }
        {
            let errors = errors.borrow();
            if errors.contains("error:") {
                panic!(
                    "compile output:\n{errors}\nexec output:\n{}\nCompiled {}/{} files.\n{} warning(s), {} error(s) generated.",
                    callback.output, pass_cnt, total_cnt, callback.warn_cnt, callback.err_cnt
                );
            }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        // let stop_flag = AtomicBool::new(false);
        // builder.do_execution(&stop_flag, u64::MAX)?;
        // assert!(builder.get_is_halted());
        // drop(builder);

        Ok(())
    }
}
