pub mod types;
mod util;
pub mod v2;

use std::{ops::ControlFlow, sync::atomic::AtomicBool};

use itertools::Itertools;
use safer_ffi::{prelude::*, slice, string};
use types::*;
use v2::engine::{
    EmptyCallback, EraCsvLoadKind, EraEngineSnapshotKind, MEraEngine, MEraEngineAsyncErbLoader,
    MEraEngineBuilder, MEraEngineConfig, MEraEngineHostFile, MEraEngineSysCallback,
};

#[cfg(all(not(miri), not(feature = "dhat-heap")))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

// NOTE: Used by safer_ffi
#[cfg(feature = "headers")]
pub fn generate_headers() -> ::std::io::Result<()> {
    use ::std::io::Read;

    fn file_time(path: impl AsRef<::std::path::Path>) -> ::std::time::SystemTime {
        ::std::fs::metadata(path).unwrap().modified().unwrap()
    }

    let builder = ::safer_ffi::headers::builder();
    if let Some(filename) = ::std::env::args_os().nth(1) {
        // Check if the file is unchanged
        let previous_file = if let Ok(file) = ::std::fs::File::open(&filename) {
            let mut reader = ::std::io::BufReader::new(file);
            let mut previous = Vec::new();
            reader.read_to_end(&mut previous)?;
            Some(previous)
        } else {
            None
        };
        let mut new_file = Vec::new();
        builder.to_writer(&mut new_file).generate()?;
        if previous_file.as_deref() == Some(&new_file[..]) {
            // File is unchanged, don't touch it to prevent unnecessary rebuilds...
            if file_time(&filename) >= file_time("../MEraEmuCore/src/lib.rs") {
                return Ok(());
            }
            // Unless the headers are older than the source file.
        }
        ::std::fs::write(&filename, new_file)
    } else {
        builder.to_writer(::std::io::stdout()).generate()
    }
}

// NOTE: We use two kinds of prefixes for FFI functions: `rust_` and `mee_` (MEraEmu).

// ----- Start of FFI Exports -----
#[ffi_export]
fn rust_drop_string(s: string::String) {
    drop(s);
}

#[ffi_export]
fn rust_drop_vec_u8(v: repr_c::Vec<u8>) {
    drop(v);
}

#[ffi_export]
fn rust_get_string_width(s: char_p::Ref<'_>) -> u64 {
    use unicode_width::UnicodeWidthStr;
    s.to_str().width() as _
}

#[ffi_export]
fn rust_get_wstring_width(s: safer_ffi::ptr::NonNullRef<u16>) -> u64 {
    use unicode_width::UnicodeWidthChar;
    let mut width = 0;
    // SAFETY: Caller must provide null-terminated wchar_t strings.
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
fn rust_get_wstring_view_width(s: slice::Ref<'_, u16>) -> u64 {
    use unicode_width::UnicodeWidthChar;
    let mut width = 0;
    for ch in char::decode_utf16(s.iter().copied()) {
        let ch = ch.unwrap_or(char::REPLACEMENT_CHARACTER);
        width += ch.width().unwrap_or(0) as u64;
    }
    width
}

#[derive_ReprC]
#[repr(C)]
#[derive(Clone, Debug, Default)]
struct RustFuzzyStringMatchEntry {
    start: usize,
    len: usize,
}

#[derive_ReprC]
#[repr(C)]
#[derive(Clone, Debug)]
struct RustFuzzyStringMatch {
    score: isize,
    matches: safer_ffi::vec::Vec<RustFuzzyStringMatchEntry>,
}

impl Default for RustFuzzyStringMatch {
    fn default() -> Self {
        RustFuzzyStringMatch {
            score: 0,
            matches: Vec::default().into(),
        }
    }
}

#[ffi_export]
fn rust_fuzzy_string_match(
    query: char_p::Ref<'_>,
    target: char_p::Ref<'_>,
) -> RustOption<RustFuzzyStringMatch> {
    let Some(result) = sublime_fuzzy::best_match(query.to_str(), target.to_str()) else {
        return None::<RustFuzzyStringMatch>.into();
    };
    let matches = result
        .continuous_matches()
        .map(|m| RustFuzzyStringMatchEntry {
            start: m.start(),
            len: m.len(),
        })
        .collect_vec();
    Some(RustFuzzyStringMatch {
        score: result.score(),
        matches: matches.into(),
    })
    .into()
}

#[ffi_export]
fn rust_drop_fuzzy_string_match(match_: RustFuzzyStringMatch) {
    drop(match_);
}

#[derive_ReprC]
#[repr(C)]
struct RustOption<T> {
    is_some: bool,
    some: T,
}

impl<T: Default, U> From<Option<T>> for RustOption<U>
where
    T: Into<U>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => RustOption {
                is_some: true,
                some: x.into(),
            },
            None => RustOption {
                is_some: false,
                some: T::default().into(),
            },
        }
    }
}

impl<T> From<RustOption<T>> for Option<T> {
    fn from(value: RustOption<T>) -> Self {
        if value.is_some {
            Some(value.some)
        } else {
            None
        }
    }
}

impl<T> RustOption<T> {
    pub fn into_native(self) -> Option<T> {
        self.into()
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Option<U> {
        if self.is_some {
            Some(f(self.some))
        } else {
            None
        }
    }
}

#[derive_ReprC]
#[repr(C)]
struct RustResult<T, E> {
    is_ok: bool,
    ok: T,
    err: E,
}

impl<T1: Default, U1: Default, T2, U2> From<Result<T1, U1>> for RustResult<T2, U2>
where
    T1: Into<T2>,
    U1: Into<U2>,
{
    fn from(value: Result<T1, U1>) -> Self {
        match value {
            Ok(x) => RustResult {
                is_ok: true,
                ok: x.into(),
                err: U1::default().into(),
            },
            Err(x) => RustResult {
                is_ok: false,
                ok: T1::default().into(),
                err: x.into(),
            },
        }
    }
}

impl<T1, U1, T2, U2> From<RustResult<T1, U1>> for Result<T2, U2>
where
    T1: Into<T2>,
    U1: Into<U2>,
{
    fn from(value: RustResult<T1, U1>) -> Self {
        if value.is_ok {
            Ok(value.ok.into())
        } else {
            Err(value.err.into())
        }
    }
}

impl<T, E> RustResult<T, E> {
    pub fn into_native(self) -> Result<T, E> {
        self.into()
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Result<U, E> {
        if self.is_ok {
            Ok(f(self.ok))
        } else {
            Err(self.err)
        }
    }

    pub fn map_err<U>(self, f: impl FnOnce(E) -> U) -> Result<T, U> {
        if self.is_ok {
            Ok(self.ok)
        } else {
            Err(f(self.err))
        }
    }
}

#[derive_ReprC]
#[repr(C)]
struct RustControlFlow<B, C> {
    is_break: bool,
    break_value: B,
    continue_value: C,
}

impl<B1: Default, C1: Default, B2, C2> From<ControlFlow<B1, C1>> for RustControlFlow<B2, C2>
where
    B1: Into<B2>,
    C1: Into<C2>,
{
    fn from(value: ControlFlow<B1, C1>) -> Self {
        match value {
            ControlFlow::Break(x) => RustControlFlow {
                is_break: true,
                break_value: x.into(),
                continue_value: C1::default().into(),
            },
            ControlFlow::Continue(x) => RustControlFlow {
                is_break: false,
                break_value: B1::default().into(),
                continue_value: x.into(),
            },
        }
    }
}

impl<B1, C1, B2, C2> From<RustControlFlow<B1, C1>> for ControlFlow<B2, C2>
where
    B1: Into<B2>,
    C1: Into<C2>,
{
    fn from(value: RustControlFlow<B1, C1>) -> Self {
        if value.is_break {
            ControlFlow::Break(value.break_value.into())
        } else {
            ControlFlow::Continue(value.continue_value.into())
        }
    }
}

impl<B, C> RustControlFlow<B, C> {
    pub fn into_native(self) -> ControlFlow<B, C> {
        self.into()
    }

    pub fn map_break<T>(self, f: impl FnOnce(B) -> T) -> ControlFlow<T, C> {
        if self.is_break {
            ControlFlow::Break(f(self.break_value))
        } else {
            ControlFlow::Continue(self.continue_value)
        }
    }

    pub fn map_continue<T>(self, f: impl FnOnce(C) -> T) -> ControlFlow<B, T> {
        if self.is_break {
            ControlFlow::Break(self.break_value)
        } else {
            ControlFlow::Continue(f(self.continue_value))
        }
    }
}

// NOTE: Usually returned by the other side of the FFI boundary.
type FfiResult<T> = RustResult<T, char_p::Raw>;
// NOTE: Usually returned by the Rust side of the FFI boundary.
type MeeResult<T> = RustResult<T, string::String>;

impl<T> FfiResult<T> {
    // fn from(value: FfiResult<T>) -> Self {
    //     if value.is_ok {
    //         Ok(value.ok)
    //     } else {
    //         Err(unsafe { anyhow::anyhow!("{}", value.err.as_ref().to_str()) })
    //     }
    // }
    pub fn into_anyhow(self) -> anyhow::Result<T> {
        if self.is_ok {
            Ok(self.ok)
        } else {
            Err(unsafe { anyhow::anyhow!("{}", self.err.as_ref().to_str()) })
        }
    }
}

trait ResultExt {
    type TOk;
    type TErr;
    fn into_mee_result<U>(self) -> MeeResult<U>
    where
        U: From<Self::TOk>,
        Self::TOk: Default;
    // fn into_mee_result_or(self, ok: TOk) -> MeeResult<Self::TOk>;
}

impl<T, E> ResultExt for Result<T, E>
where
    E: std::error::Error,
{
    type TOk = T;
    type TErr = E;
    fn into_mee_result<U>(self) -> MeeResult<U>
    where
        U: From<Self::TOk>,
        Self::TOk: Default,
    {
        self.map_err(|e| e.to_string()).into()
    }
    // fn into_mee_result_or(self, ok: TOk) -> MeeResult<Self::TOk> {
    //     self.map_err(|e| e.to_string()).unwrap_or(ok).into()
    // }
}

trait ErrorExt {
    fn into_mee_result<T: Default>(self) -> MeeResult<T>;
}

impl<E: std::error::Error> ErrorExt for E {
    fn into_mee_result<T: Default>(self) -> MeeResult<T> {
        Err::<T, _>(self.to_string()).into()
    }
}

// FIXME: Current design requires "leaking" string to the engine.
//        Figure out a better design (either Rust side owns the
//        string, or reads from C side before returning).

#[derive_ReprC(dyn)]
trait MEraEngineSysCallbackFfi {
    /// Callback for script errors.
    fn on_error(&mut self, diag: DiagnosticProviderFfi<'_, '_>);
    /// Callback for RAND statements. Note that you should always return a fully
    /// filled random u64; the engine will internally cache entropy to reduce
    /// the total amount of syscalls.
    fn on_get_rand(&mut self) -> u64;
    /// Callback for PRINT family statements.
    fn on_print(&mut self, content: string::str_ref<'_>, flags: u8);
    //fn on_debugprint(&mut self, content: string::str_ref<'_>, flags: u8);
    /// Callback for HTML_PRINT statements.
    fn on_html_print(&mut self, content: string::str_ref<'_>, no_single: i64);
    fn on_wait(&mut self, any_key: bool, is_force: bool);
    fn on_twait(&mut self, duration: i64, is_force: bool);
    fn on_input_int(
        &mut self,
        default_value: RustOption<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> RustControlFlow<(), RustOption<i64>>;
    fn on_input_str(
        &mut self,
        default_value: Option<string::str_ref<'_>>,
        can_click: bool,
        allow_skip: bool,
    ) -> RustControlFlow<(), Option<char_p::Raw>>;
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> RustControlFlow<(), RustOption<i64>>;
    fn on_tinput_str(
        &mut self,
        time_limit: i64,
        default_value: string::str_ref<'_>,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> RustControlFlow<(), Option<char_p::Raw>>;
    fn on_oneinput_int(
        &mut self,
        default_value: RustOption<i64>,
    ) -> RustControlFlow<(), RustOption<i64>>;
    fn on_oneinput_str(
        &mut self,
        default_value: Option<string::str_ref<'_>>,
    ) -> RustControlFlow<(), Option<char_p::Raw>>;
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> RustControlFlow<(), RustOption<i64>>;
    fn on_toneinput_str(
        &mut self,
        time_limit: i64,
        default_value: string::str_ref<'_>,
        show_prompt: bool,
        expiry_msg: string::str_ref<'_>,
        can_click: bool,
    ) -> RustControlFlow<(), Option<char_p::Raw>>;
    fn on_reuselastline(&mut self, content: string::str_ref<'_>);
    fn on_clearline(&mut self, count: i64);
    fn on_print_button(
        &mut self,
        content: string::str_ref<'_>,
        value: string::str_ref<'_>,
        flags: u8,
    );
    /// Callbacks for variable getters & setters. May return a string to report as execution errors.
    fn on_var_get_int(&mut self, name: string::str_ref<'_>, idx: usize) -> FfiResult<i64>;
    fn on_var_get_str(&mut self, name: string::str_ref<'_>, idx: usize) -> FfiResult<char_p::Raw>;
    fn on_var_set_int(&mut self, name: string::str_ref<'_>, idx: usize, val: i64) -> FfiResult<()>;
    fn on_var_set_str(
        &mut self,
        name: string::str_ref<'_>,
        idx: usize,
        val: string::str_ref<'_>,
    ) -> FfiResult<()>;
    // Graphics subsystem
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
        color_matrix: Option<&EraColorMatrix>,
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
        width: i64,
        height: i64,
        offset_x: i64,
        offset_y: i64,
        delay: i64,
    ) -> i64;
    fn on_spritewidth(&mut self, name: string::str_ref<'_>) -> i64;
    fn on_spriteheight(&mut self, name: string::str_ref<'_>) -> i64;
    // Filesystem subsystem
    fn on_open_host_file(
        &mut self,
        path: string::str_ref<'_>,
        can_write: bool,
    ) -> FfiResult<VirtualPtr<dyn MEraEngineHostFileFfi>>;
    fn on_check_host_file_exists(&mut self, path: string::str_ref<'_>) -> FfiResult<bool>;
    fn on_delete_host_file(&mut self, path: string::str_ref<'_>) -> FfiResult<()>;
    fn on_list_host_file(
        &mut self,
        path: string::str_ref<'_>,
    ) -> FfiResult<VirtualPtr<dyn MEraEngineHostFileListingFfi>>;
    // Others
    fn on_check_font(&mut self, font_name: string::str_ref<'_>) -> i64;
    // NOTE: Returns UTC timestamp (in milliseconds).
    fn on_get_host_time(&mut self) -> u64;
    fn on_get_config_int(&mut self, name: string::str_ref<'_>) -> FfiResult<i64>;
    fn on_get_config_str(&mut self, name: string::str_ref<'_>) -> FfiResult<char_p::Raw>;
    // NOTE: Returns { b15 = <key down>, b0 = <key triggered> }. For key codes, refer
    //       to https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes.
    fn on_get_key_state(&mut self, key_code: i64) -> i64;
    fn on_await(&mut self, milliseconds: i64);
}

#[derive_ReprC(dyn)]
trait MEraEngineHostFileFfi {
    fn read(&mut self, buf: slice::Mut<'_, u8>) -> FfiResult<u64>;
    fn write(&mut self, buf: slice::Ref<'_, u8>) -> FfiResult<()>;
    fn flush(&mut self) -> FfiResult<()>;
    fn truncate(&mut self) -> FfiResult<()>;
    fn seek(&mut self, pos: i64, mode: EraCompilerFileSeekMode) -> FfiResult<u64>;
}

#[derive_ReprC]
#[repr(C)]
struct MEraEngineHostFileListingEntryFfi<'a> {
    name: Option<char_p::Ref<'a>>,
    is_file: bool,
    is_dir: bool,
}

#[derive_ReprC(dyn)]
trait MEraEngineHostFileListingFfi {
    fn next(&mut self) -> MEraEngineHostFileListingEntryFfi<'_>;
}

impl MEraEngineHostFile for VirtualPtr<dyn MEraEngineHostFileFfi> {
    fn read(&mut self, buf: &mut [u8]) -> anyhow::Result<u64> {
        MEraEngineHostFileFfi::read(self, buf.into()).into_anyhow()
    }
    fn write(&mut self, buf: &[u8]) -> anyhow::Result<()> {
        MEraEngineHostFileFfi::write(self, buf.into()).into_anyhow()
    }
    fn flush(&mut self) -> anyhow::Result<()> {
        MEraEngineHostFileFfi::flush(self).into_anyhow()
    }
    fn truncate(&mut self) -> anyhow::Result<()> {
        MEraEngineHostFileFfi::truncate(self).into_anyhow()
    }
    fn seek(&mut self, pos: i64, mode: EraCompilerFileSeekMode) -> anyhow::Result<u64> {
        MEraEngineHostFileFfi::seek(self, pos, mode.into()).into_anyhow()
    }
}

impl MEraEngineSysCallback for VirtualPtr<dyn MEraEngineSysCallbackFfi> {
    fn on_error(&mut self, diag: &DiagnosticProvider) {
        let diag = DiagnosticProviderFfi { i: diag };
        MEraEngineSysCallbackFfi::on_error(self, diag);
    }
    fn on_get_rand(&mut self) -> u64 {
        MEraEngineSysCallbackFfi::on_get_rand(self)
    }
    fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags) {
        MEraEngineSysCallbackFfi::on_print(self, content.into(), flags.into());
    }
    //fn on_debugprint(&mut self, content: &str, flags: EraPrintExtendedFlags);
    fn on_html_print(&mut self, content: &str, no_single: i64) {
        MEraEngineSysCallbackFfi::on_html_print(self, content.into(), no_single);
    }
    fn on_wait(&mut self, any_key: bool, is_force: bool) {
        MEraEngineSysCallbackFfi::on_wait(self, any_key, is_force);
    }
    fn on_twait(&mut self, duration: i64, is_force: bool) {
        MEraEngineSysCallbackFfi::on_twait(self, duration, is_force);
    }
    fn on_input_int(
        &mut self,
        default_value: Option<i64>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<i64>> {
        MEraEngineSysCallbackFfi::on_input_int(self, default_value.into(), can_click, allow_skip)
            .into()
    }
    fn on_input_str(
        &mut self,
        default_value: Option<&str>,
        can_click: bool,
        allow_skip: bool,
    ) -> ControlFlow<(), Option<String>> {
        MEraEngineSysCallbackFfi::on_input_str(
            self,
            default_value.map(Into::into),
            can_click,
            allow_skip,
        )
        .map_continue(|x| x.map(|x| unsafe { x.as_ref().to_string() }))
    }
    fn on_tinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        MEraEngineSysCallbackFfi::on_tinput_int(
            self,
            time_limit,
            default_value,
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
    ) -> ControlFlow<(), Option<String>> {
        MEraEngineSysCallbackFfi::on_tinput_str(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .map_continue(|x| x.map(|x| unsafe { x.as_ref().to_string() }))
    }
    fn on_oneinput_int(&mut self, default_value: Option<i64>) -> ControlFlow<(), Option<i64>> {
        MEraEngineSysCallbackFfi::on_oneinput_int(self, default_value.into()).into()
    }
    fn on_oneinput_str(&mut self, default_value: Option<&str>) -> ControlFlow<(), Option<String>> {
        MEraEngineSysCallbackFfi::on_oneinput_str(self, default_value.map(Into::into))
            .map_continue(|x| x.map(|x| unsafe { x.as_ref().to_string() }))
    }
    fn on_toneinput_int(
        &mut self,
        time_limit: i64,
        default_value: i64,
        show_prompt: bool,
        expiry_msg: &str,
        can_click: bool,
    ) -> ControlFlow<(), Option<i64>> {
        MEraEngineSysCallbackFfi::on_toneinput_int(
            self,
            time_limit,
            default_value,
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
    ) -> ControlFlow<(), Option<String>> {
        MEraEngineSysCallbackFfi::on_toneinput_str(
            self,
            time_limit,
            default_value.into(),
            show_prompt,
            expiry_msg.into(),
            can_click,
        )
        .map_continue(|x| x.map(|x| unsafe { x.as_ref().to_string() }))
    }
    fn on_reuselastline(&mut self, content: &str) {
        MEraEngineSysCallbackFfi::on_reuselastline(self, content.into());
    }
    fn on_clearline(&mut self, count: i64) {
        MEraEngineSysCallbackFfi::on_clearline(self, count);
    }
    fn on_print_button(&mut self, content: &str, value: &str, flags: EraPrintExtendedFlags) {
        MEraEngineSysCallbackFfi::on_print_button(self, content.into(), value.into(), flags.into());
    }
    fn on_var_get_int(&mut self, name: &str, idx: usize) -> Result<i64, anyhow::Error> {
        MEraEngineSysCallbackFfi::on_var_get_int(self, name.into(), idx).into_anyhow()
    }
    fn on_var_get_str(&mut self, name: &str, idx: usize) -> Result<String, anyhow::Error> {
        MEraEngineSysCallbackFfi::on_var_get_str(self, name.into(), idx)
            .into_anyhow()
            .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_var_set_int(&mut self, name: &str, idx: usize, val: i64) -> Result<(), anyhow::Error> {
        MEraEngineSysCallbackFfi::on_var_set_int(self, name.into(), idx, val).into_anyhow()
    }
    fn on_var_set_str(&mut self, name: &str, idx: usize, val: &str) -> Result<(), anyhow::Error> {
        MEraEngineSysCallbackFfi::on_var_set_str(self, name.into(), idx, val.into()).into_anyhow()
    }
    fn on_gcreate(&mut self, gid: i64, width: i64, height: i64) -> i64 {
        MEraEngineSysCallbackFfi::on_gcreate(self, gid, width, height)
    }
    fn on_gcreatefromfile(&mut self, gid: i64, path: &str) -> i64 {
        MEraEngineSysCallbackFfi::on_gcreatefromfile(self, gid, path.into())
    }
    fn on_gdispose(&mut self, gid: i64) -> i64 {
        MEraEngineSysCallbackFfi::on_gdispose(self, gid)
    }
    fn on_gcreated(&mut self, gid: i64) -> i64 {
        MEraEngineSysCallbackFfi::on_gcreated(self, gid)
    }
    fn on_gdrawsprite(
        &mut self,
        gid: i64,
        sprite_name: &str,
        dest_x: i64,
        dest_y: i64,
        dest_width: i64,
        dest_height: i64,
        color_matrix: Option<&EraColorMatrix>,
    ) -> i64 {
        MEraEngineSysCallbackFfi::on_gdrawsprite(
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
        MEraEngineSysCallbackFfi::on_gclear(self, gid, color)
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
        MEraEngineSysCallbackFfi::on_spritecreate(self, name.into(), gid, x, y, width, height)
    }
    fn on_spritedispose(&mut self, name: &str) -> i64 {
        MEraEngineSysCallbackFfi::on_spritedispose(self, name.into())
    }
    fn on_spritecreated(&mut self, name: &str) -> i64 {
        MEraEngineSysCallbackFfi::on_spritecreated(self, name.into())
    }
    fn on_spriteanimecreate(&mut self, name: &str, width: i64, height: i64) -> i64 {
        MEraEngineSysCallbackFfi::on_spriteanimecreate(self, name.into(), width, height)
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
        MEraEngineSysCallbackFfi::on_spriteanimeaddframe(
            self,
            name.into(),
            gid,
            x,
            y,
            width,
            height,
            offset_x,
            offset_y,
            delay,
        )
    }
    fn on_spritewidth(&mut self, name: &str) -> i64 {
        MEraEngineSysCallbackFfi::on_spritewidth(self, name.into())
    }
    fn on_spriteheight(&mut self, name: &str) -> i64 {
        MEraEngineSysCallbackFfi::on_spriteheight(self, name.into())
    }
    // Filesystem subsystem
    fn on_open_host_file(
        &mut self,
        path: &str,
        can_write: bool,
    ) -> anyhow::Result<Box<dyn MEraEngineHostFile>> {
        MEraEngineSysCallbackFfi::on_open_host_file(self, path.into(), can_write)
            .into_anyhow()
            .map(|x| -> Box<dyn MEraEngineHostFile> { Box::new(x) })
    }
    fn on_check_host_file_exists(&mut self, path: &str) -> anyhow::Result<bool> {
        MEraEngineSysCallbackFfi::on_check_host_file_exists(self, path.into()).into_anyhow()
    }
    fn on_delete_host_file(&mut self, path: &str) -> anyhow::Result<()> {
        MEraEngineSysCallbackFfi::on_delete_host_file(self, path.into()).into_anyhow()
    }
    fn on_list_host_file(&mut self, path: &str) -> anyhow::Result<Vec<String>> {
        let mut file_listing: VirtualPtr<dyn MEraEngineHostFileListingFfi> =
            MEraEngineSysCallbackFfi::on_list_host_file(self, path.into()).into_anyhow()?;
        let mut files = Vec::new();
        loop {
            let entry = file_listing.next();
            let MEraEngineHostFileListingEntryFfi {
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
        MEraEngineSysCallbackFfi::on_check_font(self, font_name.into())
    }
    fn on_get_host_time(&mut self) -> u64 {
        MEraEngineSysCallbackFfi::on_get_host_time(self)
    }
    fn on_get_config_int(&mut self, name: &str) -> anyhow::Result<i64> {
        MEraEngineSysCallbackFfi::on_get_config_int(self, name.into()).into_anyhow()
    }
    fn on_get_config_str(&mut self, name: &str) -> anyhow::Result<String> {
        MEraEngineSysCallbackFfi::on_get_config_str(self, name.into())
            .into_anyhow()
            .map(|x| unsafe { x.as_ref().to_string() })
    }
    fn on_get_key_state(&mut self, key_code: i64) -> i64 {
        MEraEngineSysCallbackFfi::on_get_key_state(self, key_code)
    }
    fn on_await(&mut self, milliseconds: i64) {
        MEraEngineSysCallbackFfi::on_await(self, milliseconds);
    }
}

// TODO: Support builder progress

#[derive_ReprC]
#[repr(opaque)]
struct MEraEngineBuilderFfi {
    i: MEraEngineBuilder<VirtualPtr<dyn MEraEngineSysCallbackFfi>, EmptyCallback>,
}

#[derive_ReprC]
#[repr(opaque)]
struct MEraEngineFfi {
    i: MEraEngine<VirtualPtr<dyn MEraEngineSysCallbackFfi>>,
}

#[ffi_export]
fn mee_get_engine_version() -> string::str_ref<'static> {
    MEraEngine::<EmptyCallback>::get_version().into()
}

#[ffi_export]
fn mee_new_engine_builder(
    callback: VirtualPtr<dyn MEraEngineSysCallbackFfi>,
) -> repr_c::Box<MEraEngineBuilderFfi> {
    let builder = MEraEngineBuilder::new(callback);
    Box::new(MEraEngineBuilderFfi { i: builder }).into()
}

#[ffi_export]
fn mee_drop_engine_builder(builder: repr_c::Box<MEraEngineBuilderFfi>) {
    drop(builder);
}

#[ffi_export]
fn mee_drop_engine(engine: repr_c::Box<MEraEngineFfi>) {
    drop(engine);
}

#[ffi_export]
fn mee_drop_engine_async_erb_loader(loader: repr_c::Box<MEraEngineAsyncErbLoader>) {
    drop(loader);
}

#[ffi_export]
fn mee_build_engine(
    builder: repr_c::Box<MEraEngineBuilderFfi>,
) -> MeeResult<Option<repr_c::Box<MEraEngineFfi>>> {
    let builder: Box<_> = builder.into();
    let engine = match builder.i.build() {
        Ok(engine) => engine,
        Err(e) => return e.into_mee_result(),
    };
    let engine: repr_c::Box<MEraEngineFfi> = Box::new(MEraEngineFfi { i: engine }).into();
    MeeResult {
        is_ok: true,
        ok: Some(engine),
        err: String::new().into(),
    }
}

#[ffi_export]
fn mee_engine_async_erb_loader_load_erb(
    loader: &mut MEraEngineAsyncErbLoader,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
) -> MeeResult<()> {
    loader
        .load_erb(filename.to_str(), &content)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_get_config(builder: &mut MEraEngineBuilderFfi) -> MEraEngineConfig {
    builder.i.get_config()
}

#[ffi_export]
fn mee_engine_builder_set_config(
    builder: &mut MEraEngineBuilderFfi,
    config: MEraEngineConfig,
) -> MeeResult<()> {
    builder.i.set_config(config).into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_load_csv(
    builder: &mut MEraEngineBuilderFfi,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
    kind: EraCsvLoadKind,
) -> MeeResult<()> {
    builder
        .i
        .load_csv(filename.to_str(), &content, kind)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_finish_load_csv(builder: &mut MEraEngineBuilderFfi) -> MeeResult<()> {
    builder.i.finish_load_csv().into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_load_erh(
    builder: &mut MEraEngineBuilderFfi,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
) -> MeeResult<()> {
    builder
        .i
        .load_erh(filename.to_str(), &content)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_finish_load_erh(builder: &mut MEraEngineBuilderFfi) -> MeeResult<()> {
    builder.i.finish_load_erh().into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_load_erb(
    builder: &mut MEraEngineBuilderFfi,
    filename: char_p::Ref<'_>,
    content: c_slice::Ref<'_, u8>,
) -> MeeResult<()> {
    builder
        .i
        .load_erb(filename.to_str(), &content)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_start_async_erb_loader(
    builder: &mut MEraEngineBuilderFfi,
) -> repr_c::Box<MEraEngineAsyncErbLoader> {
    Box::new(builder.i.start_async_erb_loader()).into()
}

#[ffi_export]
fn mee_engine_builder_wait_for_async_loader(builder: &mut MEraEngineBuilderFfi) -> MeeResult<()> {
    builder.i.wait_for_async_loader().into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_register_variable(
    builder: &mut MEraEngineBuilderFfi,
    name: char_p::Ref<'_>,
    is_string: bool,
    dimension: u32,
    watch: bool,
) -> MeeResult<()> {
    builder
        .i
        .register_variable(name.to_str(), is_string, dimension, watch)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_set_variable_int(
    builder: &mut MEraEngineBuilderFfi,
    name: char_p::Ref<'_>,
    index: usize,
    value: i64,
) -> MeeResult<()> {
    builder
        .i
        .set_variable_int(name.to_str(), index, value)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_builder_set_variable_str(
    builder: &mut MEraEngineBuilderFfi,
    name: char_p::Ref<'_>,
    index: usize,
    value: string::str_ref<'_>,
) -> MeeResult<()> {
    builder
        .i
        .set_variable_str(name.to_str(), index, value.as_str())
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_get_config(engine: &MEraEngineFfi) -> MEraEngineConfig {
    engine.i.get_config().clone()
}

#[ffi_export]
fn mee_engine_set_config(engine: &mut MEraEngineFfi, config: MEraEngineConfig) -> MeeResult<()> {
    engine.i.set_config(config).into_mee_result()
}

#[ffi_export]
fn mee_engine_do_execution(
    engine: &mut MEraEngineFfi,
    run_flag: &mut bool,
    max_inst_cnt: u64,
) -> MeeResult<EraExecutionBreakReason> {
    let run_flag = unsafe { AtomicBool::from_ptr(run_flag) };
    engine
        .i
        .do_execution(run_flag, max_inst_cnt)
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_do_rpc(engine: &mut MEraEngineFfi, request: char_p::Ref<'_>) -> string::String {
    engine.i.do_rpc(request.to_str()).into()
}

#[ffi_export]
fn mee_engine_take_snapshot(
    engine: &mut MEraEngineFfi,
    parts_to_add: u32,
) -> MeeResult<repr_c::Vec<u8>> {
    engine
        .i
        .take_snapshot(EraEngineSnapshotKind::from_bits_truncate(parts_to_add))
        .into_mee_result()
}

#[ffi_export]
fn mee_engine_restore_snapshot(
    engine: &mut MEraEngineFfi,
    snapshot: c_slice::Ref<'_, u8>,
) -> MeeResult<()> {
    engine.i.restore_snapshot(&snapshot).into_mee_result()
}

#[derive_ReprC]
#[repr(C)]
pub struct DiagnosticProviderFfi<'p, 'a> {
    i: &'p DiagnosticProvider<'a>,
}

#[ffi_export]
fn mee_diag_get_entry_count(diag: DiagnosticProviderFfi<'_, '_>) -> u64 {
    diag.i.get_entries().len() as u64
}

#[derive_ReprC]
#[repr(C)]
struct DiagnosticEntryFfi<'a> {
    pub level: DiagnosticLevel,
    /// The filename of the source file. Empty if the source file is not specified.
    pub filename: string::str_ref<'a>,
    pub span: SrcSpan,
    pub message: string::str_ref<'a>,
}

impl Default for DiagnosticEntryFfi<'_> {
    fn default() -> Self {
        Self {
            level: DiagnosticLevel::Error,
            filename: "".into(),
            span: SrcSpan::default(),
            message: "".into(),
        }
    }
}

#[ffi_export]
fn mee_diag_get_entry<'p>(
    diag: DiagnosticProviderFfi<'p, '_>,
    idx: u64,
) -> RustOption<DiagnosticEntryFfi<'p>> {
    let Some(entry) = diag.i.get_entries().get(idx as usize) else {
        return None::<DiagnosticEntryFfi<'p>>.into();
    };
    Some(DiagnosticEntryFfi {
        level: entry.level,
        filename: entry.filename.as_str().into(),
        span: entry.span,
        message: entry.message.as_str().into(),
    })
    .into()
}

#[derive_ReprC]
#[repr(C)]
pub struct DiagnosticResolveSrcSpanResultFfi {
    /// Single line of source code containing the span.
    pub snippet: string::String,
    /// Offset into the snippet where the span starts.
    pub offset: u32,
    /// Location of the span.
    pub loc: SrcLoc,
    /// Length of the span.
    pub len: u32,
}

impl Default for DiagnosticResolveSrcSpanResultFfi {
    fn default() -> Self {
        Self {
            snippet: String::new().into(),
            offset: 0,
            loc: SrcLoc::default(),
            len: 0,
        }
    }
}

#[ffi_export]
fn mee_diag_resolve_src_span<'p>(
    diag: DiagnosticProviderFfi<'p, '_>,
    filename: string::str_ref<'_>,
    span: SrcSpan,
) -> RustOption<DiagnosticResolveSrcSpanResultFfi> {
    let filename = filename.as_str();
    let Some(result) = diag.i.resolve_src_span(filename, span) else {
        return None::<DiagnosticResolveSrcSpanResultFfi>.into();
    };
    Some(DiagnosticResolveSrcSpanResultFfi {
        snippet: result.snippet.into(),
        offset: result.offset,
        loc: result.loc,
        len: result.len,
    })
    .into()
}

#[ffi_export]
fn mee_do_nothing_to_export_types_eagerly(
    _v1: ValueKind,
    _v2: ScalarValueKind,
    _v3: StackValueKind,
) {
}
// ----- End of FFI Exports -----

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, sync::atomic::AtomicBool};

    use anyhow::Context;
    use colored::{Color, Colorize};
    use indoc::indoc;
    use types::EraPrintExtendedFlags;
    use v2::{engine::MEraEngineRpc, vm::EraExecIp};

    use super::*;

    #[cfg(feature = "legacy_v1")]
    trait EngineExtension {
        fn reg_int(&mut self, name: &str) -> Result<(), MEraEngineError>;
        fn reg_str(&mut self, name: &str) -> Result<(), MEraEngineError>;
    }

    #[cfg(feature = "legacy_v1")]
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
    }
    impl<'a> MockEngineCallback<'a> {
        fn new(errors: &'a RefCell<String>) -> Self {
            Self {
                errors,
                output: String::new(),
                err_cnt: 0,
                warn_cnt: 0,
            }
        }
    }

    impl v2::engine::MEraEngineSysCallback for &mut MockEngineCallback<'_> {
        fn on_error(&mut self, diag: &types::DiagnosticProvider) {
            use unicode_width::UnicodeWidthStr;

            let mut errors = self.errors.borrow_mut();

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
                let Some(mut resolved) = diag.resolve_src_span(&entry.filename, entry.span) else {
                    let msg = format!(
                        "{}: {}: {}\nSnippet info not available\n",
                        entry.filename, noun, entry.message,
                    )
                    .color(color);
                    if PRINT_TO_STDOUT {
                        print!("{}", msg);
                    } else {
                        *errors += &msg.to_string();
                    }
                    continue;
                };
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

        fn on_print(&mut self, content: &str, flags: EraPrintExtendedFlags) {
            self.output += content;
        }

        fn on_html_print(&mut self, content: &str, no_single: i64) {}

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
            #DIM arg
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
        @EMPTYFN
        @BENCHMARK_FN(fn_name)
            #DIMS DYNAMIC fn_name
            #DIM DYNAMIC t1
            t1 = GETMILLISECOND()
            CALLFORM %fn_name%
            PRINTFORM `%fn_name%` took {(GETMILLISECOND() - t1)*0}ms.
        @ASK_CHOICES3(argChoices:0="", argChoices:1="", argChoices:2="")
            #DIMS argChoices, 3
            CALL ASK_CHOICES(argChoices)
            RETURN RESULT
        @ASK_CHOICES(refChoices, argCanselNo=-1,argCanselText="中止")
            {
                #DIM CONST SEASONALCOLORS =
                    0x3C2D0F, 0x2D2D00, 0x323D2D, 0x46461E,
                    0x5A4B1E, 0x787800, 0x648C4B, 0x8C8C3C,
                    0x4B3C14, 0x4B4B00, 0x50643C, 0x64642D,
                    0x2D1E0A, 0x141400, 0x141E0F, 0x28280F,
                    0x3C2D0F, 0x64963C, 0x238C32, 0x78460F,
                    0x5A4B1E, 0xA0D264, 0x50B45A, 0xB4961E,
                    0x4B3C14, 0x82B450, 0x32A046, 0x966E14,
                    0x2D1E0A, 0x3C7828, 0x00641E, 0x3C280A,
                    0x321E14, 0xF096C8, 0x643C5A, 0x5A4B32,
                    0x28190F, 0xD26496, 0x502846, 0x4B3C28,
                    0x1E140A, 0xA03C64, 0x3C1E37, 0x3C2D1E,
                    0x140A05, 0x5A283C, 0x281423, 0x2D1E14
            }
            #DIMS REF refChoices, 0
            #DIM DYNAMIC argCanselNo
            #DIMS DYNAMIC argCanselText
            #DIMS html, 4
            #DIM skip, 4
            ;生成
            0+VARSIZE("refChoices")
            PRINTFORM %VARSIZE("SEASONALCOLORS")%
            SEASONALCOLORS:20 + 0
            VARSET skip
            VARSET html
        @SYSTEM_TITLE()
            ;#DIM REF xre
            #DIM val = 1 + 1 ;*0
            #DIMS world_str, -2 + 3 = ""
            #DIMS tmpstr = "[" + ";" + "&" + "]"

            ;#DIM cnt = 10000000
            #DIM cnt = 100
            #DIM DYNAMIC sum1

            ; ------------------------------

            FOR LOCAL, 0, 10
                PRINTFORM {GROUPMATCH(LOCAL, 3, 5, 7, 8)}
            NEXT

            ASK_CHOICES3("a", "b", "c")

            PRINTSINGLEFORMS "== %ARGS% " + "%(\"=\" * 3)%"
            BENCHMARK_FN("EMPTYFN")

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

    const PRINT_TO_STDOUT: bool = false;

    #[test]
    fn basic_engine() -> anyhow::Result<()> {
        let main_erb = BASIC_ENGINE_SRC;
        let errors = RefCell::new(String::new());
        let mut callback = MockEngineCallback::new(&errors);
        let mut builder = v2::engine::MEraEngineBuilder::new(&mut callback);
        builder.register_variable("WINDOW_TITLE", true, 1, true)?;
        // builder.finish_load_csv()?;
        // builder.finish_load_erh()?;
        builder.load_erb("main.erb", main_erb.as_bytes())?;
        let mut engine = builder.build()?;
        *errors.borrow_mut() += &format!("----- Engine built -----\n");
        let func_info = engine
            .get_func_info("SYSTEM_TITLE")
            .context("SYSTEM_TITLE not found")?;
        let ip = EraExecIp {
            chunk: func_info.chunk_idx,
            offset: func_info.bc_offset,
        };
        engine.reset_exec_to_ip(ip)?;
        let run_flag = AtomicBool::new(true);
        _ = engine.do_execution(&run_flag, 1024 * 1024);
        // assert!(engine.get_is_halted());
        drop(engine);
        {
            let errors = errors.borrow();
            // if errors.contains("error:") {
            //     panic!(
            //         "compile output:\n{errors}\nexec output:\n{}",
            //         callback.output
            //     );
            // }
            if !errors.is_empty() {
                println!("compile output:\n{errors}");
            }
        }
        assert_eq!(
            &callback.output,
            "`EMPTYFN` took 0ms.(5050)[0,0,0,3][0,3,2,2]Hello, 4 the world!falseDone[IN 50][Ret][OK][IN 10][Ret][IN 100][Ret]55~-5050~WINDOW_TITLE![1]0135[;&]"
        );

        Ok(())
    }

    #[test]
    fn test_miri() -> anyhow::Result<()> {
        let main_erb = "@SYSTEM_TITLE()\nPRINTFORM Hello, world!";
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
        // HACK: Grow stack size
        match stacker::remaining_stack() {
            Some(remaining) if remaining < 4 * 1024 * 1024 => {
                return stacker::grow(8 * 1024 * 1024, assembled_game);
            }
            _ => (),
        }

        #[cfg(feature = "dhat-heap")]
        let _profiler = dhat::Profiler::new_heap();

        // TODO: Redact this
        // Read from environment variable
        let config = v2::engine::MEraEngineConfig {
            no_src_map: std::env::var("ERA_NO_SRC_MAP").is_ok(),
            threads_cnt: std::env::var("ERA_THREADS_CNT")
                .ok()
                .and_then(|x| x.parse().ok())
                .unwrap_or(1),
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
            let erb = &std::fs::read(&erb_path)?;
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

        let threaded_parse = std::env::var("ERA_THREADED_PARSE").is_ok();
        if threaded_parse {
            let loader = builder.start_async_erb_loader();
            std::thread::spawn(move || {
                for erb_path in erbs.into_iter() {
                    let filename = erb_path.to_string_lossy();
                    let content = std::fs::read(&erb_path).unwrap();
                    loader.load_erb(&filename, &content).unwrap();
                }
            });
            builder.wait_for_async_loader()?;
        } else {
            for erb_path in erbs.into_iter() {
                load_erb_fn(&mut builder, erb_path, false)?;
            }
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
        if std::env::var("ERA_LATE_DROP_DHAT").is_err() {
            drop(_profiler);
        }

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
