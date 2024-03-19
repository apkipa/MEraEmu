// A wrapper over Rust MEraEngine

#include "pch.h"

#include "MEraEngine.hpp"

#include "Tenkai.hpp"

//#pragma comment(lib, "MEraEmuCore.dll.lib")

#define STR_VIEW(rust_str) (std::string_view{ (const char*)(rust_str).ptr, (rust_str).len })
#define TO_SPAN(slice_like) (std::span{ (slice_like).ptr, (slice_like).len })
#define TO_RUST_SLICE(span) { .ptr = (span).data(), .len = (span).size() }
#define TO_RUST_OPTION(opt) { .has_value = (bool)(opt), .value = (opt) ? std::move(*(opt)) : {} }

template<typename T>
auto unwrap_rust_result(T result) {
    auto [ok, err] = result;
    if (!ok.has_value) {
        throw MEraEngineException(rust_String(std::move(err.msg)));
    }
    if constexpr (requires { ok.value; }) {
        return ok.value;
    }
}
template<typename T>
auto from_rust_option(T opt) -> decltype(std::optional{ std::move(opt.value) }) {
    using RetType = decltype(std::optional{ std::move(opt.value) });
    if (opt.has_value) {
        return RetType{ std::move(opt.value) };
    }
    return RetType{ std::nullopt };
}
template<typename Ret, typename T>
Ret to_rust_option(std::optional<T> opt) {
    if (opt) {
        return { .has_value = true, .value = std::move(*opt) };
    }
    return { .has_value = false };
}

template<typename T>
auto from_rust_type(T val) {
    if constexpr (std::is_same_v<T, slice_ref_uint8_t>) {
        if (val.ptr) {
            return std::string_view{ (const char*)val.ptr, val.len };
        }
        return std::string_view{};
    }
    else {
        return from_rust_option(std::move(val));
    }
}
template<typename T>
auto from_rust_opt(T val) {
    if constexpr (std::is_same_v<T, slice_ref_uint8_t>) {
        if (val.ptr) {
            return std::optional{ std::string_view{ (const char*)val.ptr, val.len } };
        }
        return std::optional<std::string_view>{ std::nullopt };
    }
    else {
        return from_rust_option(std::move(val));
    }
}
template<typename Ret, typename T>
Ret to_rust_type(T val) {
    if constexpr (std::is_same_v<T, char const*>) {
        return val;
    }
    else {
        return to_rust_option<Ret>(std::move(val));
    }
}

template<typename T, typename Functor>
T rust_exception_boundary(Functor&& functor) noexcept {
    try {
        if constexpr (requires(T t) { t.ok.value; }) {
            return { .ok = { true, functor() } };
        }
        else {
            functor();
            return { .ok = { true } };
        }
    }
    catch (std::exception const& e) {
        thread_local std::string ex_what{ e.what() };
        return { .ok = { false }, .err = { ex_what.c_str() } };
    }
    catch (...) {
        auto msg = "Uncaught exception at FFI boundary, aborting for safety.";
        (void)msg;
        std::abort();
    }
}

MEraEngine::MEraEngine() {
    m_engine = new_engine();
}
MEraEngine::~MEraEngine() {
    if (!m_engine) { return; }
    delete_engine(m_engine);
}
void MEraEngine::install_sys_callback(std::unique_ptr<MEraEngineSysCallback> callback) {
    VirtualPtr__Erased_ptr_MEraEngineSysCallbackInteropVTable_t c{
        .ptr = (Erased_t*)callback.release(),
        .vtable = {
            .release_vptr = [](Erased_t* self) noexcept {
                // Drop immediately (effectively executes `delete self;`)
                std::unique_ptr<MEraEngineSysCallback>{ (MEraEngineSysCallback*)self };
            },
            .on_compile_error = [](Erased_t* self, EraScriptErrorInfoInterop_t const* info) noexcept {
                EraScriptErrorInfo native_info{
                    .filename = STR_VIEW(info->filename),
                    .src_info = { info->src_info.line, info->src_info.column },
                    .is_error = info->is_error,
                    .msg = STR_VIEW(info->msg),
                };
                ((MEraEngineSysCallback*)self)->on_compile_error(native_info);
            },
            .on_execute_error = [](Erased_t* self, EraScriptErrorInfoInterop_t const* info) noexcept {
                EraScriptErrorInfo native_info{
                    .filename = STR_VIEW(info->filename),
                    .src_info = { info->src_info.line, info->src_info.column },
                    .is_error = info->is_error,
                    .msg = STR_VIEW(info->msg),
                };
                ((MEraEngineSysCallback*)self)->on_execute_error(native_info);
            },
            .on_get_rand = [](Erased_t* self) noexcept {
                return ((MEraEngineSysCallback*)self)->on_get_rand();
            },
            .on_print = [](Erased_t* self, slice_ref_uint8_t content, uint8_t flags) noexcept {
                ((MEraEngineSysCallback*)self)->on_print(STR_VIEW(content), (PrintExtendedFlags)flags);
            },
            .on_html_print = [](Erased_t* self, slice_ref_uint8_t content) noexcept {
                ((MEraEngineSysCallback*)self)->on_html_print(STR_VIEW(content));
            },
            .on_wait = [](Erased_t* self, bool any_key, bool is_force) noexcept {
                ((MEraEngineSysCallback*)self)->on_wait(any_key, is_force);
            },
            .on_twait = [](Erased_t* self, int64_t duration, bool is_force) noexcept {
                ((MEraEngineSysCallback*)self)->on_twait(duration, is_force);
            },
            .on_input_int = [](Erased_t* self, FfiOption_int64_t default_value, bool can_click, bool allow_skip) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_input_int(from_rust_type(default_value), can_click, allow_skip);
                return to_rust_type<FfiOption_int64_t>(r);
            },
            .on_input_str = [](Erased_t* self, slice_ref_uint8_t default_value, bool can_click, bool allow_skip) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_input_str(from_rust_opt(default_value), can_click, allow_skip);
                return to_rust_type<char const*>(r);
            },
            .on_tinput_int = [](Erased_t* self, int64_t time_limit, int64_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_tinput_int(time_limit, default_value, show_prompt, STR_VIEW(expiry_msg), can_click);
                return to_rust_type<FfiOption_int64_t>(r);
            },
            .on_tinput_str = [](Erased_t* self, int64_t time_limit, slice_ref_uint8_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_tinput_str(time_limit, STR_VIEW(default_value), show_prompt, STR_VIEW(expiry_msg), can_click);
                return to_rust_type<char const*>(r);
            },
            .on_oneinput_int = [](Erased_t* self, FfiOption_int64_t default_value) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_oneinput_int(from_rust_type(default_value));
                return to_rust_type<FfiOption_int64_t>(r);
            },
            .on_oneinput_str = [](Erased_t* self, slice_ref_uint8_t default_value) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_oneinput_str(from_rust_opt(default_value));
                return to_rust_type<char const*>(r);
            },
            .on_toneinput_int = [](Erased_t* self, int64_t time_limit, int64_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_toneinput_int(time_limit, default_value, show_prompt, STR_VIEW(expiry_msg), can_click);
                return to_rust_type<FfiOption_int64_t>(r);
            },
            .on_toneinput_str = [](Erased_t* self, int64_t time_limit, slice_ref_uint8_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
                auto r = ((MEraEngineSysCallback*)self)->on_toneinput_str(time_limit, STR_VIEW(default_value), show_prompt, STR_VIEW(expiry_msg), can_click);
                return to_rust_type<char const*>(r);
            },
            .on_reuselastline = [](Erased_t* self, slice_ref_uint8_t content) noexcept {
                ((MEraEngineSysCallback*)self)->on_reuselastline(STR_VIEW(content));
            },
            .on_clearline = [](Erased_t* self, int64_t count) noexcept {
                ((MEraEngineSysCallback*)self)->on_clearline(count);
            },
            .on_var_get_int = [](Erased_t* self, slice_ref_uint8_t name, uint32_t idx) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_int64_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_var_get_int(STR_VIEW(name), idx);
                });
            },
            .on_var_get_str = [](Erased_t* self, slice_ref_uint8_t name, uint32_t idx) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_char_const_ptr_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_var_get_str(STR_VIEW(name), idx);
                });
            },
            .on_var_set_int = [](Erased_t* self, slice_ref_uint8_t name, uint32_t idx, int64_t val) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_void_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_var_set_int(STR_VIEW(name), idx, val);
                });
            },
            .on_var_set_str = [](Erased_t* self, slice_ref_uint8_t name, uint32_t idx, slice_ref_uint8_t val) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_void_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_var_set_str(STR_VIEW(name), idx, STR_VIEW(val));
                });
            },
            .on_print_button = [](Erased_t* self, slice_ref_uint8_t content, slice_ref_uint8_t value, uint8_t flags) noexcept {
                ((MEraEngineSysCallback*)self)->on_print_button(STR_VIEW(content), STR_VIEW(value), (PrintExtendedFlags)flags);
            },
            .on_gcreate = [](Erased_t* self, int64_t gid, int64_t width, int64_t height) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gcreate(gid, width, height);
            },
            .on_gcreatefromfile = [](Erased_t* self, int64_t gid, slice_ref_uint8_t path) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gcreatefromfile(gid, STR_VIEW(path));
            },
            .on_gdispose = [](Erased_t* self, int64_t gid) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gdispose(gid);
            },
            .on_gcreated = [](Erased_t* self, int64_t gid) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gcreated(gid);
            },
            .on_gdrawsprite = [](Erased_t* self, int64_t gid, slice_ref_uint8_t sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const* color_matrix) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gdrawsprite(gid, STR_VIEW(sprite_name), dest_x, dest_y, dest_width, dest_height, *color_matrix);
            },
            .on_gclear = [](Erased_t* self, int64_t gid, int64_t color) noexcept {
                return ((MEraEngineSysCallback*)self)->on_gclear(gid, color);
            },
            .on_spritecreate = [](Erased_t* self, slice_ref_uint8_t name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spritecreate(STR_VIEW(name), gid, x, y, width, height);
            },
            .on_spritedispose = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spritedispose(STR_VIEW(name));
            },
            .on_spritecreated = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spritecreated(STR_VIEW(name));
            },
            .on_spriteanimecreate = [](Erased_t* self, slice_ref_uint8_t name, int64_t width, int64_t height) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spriteanimecreate(STR_VIEW(name), width, height);
            },
            .on_spriteanimeaddframe = [](Erased_t* self, slice_ref_uint8_t name, int64_t gid, int64_t x, int64_t y, Pair_int64_int64_t dimension, int64_t offset_x, int64_t offset_y, int64_t delay) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spriteanimeaddframe(STR_VIEW(name), gid, x, y, dimension.a, dimension.b, offset_x, offset_y, delay);
            },
            .on_spritewidth = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spritewidth(STR_VIEW(name));
            },
            .on_spriteheight = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return ((MEraEngineSysCallback*)self)->on_spriteheight(STR_VIEW(name));
            },
            // TODO: FFI callbacks...
            .on_check_font = [](Erased_t* self, slice_ref_uint8_t font_name) noexcept {
                return ((MEraEngineSysCallback*)self)->on_check_font(STR_VIEW(font_name));
            },
            .on_get_host_time = [](Erased_t* self) noexcept {
                return ((MEraEngineSysCallback*)self)->on_get_host_time();
            },
            .on_get_config_int = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_int64_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_get_config_int(STR_VIEW(name));
                });
            },
            .on_get_config_str = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
                return rust_exception_boundary<MEraEngineFfiResult_char_const_ptr_t>([&] {
                    return ((MEraEngineSysCallback*)self)->on_get_config_str(STR_VIEW(name));
                });
            },
            .on_get_key_state = [](Erased_t* self, int64_t key_code) noexcept {
                return ((MEraEngineSysCallback*)self)->on_get_key_state(key_code);
            },
        },
    };
    engine_install_sys_callback(m_engine, c);
}
void MEraEngine::load_csv(const char* filename, std::span<uint8_t> content, EraCsvLoadKind kind) {
    unwrap_rust_result(engine_load_csv(m_engine, filename, TO_RUST_SLICE(content), static_cast<EraCsvLoadKind_t>(kind)));
}
void MEraEngine::load_erh(const char* filename, std::span<uint8_t> content) {
    unwrap_rust_result(engine_load_erh(m_engine, filename, TO_RUST_SLICE(content)));
}
void MEraEngine::load_erb(const char* filename, std::span<uint8_t> content) {
    unwrap_rust_result(engine_load_erb(m_engine, filename, TO_RUST_SLICE(content)));
}
void MEraEngine::register_global_var(const char* name, bool is_string, uint32_t dimension, bool watch) {
    unwrap_rust_result(engine_register_global_var(m_engine, name, is_string, dimension, watch));
}
void MEraEngine::finialize_load_srcs() {
    unwrap_rust_result(engine_finialize_load_srcs(m_engine));
}
bool MEraEngine::do_execution(_Atomic(bool)*stop_flag, uint64_t max_inst_cnt) {
    return unwrap_rust_result(engine_do_execution(m_engine, reinterpret_cast<bool*>(stop_flag), max_inst_cnt));
}
bool MEraEngine::get_is_halted() {
    return engine_get_is_halted(m_engine);
}
void MEraEngine::reset_exec_to_ip(EraExecIpInfo ip) {
    unwrap_rust_result(engine_reset_exec_to_ip(m_engine, ip));
}
EraFuncInfo MEraEngine::get_func_info(const char* name) {
    return unwrap_rust_result(engine_get_func_info(m_engine, name));
}
MEraEngineStackTrace MEraEngine::get_stack_trace() {
    auto rust_stack_trace = unwrap_rust_result(engine_get_stack_trace(m_engine));
    tenkai::cpp_utils::ScopeExit se_rust([&] {
        delete_engine_stack_trace(rust_stack_trace);
    });
    MEraEngineStackTrace stack_trace;
    for (auto const& rust_frame : TO_SPAN(rust_stack_trace.frames)) {
        MEraEngineStackTraceFrame frame{
            .file_name = STR_VIEW(rust_frame.file_name),
            .func_name = STR_VIEW(rust_frame.func_name),
            .ip = rust_frame.ip,
            .src_info = { rust_frame.src_info.line, rust_frame.src_info.column },
        };
        stack_trace.frames.push_back(std::move(frame));
    }
    return stack_trace;
}
std::string_view MEraEngine::get_version() {
    return STR_VIEW(engine_get_version());
}
