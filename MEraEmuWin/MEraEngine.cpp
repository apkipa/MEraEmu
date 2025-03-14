// A wrapper over Rust MEraEngine

#include "pch.h"

#include "MEraEngine.hpp"
#include "Tenkai.hpp"

//#pragma comment(lib, "MEraEmuCore.dll.lib")

#define STR_VIEW(rust_str) (std::string_view{ (const char*)(rust_str).ptr, (rust_str).len })
#define TO_SPAN(slice_like) (std::span{ (slice_like).ptr, (slice_like).len })
#define TO_RUST_SLICE(span) { .ptr = (span).data(), .len = (span).size() }
#define TO_RUST_STR(span) { .ptr = (uint8_t const*)(span).data(), .len = (span).size() }
#define TO_RUST_OPTION(opt) { .has_value = (bool)(opt), .value = (opt) ? std::move(*(opt)) : {} }

template<typename Ret, typename T>
Ret to_rust_type(T val);

template<typename T>
auto unwrap_rust(T result) {
    if constexpr (requires { result.is_ok; }) {
        if (!result.is_ok) {
            throw MEraEngineException(rust_String(result.err));
        }
        if constexpr (requires { result.ok; }) {
            return result.ok;
        }
    }
    else if constexpr (requires { result.is_some; }) {
        if (!result.is_some) {
            throw MEraEngineException("called `Option::unwrap()` on a `None` value");
        }
        if constexpr (requires { result.some; }) {
            return result.some;
        }
    }
    else {
        static_assert(false, "Unsupported Rust result type (expected `Result` or `Option`)");
    }
}
template<typename T>
auto from_rust_option(T opt) -> decltype(std::optional{ std::move(opt.some) }) {
    using RetType = decltype(std::optional{ std::move(opt.some) });
    if (opt.is_some) {
        return RetType{ std::move(opt.some) };
    }
    return RetType{ std::nullopt };
}
template<typename Ret, typename T>
Ret to_rust_option(std::optional<T> opt) {
    if (opt) {
        return { .is_some = true, .some = std::move(*opt) };
    }
    return { .is_some = false };
}
template<typename Ret, typename T>
Ret to_rust_control_flow(T val) {
    if constexpr (std::is_same_v<T, std::nullopt_t>) {
        return { .is_break = true };
    }
    return {
        .is_break = false,
        .continue_value = std::move(val),
    };
}
template<typename Ret, typename B, typename C>
Ret to_rust_control_flow(ControlFlow<B, C> val) {
    if (val.is_break()) {
        if constexpr (std::is_same_v<B, std::monostate>) {
            return { .is_break = true };
        }
        else {
            using Ty = decltype(std::declval<Ret>().break_value);
            return {
                .is_break = true,
                .break_value = to_rust_type<Ty, B>(std::move(*val.as_break())),
            };
        }
    }
    else {
        if constexpr (std::is_same_v<C, std::monostate>) {
            return { .is_break = false };
        }
        else {
            using Ty = decltype(std::declval<Ret>().continue_value);
            return {
                .is_break = false,
                .continue_value = to_rust_type<Ty, C>(std::move(*val.as_continue())),
            };
        }
    }
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
    else if constexpr (requires(Ret r) { r.is_some; }) {
        return to_rust_option<Ret>(std::move(val));
    }
    else if constexpr (requires(Ret r) { r.is_ok; }) {
        return { .is_ok = true, .ok = std::move(val) };
    }
    else if constexpr (requires(Ret r) { r.is_break; }) {
        return to_rust_control_flow<Ret>(std::move(val));
    }
    else {
        static_assert(false, "Unsupported Rust type (expected `Result`, `Option`, or `ControlFlow`)");
    }
}

thread_local std::string t_rust_exception_boundary_ex_msg;
template<typename T, typename Functor>
T rust_exception_boundary(Functor&& functor) noexcept {
    try {
        if constexpr (requires(T t) { t.ok; }) {
            return { .is_ok = true, .ok = functor() };
        }
        else {
            functor();
            return { .is_ok = true };
        }
    }
    catch (std::exception const& e) {
        auto& ex_msg = t_rust_exception_boundary_ex_msg;
        ex_msg = e.what();

        // Fix vtable on exception
        if constexpr (requires(T t) { t.ok.vtable; }) {
            return {
                .is_ok = { false },
                .ok = {.vtable = {.release_vptr = [](Erased_t*) {}}},
                .err = { ex_msg.c_str() },
            };
        }

        return { .is_ok = { false }, .err = { ex_msg.c_str() } };
    }
#ifdef CPPWINRT_VERSION
    catch (winrt::hresult_error const& e) {
        auto& ex_msg = t_rust_exception_boundary_ex_msg;
        ex_msg = winrt::to_string(winrt::format(L"0x{:08x}\n{}", (uint32_t)e.code(), e.message()));

        // Fix vtable on exception
        if constexpr (requires(T t) { t.ok.vtable; }) {
            return {
                .is_ok = { false },
                .ok = {.vtable = {.release_vptr = [](Erased_t*) {}}},
                .err = { ex_msg.c_str() },
            };
        }

        return { .is_ok = { false }, .err = { ex_msg.c_str() } };
    }
#endif
    catch (...) {
        auto msg = "Uncaught exception at FFI boundary, aborting for safety.";
        (void)msg;
        std::abort();
    }
}

nlohmann::json make_jsonrpc(std::string_view method, nlohmann::json params, uint64_t id = 1) {
    return {
        { "jsonrpc", "2.0" },
        { "method", method },
        { "params", std::move(params) },
        { "id", id },
    };
}

nlohmann::json unwrap_jsonrpc(std::string_view json) {
    auto j = nlohmann::json::parse(json);
    if (j.contains("error")) {
        auto& err = j["error"];
        throw MEraEngineException(err["message"].get<std::string>());
    }
    auto& jresult = j["result"];
    if (jresult.contains("Err")) {
        auto& err = jresult["Err"];
        if (err.contains("msg")) {
            throw MEraEngineException(err["msg"].get<std::string>());
        }
        throw MEraEngineException(err.get<std::string>());
    }
    return std::move(jresult.contains("Ok") ? jresult["Ok"] : jresult);
}

std::optional<EraDiagnosticEntry> EraDiagnosticProvider::get_entry(uint64_t idx) const {
    return from_rust_opt(mee_diag_get_entry(m_provider, idx)).transform([](auto entry) {
        return EraDiagnosticEntry{
            .level = (DiagnosticLevel)entry.level,
            .filename = STR_VIEW(entry.filename),
            .span = entry.span,
            .message = STR_VIEW(entry.message),
        };
    });
}

uint64_t EraDiagnosticProvider::get_entry_count() const {
    return mee_diag_get_entry_count(m_provider);
}

std::optional<DiagnosticResolveSrcSpanResult> EraDiagnosticProvider::resolve_src_span(std::string_view filename, SrcSpan span) const {
    return from_rust_opt(mee_diag_resolve_src_span(m_provider, TO_RUST_STR(filename), span)).transform([](auto result) {
        return DiagnosticResolveSrcSpanResult{
            .snippet = rust_String(result.snippet).to_string(),
            .offset = result.offset,
            .loc = result.loc,
            .len = result.len,
        };
    });
}

MEraEngineBuilder::MEraEngineBuilder(std::unique_ptr<MEraEngineSysCallback> callback) {
    MEraEngineSysCallbackFfiVTable_t vtbl{
        .release_vptr = [](Erased_t* self) noexcept {
            delete (MEraEngineSysCallback*)self;
        },
        .on_error = [](Erased_t* self, DiagnosticProviderFfi_t provider) noexcept {
            ((MEraEngineSysCallback*)self)->on_error(EraDiagnosticProvider{ provider });
        },
        .on_get_rand = [](Erased_t* self) noexcept {
            return ((MEraEngineSysCallback*)self)->on_get_rand();
        },
        .on_print = [](Erased_t* self, slice_ref_uint8_t content, uint8_t flags) noexcept {
            ((MEraEngineSysCallback*)self)->on_print(STR_VIEW(content), (PrintExtendedFlags)flags);
        },
        .on_html_print = [](Erased_t* self, slice_ref_uint8_t content, int64_t no_single) noexcept {
            ((MEraEngineSysCallback*)self)->on_html_print(STR_VIEW(content), no_single);
        },
        .on_html_popprintingstr = [](Erased_t* self) noexcept {
            return ((MEraEngineSysCallback*)self)->on_html_popprintingstr();
        },
        .on_html_getprintedstr = [](Erased_t* self, int64_t line_no) noexcept {
            return ((MEraEngineSysCallback*)self)->on_html_getprintedstr(line_no);
        },
        .on_html_stringlen = [](Erased_t* self, slice_ref_uint8_t content, bool return_pixel) noexcept {
            return ((MEraEngineSysCallback*)self)->on_html_stringlen(STR_VIEW(content), return_pixel);
        },
        .on_wait = [](Erased_t* self, bool any_key, bool is_force) noexcept {
            ((MEraEngineSysCallback*)self)->on_wait(any_key, is_force);
        },
        .on_twait = [](Erased_t* self, int64_t duration, bool is_force) noexcept {
            ((MEraEngineSysCallback*)self)->on_twait(duration, is_force);
        },
        .on_input_int = [](Erased_t* self, RustOption_int64_t default_value, bool can_click, bool allow_skip) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_input_int(from_rust_type(default_value), can_click, allow_skip);
            return to_rust_type<RustControlFlow_void_RustOption_int64_t>(r);
        },
        .on_input_str = [](Erased_t* self, slice_ref_uint8_t default_value, bool can_click, bool allow_skip) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_input_str(from_rust_opt(default_value), can_click, allow_skip);
            return to_rust_type<RustControlFlow_void_char_const_ptr_t>(r);
        },
        .on_tinput_int = [](Erased_t* self, int64_t time_limit, int64_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_tinput_int(time_limit, default_value, show_prompt, STR_VIEW(expiry_msg), can_click);
            return to_rust_type<RustControlFlow_void_RustOption_int64_t>(r);
        },
        .on_tinput_str = [](Erased_t* self, int64_t time_limit, slice_ref_uint8_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_tinput_str(time_limit, STR_VIEW(default_value), show_prompt, STR_VIEW(expiry_msg), can_click);
            return to_rust_type<RustControlFlow_void_char_const_ptr_t>(r);
        },
        .on_oneinput_int = [](Erased_t* self, RustOption_int64_t default_value) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_oneinput_int(from_rust_type(default_value));
            return to_rust_type<RustControlFlow_void_RustOption_int64_t>(r);
        },
        .on_oneinput_str = [](Erased_t* self, slice_ref_uint8_t default_value) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_oneinput_str(from_rust_type(default_value));
            return to_rust_type<RustControlFlow_void_char_const_ptr_t>(r);
        },
        .on_toneinput_int = [](Erased_t* self, int64_t time_limit, int64_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_toneinput_int(time_limit, default_value, show_prompt, STR_VIEW(expiry_msg), can_click);
            return to_rust_type<RustControlFlow_void_RustOption_int64_t>(r);
        },
        .on_toneinput_str = [](Erased_t* self, int64_t time_limit, slice_ref_uint8_t default_value, bool show_prompt, slice_ref_uint8_t expiry_msg, bool can_click) noexcept {
            auto r = ((MEraEngineSysCallback*)self)->on_toneinput_str(time_limit, STR_VIEW(default_value), show_prompt, STR_VIEW(expiry_msg), can_click);
            return to_rust_type<RustControlFlow_void_char_const_ptr_t>(r);
        },
        .on_reuselastline = [](Erased_t* self, slice_ref_uint8_t content) noexcept {
            ((MEraEngineSysCallback*)self)->on_reuselastline(STR_VIEW(content));
        },
        .on_clearline = [](Erased_t* self, int64_t count) noexcept {
            ((MEraEngineSysCallback*)self)->on_clearline(count);
        },
        .on_print_button = [](Erased_t* self, slice_ref_uint8_t content, slice_ref_uint8_t value, uint8_t flags) noexcept {
            ((MEraEngineSysCallback*)self)->on_print_button(STR_VIEW(content), STR_VIEW(value), (PrintExtendedFlags)flags);
        },
        .on_var_get_int = [](Erased_t* self, slice_ref_uint8_t name, size_t idx) noexcept {
            return rust_exception_boundary<RustResult_int64_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_var_get_int(STR_VIEW(name), idx);
            });
        },
        .on_var_get_str = [](Erased_t* self, slice_ref_uint8_t name, size_t idx) noexcept {
            return rust_exception_boundary<RustResult_char_const_ptr_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_var_get_str(STR_VIEW(name), idx);
            });
        },
        .on_var_set_int = [](Erased_t* self, slice_ref_uint8_t name, size_t idx, int64_t val) noexcept {
            return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_var_set_int(STR_VIEW(name), idx, val);
            });
        },
        .on_var_set_str = [](Erased_t* self, slice_ref_uint8_t name, size_t idx, slice_ref_uint8_t val) noexcept {
            return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_var_set_str(STR_VIEW(name), idx, STR_VIEW(val));
            });
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
            return ((MEraEngineSysCallback*)self)->on_gdrawsprite(gid, STR_VIEW(sprite_name), dest_x, dest_y, dest_width, dest_height, color_matrix);
        },
        .on_gclear = [](Erased_t* self, int64_t gid, int64_t color) noexcept {
            return ((MEraEngineSysCallback*)self)->on_gclear(gid, color);
        },
        .on_spritecreate = [](Erased_t* self, slice_ref_uint8_t name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y) noexcept {
            return ((MEraEngineSysCallback*)self)->on_spritecreate(STR_VIEW(name), gid, x, y, width, height, offset_x, offset_y);
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
        .on_spriteanimeaddframe = [](Erased_t* self, slice_ref_uint8_t name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y, int64_t delay) noexcept {
            return ((MEraEngineSysCallback*)self)->on_spriteanimeaddframe(STR_VIEW(name), gid, x, y, width, height, offset_x, offset_y, delay);
        },
        .on_spritewidth = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
            return ((MEraEngineSysCallback*)self)->on_spritewidth(STR_VIEW(name));
        },
        .on_spriteheight = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
            return ((MEraEngineSysCallback*)self)->on_spriteheight(STR_VIEW(name));
        },
        .on_open_host_file = [](Erased_t* self, slice_ref_uint8_t path, bool can_write) noexcept {
            return rust_exception_boundary<RustResult_VirtualPtr__Erased_ptr_MEraEngineHostFileFfiVTable_char_const_ptr_t>([&] {
                auto r = ((MEraEngineSysCallback*)self)->on_open_host_file(STR_VIEW(path), can_write);
                MEraEngineHostFileFfiVTable_t vtable{
                    .release_vptr = [](Erased_t* self) noexcept {
                        delete (MEraEngineHostFile*)self;
                    },
                    .read = [](Erased_t* self, slice_mut_uint8_t buf) noexcept {
                        return rust_exception_boundary<RustResult_uint64_char_const_ptr_t>([&] {
                            return ((MEraEngineHostFile*)self)->read(TO_SPAN(buf));
                        });
                    },
                    .write = [](Erased_t* self, slice_ref_uint8_t buf) noexcept {
                        return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                            return ((MEraEngineHostFile*)self)->write(TO_SPAN(buf));
                        });
                    },
                    .flush = [](Erased_t* self) noexcept {
                        return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                            return ((MEraEngineHostFile*)self)->flush();
                        });
                    },
                    .truncate = [](Erased_t* self) noexcept {
                        return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                            return ((MEraEngineHostFile*)self)->truncate();
                        });
                    },
                    .seek = [](Erased_t* self, int64_t pos, EraCompilerFileSeekMode_t mode) noexcept {
                        return rust_exception_boundary<RustResult_uint64_char_const_ptr_t>([&] {
                            return ((MEraEngineHostFile*)self)->seek(pos, (EraCompilerFileSeekMode)mode);
                        });
                    },
                };
                return VirtualPtr__Erased_ptr_MEraEngineHostFileFfiVTable_t{
                    .ptr = (Erased_t*)r.release(),
                    .vtable = vtable,
                };
            });
        },
        .on_check_host_file_exists = [](Erased_t* self, slice_ref_uint8_t path) noexcept {
            return rust_exception_boundary<RustResult_bool_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_check_host_file_exists(STR_VIEW(path));
            });
        },
        .on_delete_host_file = [](Erased_t* self, slice_ref_uint8_t path) noexcept {
            return rust_exception_boundary<RustResult_void_char_const_ptr_t>([&] {
                // TODO: Implement this
                //return ((MEraEngineSysCallback*)self)->on_delete_host_file(STR_VIEW(path));
                throw std::runtime_error("Not implemented");
            });
        },
        .on_list_host_file = [](Erased_t* self, slice_ref_uint8_t path) noexcept {
            return rust_exception_boundary<RustResult_VirtualPtr__Erased_ptr_MEraEngineHostFileListingFfiVTable_char_const_ptr_t>([&] {
                // TODO: Implement this
                //return ((MEraEngineSysCallback*)self)->on_list_host_file(STR_VIEW(path));
                throw std::runtime_error("Not implemented");
                return VirtualPtr__Erased_ptr_MEraEngineHostFileListingFfiVTable_t{};
            });
        },
        .on_play_sound = [](Erased_t* self, slice_ref_uint8_t path, int64_t loop_count, bool is_bgm) noexcept {
            return ((MEraEngineSysCallback*)self)->on_play_sound(STR_VIEW(path), loop_count, is_bgm);
        },
        .on_stop_sound = [](Erased_t* self, int64_t sound_id) noexcept {
            return ((MEraEngineSysCallback*)self)->on_stop_sound(sound_id);
        },
        .on_check_font = [](Erased_t* self, slice_ref_uint8_t font_name) noexcept {
            return ((MEraEngineSysCallback*)self)->on_check_font(STR_VIEW(font_name));
        },
        .on_get_host_time = [](Erased_t* self) noexcept {
            return ((MEraEngineSysCallback*)self)->on_get_host_time();
        },
        .on_get_config_int = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
            return rust_exception_boundary<RustResult_int64_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_get_config_int(STR_VIEW(name));
            });
        },
        .on_get_config_str = [](Erased_t* self, slice_ref_uint8_t name) noexcept {
            return rust_exception_boundary<RustResult_char_const_ptr_char_const_ptr_t>([&] {
                return ((MEraEngineSysCallback*)self)->on_get_config_str(STR_VIEW(name));
            });
        },
        .on_get_key_state = [](Erased_t* self, int64_t key_code) noexcept {
            return ((MEraEngineSysCallback*)self)->on_get_key_state(key_code);
        },
        .on_await = [](Erased_t* self, int64_t milliseconds) noexcept {
            return ((MEraEngineSysCallback*)self)->on_await(milliseconds);
        },
    };
    VirtualPtr__Erased_ptr_MEraEngineSysCallbackFfiVTable_t c{
        .ptr = (Erased_t*)callback.release(),
        .vtable = vtbl,
    };
    m_builder = mee_new_engine_builder(c);
}

MEraEngineBuilder::~MEraEngineBuilder() {
    if (!m_builder) { return; }
    mee_drop_engine_builder(m_builder);
}
MEraEngineConfig MEraEngineBuilder::get_config() const {
    return mee_engine_builder_get_config(m_builder);
}
void MEraEngineBuilder::set_config(MEraEngineConfig config) const {
    unwrap_rust(mee_engine_builder_set_config(m_builder, config));
}
void MEraEngineBuilder::load_csv(const char* filename, std::span<const uint8_t> content, EraCsvLoadKind kind) const {
    unwrap_rust(mee_engine_builder_load_csv(m_builder, filename, TO_RUST_SLICE(content), static_cast<EraCsvLoadKind_t>(kind)));
}
void MEraEngineBuilder::load_erh(const char* filename, std::span<const uint8_t> content) const {
    unwrap_rust(mee_engine_builder_load_erh(m_builder, filename, TO_RUST_SLICE(content)));
}
void MEraEngineBuilder::load_erb(const char* filename, std::span<const uint8_t> content) const {
    unwrap_rust(mee_engine_builder_load_erb(m_builder, filename, TO_RUST_SLICE(content)));
}
MEraEngineAsyncErbLoader MEraEngineBuilder::start_async_erb_loader() const {
    return MEraEngineAsyncErbLoader(mee_engine_builder_start_async_erb_loader(m_builder));
}
void MEraEngineBuilder::wait_for_async_loader() const {
    unwrap_rust(mee_engine_builder_wait_for_async_loader(m_builder));
}
void MEraEngineBuilder::finish_load_csv() const {
    unwrap_rust(mee_engine_builder_finish_load_csv(m_builder));
}
void MEraEngineBuilder::finish_load_erh() const {
    unwrap_rust(mee_engine_builder_finish_load_erh(m_builder));
}
void MEraEngineBuilder::register_variable(const char* name, bool is_string, uint32_t dimension, bool watch) const {
    unwrap_rust(mee_engine_builder_register_variable(m_builder, name, is_string, dimension, watch));
}
void MEraEngineBuilder::set_variable_int(const char* name, size_t index, int64_t value) const {
    unwrap_rust(mee_engine_builder_set_variable_int(m_builder, name, index, value));
}
void MEraEngineBuilder::set_variable_str(const char* name, size_t index, std::string_view value) const {
    unwrap_rust(mee_engine_builder_set_variable_str(m_builder, name, index, TO_RUST_STR(value)));
}
MEraEngine MEraEngineBuilder::build() {
    auto builder = std::exchange(m_builder, nullptr);
    return MEraEngine(unwrap_rust(mee_build_engine(builder)));
}

MEraEngine::~MEraEngine() {
    if (!m_engine) { return; }
    mee_drop_engine(m_engine);
}
MEraEngineConfig MEraEngine::get_config() const {
    return mee_engine_get_config(m_engine);
}
void MEraEngine::set_config(MEraEngineConfig config) const {
    unwrap_rust(mee_engine_set_config(m_engine, config));
}
EraExecutionBreakReason MEraEngine::do_execution(std::atomic_bool const& run_flag, uint64_t max_inst_cnt) const {
    // NOTE: It is safe to cast away the constness of `run_flag` because the Rust function does not modify the value
    return (EraExecutionBreakReason)unwrap_rust(mee_engine_do_execution(m_engine, (bool*)&run_flag, max_inst_cnt));
}
//bool MEraEngine::get_is_halted() {
//    std::atomic_bool run_flag = true;
//    return do_execution(&run_flag, 0) != ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS;
//}
void MEraEngine::reset_exec_to_ip(EraExecIp ip) const {
    do_rpc("reset_exec_to_ip", { { "ip", ip } });
}
std::optional<EraFuncInfo> MEraEngine::get_func_info(const char* name) const {
    auto r = do_rpc("get_func_info", { { "name", name } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraFuncInfo>();
}
std::optional<EraFuncInfo> MEraEngine::get_func_info_by_ip(EraExecIp ip) const {
    auto r = do_rpc("get_func_info_by_ip", { { "ip", ip } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraFuncInfo>();
}
std::optional<EraSourceInfo> MEraEngine::get_src_info_from_ip(EraExecIp ip) const {
    auto r = do_rpc("get_src_info_from_ip", { { "ip", ip } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraSourceInfo>();
}
std::optional<EraExecIp> MEraEngine::get_ip_from_src(std::string_view filename, SrcSpan span) const {
    auto r = do_rpc("get_ip_from_src", { { "filename", filename }, { "span", span } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraExecIp>();
}
std::optional<EraChunkInfo> MEraEngine::get_chunk_info(uint32_t idx) const {
    auto r = do_rpc("get_chunk_info", { { "idx", idx } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraChunkInfo>();
}
EraEngineStackTrace MEraEngine::get_stack_trace() const {
    return do_rpc("get_stack_trace", {}).get<EraEngineStackTrace>();
}
std::optional<EraExecIp> MEraEngine::get_current_ip() const {
    auto r = do_rpc("get_current_ip", {});
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<EraExecIp>();
}
std::optional<std::string> MEraEngine::get_file_source(std::string_view name) const {
    auto r = do_rpc("get_file_source", { { "name", name } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<std::string>();
}
std::string_view MEraEngine::get_version() {
    return STR_VIEW(mee_get_engine_version());
}
std::optional<DiagnosticResolveSrcSpanResult> MEraEngine::resolve_src_span(std::string_view filename, SrcSpan span) const {
    auto r = do_rpc("resolve_src_span", { { "filename", filename }, { "span", span } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<DiagnosticResolveSrcSpanResult>();
}
std::vector<std::string> MEraEngine::get_loaded_files_list() const {
    return do_rpc("get_loaded_files_list", {}).get<std::vector<std::string>>();
}
std::optional<std::vector<uint8_t>> MEraEngine::read_bytecode(uint32_t chunk_idx, uint32_t offset, uint32_t size) const {
    auto r = do_rpc("read_bytecode", { { "chunk_idx", chunk_idx }, { "offset", offset }, { "size", size } });
    if (r.is_null()) {
        return std::nullopt;
    }
    return r.get<std::vector<uint8_t>>();
}
void MEraEngine::patch_bytecode(uint32_t chunk_idx, uint32_t offset, std::span<const uint8_t> data) const {
    do_rpc("patch_bytecode", { { "chunk_idx", chunk_idx }, { "offset", offset }, { "data", data } });
}
EraEvaluateExprResult MEraEngine::evaluate_expr(std::string_view expr, std::optional<uint32_t> scope_idx, uint64_t offset, uint64_t count, uint64_t eval_limit) const {
    return do_rpc("evaluate_expr", { { "expr", expr }, { "scope_idx", scope_idx }, { "offset", offset }, { "count", count }, { "eval_limit", eval_limit } }).get<EraEvaluateExprResult>();
}
std::vector<std::string> MEraEngine::get_functions_list() const {
    return do_rpc("get_functions_list", {}).get<std::vector<std::string>>();
}
std::vector<EraDumpFunctionBytecodeEntry> MEraEngine::dump_function_bytecode(std::string_view name) const {
    return do_rpc("dump_function_bytecode", { { "name", name } }).get<std::vector<EraDumpFunctionBytecodeEntry>>();
}
nlohmann::json MEraEngine::dump_stack() const {
    return do_rpc("dump_stack", {});
}
nlohmann::json MEraEngine::decode_bytecode(std::span<const uint8_t> bc) const {
    return do_rpc("decode_bytecode", { { "bc", bc } });
}
void MEraEngine::goto_next_safe_ip() const {
    do_rpc("goto_next_safe_ip", {});
}
nlohmann::json MEraEngine::do_rpc(std::string_view method, nlohmann::json params) const {
    auto json = make_jsonrpc(method, std::move(params));
    rust_String response{ mee_engine_do_rpc(m_engine, json.dump().c_str()) };
    json.clear();
    return unwrap_jsonrpc(response);
}
EraEngineSnapshot MEraEngine::take_snapshot(EraEngineSnapshotKind parts_to_add) const {
    return EraEngineSnapshot{ unwrap_rust(mee_engine_take_snapshot(m_engine, parts_to_add)) };
}
void MEraEngine::restore_snapshot(std::span<const uint8_t> snapshot) const {
    unwrap_rust(mee_engine_restore_snapshot(m_engine, TO_RUST_SLICE(snapshot)));
}
