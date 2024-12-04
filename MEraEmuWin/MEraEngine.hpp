// A wrapper over Rust MEraEngine

#pragma once

#include "pch.h"

#include <nlohmann/json.hpp>
#include <stdatomic.h>
#include <optional>
#include <variant>
#include <span>

#include "ffi/MEraEmuCore.rust.h"

struct rust_String {
    rust_String(Vec_uint8_t v) : i(v) {}
    rust_String(rust_String const&) = delete;
    ~rust_String() { rust_drop_string(i); }

    operator std::string_view() {
        return { reinterpret_cast<char*>(i.ptr), i.len };
    }

    std::string to_string() {
        return { reinterpret_cast<char*>(i.ptr), i.len };
    }

    Vec_uint8_t i;
};

static const struct break_tag_t {} break_tag;
static const struct continue_tag_t {} continue_tag;

template<typename B, typename C>
struct ControlFlow {
    ControlFlow(break_tag_t, B value) : value(std::move(value)) {}
    ControlFlow(continue_tag_t, C value) : value(std::move(value)) {}

    bool is_break() const { return std::holds_alternative<B>(value); }
    bool is_continue() const { return std::holds_alternative<C>(value); }

    B const* as_break() const {
        return std::get_if<B>(&value);
    }
    B* as_break() {
        return std::get_if<B>(&value);
    }

    C const* as_continue() const {
        return std::get_if<C>(&value);
    }
    C* as_continue() {
        return std::get_if<C>(&value);
    }

    std::variant<B, C> value;
};

struct MEraEngineException : std::runtime_error {
    using std::runtime_error::runtime_error;
    MEraEngineException(rust_String msg) : MEraEngineException(msg.to_string()) {}
};

#define ERA_PEF_IS_SINGLE ((PrintExtendedFlags)0x1)
#define ERA_PEF_USE_KANA ((PrintExtendedFlags)0x2)
#define ERA_PEF_IGNORE_COLOR ((PrintExtendedFlags)0x4)
#define ERA_PEF_IS_LINE ((PrintExtendedFlags)0x8)
#define ERA_PEF_IS_WAIT ((PrintExtendedFlags)0x10)
#define ERA_PEF_FORCE_PLAIN ((PrintExtendedFlags)0x20)
#define ERA_PEF_LEFT_PAD ((PrintExtendedFlags)0x40)
#define ERA_PEF_RIGHT_PAD ((PrintExtendedFlags)0x80)
typedef uint8_t PrintExtendedFlags;

struct EraDiagnosticEntry {
    DiagnosticLevel level;
    std::string_view filename;
    SrcSpan span;
    std::string_view message;
};

struct EraExecIp {
    uint32_t chunk, offset;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraExecIp, chunk, offset);

struct EraFuncExecFrame {
    uint32_t stack_start;
    EraExecIp ip, ret_ip;
    bool ignore_return_value, is_transient;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraFuncExecFrame, stack_start, ip, ret_ip, ignore_return_value, is_transient);

struct EraEngineStackTrace {
    std::vector<EraFuncExecFrame> frames;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraEngineStackTrace, frames);

struct ScalarValue_Void {};
struct ScalarValue_Empty {};

struct ScalarValue : std::variant<ScalarValue_Empty, ScalarValue_Void, int64_t, std::string> {
    using std::variant<ScalarValue_Empty, ScalarValue_Void, int64_t, std::string>::variant;

    bool is_empty() const { return std::holds_alternative<ScalarValue_Empty>(*this); }
    bool is_void() const { return std::holds_alternative<ScalarValue_Void>(*this); }
    bool is_int() const { return std::holds_alternative<int64_t>(*this); }
    bool is_str() const { return std::holds_alternative<std::string>(*this); }

    int64_t const* as_int() const {
        return std::get_if<int64_t>(this);
    }
    int64_t* as_int() {
        return std::get_if<int64_t>(this);
    }

    std::string const* as_str() const {
        return std::get_if<std::string>(this);
    }
    std::string* as_str() {
        return std::get_if<std::string>(this);
    }
};
inline void to_json(nlohmann::json& j, ScalarValue const& v) {
    if (v.is_empty()) {
        j = "Empty";
    }
    else if (v.is_void()) {
        j = "Void";
    }
    else if (v.is_int()) {
        j = { { "Int", *v.as_int() } };
    }
    else if (v.is_str()) {
        j = { { "Str", *v.as_str() } };
    }
}
inline void from_json(nlohmann::json const& j, ScalarValue& v) {
    if (j.is_string()) {
        if (j == "Empty") {
            v = ScalarValue_Empty{};
        }
        else if (j == "Void") {
            v = ScalarValue_Void{};
        }
    }
    else if (j.is_object()) {
        if (j.contains("Int")) {
            v = j["Int"].get<int64_t>();
        }
        else if (j.contains("Str")) {
            v = j["Str"].get<std::string>();
        }
    }
}

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(SrcSpan, start, len);
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(SrcLoc, line, col);

NLOHMANN_JSON_SERIALIZE_ENUM(ValueKind, {
    {VALUE_KIND_INT, "Int"},
    {VALUE_KIND_STR, "Str"},
    {VALUE_KIND_ARR_INT, "ArrInt"},
    {VALUE_KIND_ARR_STR, "ArrStr"},
    });
NLOHMANN_JSON_SERIALIZE_ENUM(ScalarValueKind, {
    {SCALAR_VALUE_KIND_INT, "Int"},
    {SCALAR_VALUE_KIND_STR, "Str"},
    {SCALAR_VALUE_KIND_VOID, "Void"},
    {SCALAR_VALUE_KIND_EMPTY, "Empty"},
    });

struct EraFuncFrameArgInfo {
    ScalarValueKind var_kind;
    uint8_t dims_cnt;
    ScalarValue default_value;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraFuncFrameArgInfo, var_kind, dims_cnt, default_value);

struct EraFuncFrameVarInfo {
    std::string name;
    SrcSpan span;
    bool is_ref, is_const, is_charadata, in_local_frame;
    uint32_t var_idx;
    ScalarValueKind var_kind;
    uint8_t dims_cnt;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraFuncFrameVarInfo, name, span, is_ref, is_const, is_charadata, in_local_frame, var_idx, var_kind, dims_cnt);

struct EraFuncFrameInfo {
    std::vector<EraFuncFrameVarInfo> vars;
    std::vector<EraFuncFrameArgInfo> args;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraFuncFrameInfo, vars, args);

struct EraFuncInfo {
    std::string name;
    SrcSpan name_span;
    EraFuncFrameInfo frame_info;
    uint32_t chunk_idx, bc_offset, bc_size;
    ScalarValueKind ret_kind;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraFuncInfo, name, name_span, frame_info, chunk_idx, bc_offset, bc_size, ret_kind);

struct EraSourceInfo {
    std::string filename;
    SrcSpan span;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraSourceInfo, filename, span);

struct EraChunkInfo {
    std::string name;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(EraChunkInfo, name);

struct DiagnosticResolveSrcSpanResult {
    std::string snippet;
    uint32_t offset;
    SrcLoc loc;
    uint32_t len;
};
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(DiagnosticResolveSrcSpanResult, snippet, offset, loc, len);

struct EraDiagnosticProvider {
    EraDiagnosticProvider(DiagnosticProviderFfi const& provider) : m_provider(provider) {}

    std::optional<EraDiagnosticEntry> get_entry(uint64_t idx) const;
    uint64_t get_entry_count() const;
    std::optional<DiagnosticResolveSrcSpanResult> resolve_src_span(std::string_view filename, SrcSpan span) const;

private:
    DiagnosticProviderFfi m_provider;
};

struct MEraEngineHostFile {
    MEraEngineHostFile() {}
    virtual ~MEraEngineHostFile() {}

    virtual uint64_t read(std::span<uint8_t> buf) = 0;
    virtual void write(std::span<const uint8_t> buf) = 0;
    virtual void flush() = 0;
    virtual void truncate() = 0;
    virtual uint64_t seek(int64_t pos, EraCompilerFileSeekMode mode) = 0;
};

struct MEraEngineSysCallback {
    MEraEngineSysCallback() {}
    virtual ~MEraEngineSysCallback() {}

    virtual void on_error(EraDiagnosticProvider const& provider) = 0;
    virtual uint64_t on_get_rand() = 0;
    virtual void on_print(std::string_view content, PrintExtendedFlags flags) = 0;
    virtual void on_html_print(std::string_view content) = 0;
    virtual void on_wait(bool any_key, bool is_force) = 0;
    virtual void on_twait(int64_t duration, bool is_force) = 0;
    virtual ControlFlow<std::monostate, std::optional<int64_t>> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) = 0;
    virtual ControlFlow<std::monostate, const char*> on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) = 0;
    virtual ControlFlow<std::monostate, std::optional<int64_t>> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual ControlFlow<std::monostate, const char*> on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual ControlFlow<std::monostate, std::optional<int64_t>> on_oneinput_int(std::optional<int64_t> default_value) = 0;
    virtual ControlFlow<std::monostate, const char*> on_oneinput_str(std::optional<std::string_view> default_value) = 0;
    virtual ControlFlow<std::monostate, std::optional<int64_t>> on_toneinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual ControlFlow<std::monostate, const char*> on_toneinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual void on_reuselastline(std::string_view content) = 0;
    virtual void on_clearline(int64_t count) = 0;
    virtual void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) = 0;
    // NOTE: Can throw std::exception
    virtual int64_t on_var_get_int(std::string_view name, size_t idx) = 0;
    // NOTE: Can throw std::exception
    virtual const char* on_var_get_str(std::string_view name, size_t idx) = 0;
    // NOTE: Can throw std::exception
    virtual void on_var_set_int(std::string_view name, size_t idx, int64_t val) = 0;
    // NOTE: Can throw std::exception
    virtual void on_var_set_str(std::string_view name, size_t idx, std::string_view val) = 0;
    virtual int64_t on_gcreate(int64_t gid, int64_t width, int64_t height) = 0;
    virtual int64_t on_gcreatefromfile(int64_t gid, std::string_view path) = 0;
    virtual int64_t on_gdispose(int64_t gid) = 0;
    virtual int64_t on_gcreated(int64_t gid) = 0;
    virtual int64_t on_gdrawsprite(int64_t gid, std::string_view sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const& color_matrix) = 0;
    virtual int64_t on_gclear(int64_t gid, int64_t color) = 0;
    virtual int64_t on_spritecreate(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height) = 0;
    virtual int64_t on_spritedispose(std::string_view name) = 0;
    virtual int64_t on_spritecreated(std::string_view name) = 0;
    virtual int64_t on_spriteanimecreate(std::string_view name, int64_t width, int64_t height) = 0;
    virtual int64_t on_spriteanimeaddframe(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y, int64_t delay) = 0;
    virtual int64_t on_spritewidth(std::string_view name) = 0;
    virtual int64_t on_spriteheight(std::string_view name) = 0;
    // NOTE: Can throw std::exception
    virtual std::unique_ptr<MEraEngineHostFile> on_open_host_file(std::string_view path, bool can_write) = 0;
    // NOTE: Can throw std::exception
    virtual bool on_check_host_file_exists(std::string_view path) = 0;
    //// NOTE: Can throw std::exception
    //virtual void on_delete_host_file(std::string_view path) = 0;
    //// NOTE: Can throw std::exception
    //virtual std::unique_ptr<MEraEngineHostFileListing> on_list_host_file(std::string_view path) = 0;
    // TODO: FFI callbacks: on_delete_host_file, on_list_host_file
    virtual int64_t on_check_font(std::string_view font_name) = 0;
    virtual uint64_t on_get_host_time() = 0;
    // NOTE: Can throw std::exception
    virtual int64_t on_get_config_int(std::string_view name) = 0;
    // NOTE: Can throw std::exception
    virtual const char* on_get_config_str(std::string_view name) = 0;
    virtual int64_t on_get_key_state(int64_t key_code) = 0;
};

struct MEraEngine {
    ~MEraEngine();
    MEraEngine(std::nullptr_t) noexcept : m_engine() {}

    MEraEngine(MEraEngine const& other) = delete;
    MEraEngine(MEraEngine&& other) noexcept : MEraEngine(nullptr) {
        std::swap(m_engine, other.m_engine);
    }
    MEraEngine& operator=(MEraEngine other) noexcept {
        std::swap(m_engine, other.m_engine);
        return *this;
    }
    operator bool() const noexcept {
        return m_engine;
    }

    EraExecutionBreakReason do_execution(std::atomic_bool const& run_flag, uint64_t max_inst_cnt) const;
    //bool get_is_halted();
    void reset_exec_to_ip(EraExecIp ip) const;
    std::optional<EraFuncInfo> get_func_info(const char* name) const;
    std::optional<EraFuncInfo> get_func_info_by_ip(EraExecIp ip) const;
    std::optional<EraSourceInfo> get_src_info_from_ip(EraExecIp ip) const;
    std::optional<EraChunkInfo> get_chunk_info(uint32_t idx) const;
    EraEngineStackTrace get_stack_trace() const;
    std::optional<std::string> get_file_source(std::string_view name) const;
    static std::string_view get_version();
    std::optional<DiagnosticResolveSrcSpanResult> resolve_src_span(std::string_view filename, SrcSpan span) const;
    std::vector<std::string> get_loaded_files_list() const;
    std::optional<std::vector<uint8_t>> read_bytecode(uint32_t chunk_idx, uint32_t offset, uint32_t size) const;
    void patch_bytecode(uint32_t chunk_idx, uint32_t offset, std::span<const uint8_t> data) const;

    nlohmann::json do_rpc(std::string_view method, nlohmann::json params) const;

private:
    friend struct MEraEngineBuilder;

    MEraEngine(MEraEngineFfi* engine) : m_engine(engine) {}

    MEraEngineFfi* m_engine;
};

struct MEraEngineBuilder {
    MEraEngineBuilder(std::unique_ptr<MEraEngineSysCallback> callback);
    ~MEraEngineBuilder();
    MEraEngineBuilder(std::nullptr_t) noexcept : m_builder() {}

    MEraEngineBuilder(MEraEngineBuilder const& other) = delete;
    MEraEngineBuilder(MEraEngineBuilder&& other) noexcept : MEraEngineBuilder(nullptr) {
        std::swap(m_builder, other.m_builder);
    }
    MEraEngineBuilder& operator=(MEraEngineBuilder other) noexcept {
        std::swap(m_builder, other.m_builder);
        return *this;
    }
    operator bool() noexcept {
        return m_builder;
    }

    void load_csv(const char* filename, std::span<const uint8_t> content, EraCsvLoadKind kind);
    void load_erh(const char* filename, std::span<const uint8_t> content);
    void load_erb(const char* filename, std::span<const uint8_t> content);
    void finish_load_csv();
    void finish_load_erh();
    void register_variable(const char* name, bool is_string, uint32_t dimension, bool watch);
    MEraEngine build();

private:
    MEraEngineBuilderFfi* m_builder;
};

/// <summary>
/// Checks if the given execution break reason is fatal (i.e. no further progress can be made without
/// user intervention, such as modifying bytecode).
/// </summary>
/// <param name="reason"></param>
/// <returns></returns>
inline bool is_execution_break_reason_fatal(EraExecutionBreakReason reason) {
    switch (reason) {
    case ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS:
    case ERA_EXECUTION_BREAK_REASON_CALLBACK_BREAK:
    case ERA_EXECUTION_BREAK_REASON_STOP_FLAG:
        return false;
    default:
        return true;
    }
}
