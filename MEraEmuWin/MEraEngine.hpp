// A wrapper over Rust MEraEngine

#pragma once

#include "pch.h"

#include <stdatomic.h>

#include "ffi/MEraEmuCore.rust.h"

struct rust_String {
    rust_String(Vec_uint8_t v) : i(std::move(v)) {}
    rust_String(rust_String const&) = delete;
    ~rust_String() { delete_rust_string(std::move(i)); }

    std::string to_string() {
        return { reinterpret_cast<char*>(i.ptr), i.len };
    }

    Vec_uint8_t i;
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

struct MEraEngineException : std::exception {
    MEraEngineException(rust_String msg) : m_msg(msg.to_string()) {}
    char const* what() const {
        return m_msg.c_str();
    }
private:
    std::string m_msg;
};

struct EraExecSourceInfo {
    uint32_t line, column;
};

struct EraScriptErrorInfo {
    std::string_view filename;
    EraExecSourceInfo src_info;
    bool is_error;
    std::string_view msg;
};

struct MEraEngineStackTraceFrame {
    std::string_view file_name;
    std::string_view func_name;
    EraExecIpInfo ip;
    EraExecSourceInfo src_info;
};
struct MEraEngineStackTrace {
    std::vector<MEraEngineStackTraceFrame> frames;
};

struct MEraEngineHostFile {
    MEraEngineHostFile() {}
    virtual ~MEraEngineHostFile() {}

    virtual uint64_t read(std::span<uint8_t> buf) = 0;
    virtual void write(std::span<const uint8_t> buf) = 0;
    virtual void flush() = 0;
    virtual void truncate() = 0;
    virtual void seek(int64_t pos, MEraEngineFileSeekMode mode) = 0;
    virtual uint64_t tell() = 0;
};

struct MEraEngineSysCallback {
    MEraEngineSysCallback() {}
    virtual ~MEraEngineSysCallback() {}

    virtual void on_compile_error(EraScriptErrorInfo const& info) = 0;
    virtual void on_execute_error(EraScriptErrorInfo const& info) = 0;
    virtual uint64_t on_get_rand() = 0;
    virtual void on_print(std::string_view content, PrintExtendedFlags flags) = 0;
    virtual void on_html_print(std::string_view content) = 0;
    virtual void on_wait(bool any_key, bool is_force) = 0;
    virtual void on_twait(int64_t duration, bool is_force) = 0;
    virtual std::optional<int64_t> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) = 0;
    virtual const char* on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) = 0;
    virtual std::optional<int64_t> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual const char* on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual std::optional<int64_t> on_oneinput_int(std::optional<int64_t> default_value) = 0;
    virtual const char* on_oneinput_str(std::optional<std::string_view> default_value) = 0;
    virtual std::optional<int64_t> on_toneinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual const char* on_toneinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) = 0;
    virtual void on_reuselastline(std::string_view content) = 0;
    virtual void on_clearline(int64_t count) = 0;
    // NOTE: Can throw std::exception
    virtual int64_t on_var_get_int(std::string_view name, uint32_t idx) = 0;
    // NOTE: Can throw std::exception
    virtual const char* on_var_get_str(std::string_view name, uint32_t idx) = 0;
    // NOTE: Can throw std::exception
    virtual void on_var_set_int(std::string_view name, uint32_t idx, int64_t val) = 0;
    // NOTE: Can throw std::exception
    virtual void on_var_set_str(std::string_view name, uint32_t idx, std::string_view val) = 0;
    virtual void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) = 0;
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
    virtual std::unique_ptr<MEraEngineHostFile> on_open_host_file(std::string_view path, bool can_write) = 0;
    virtual bool on_check_host_file_exists(std::string_view path) = 0;
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
    MEraEngine();
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
    operator bool() noexcept {
        return m_engine;
    }

    void install_sys_callback(std::unique_ptr<MEraEngineSysCallback> callback);
    void load_csv(const char* filename, std::span<const uint8_t> content, EraCsvLoadKind kind);
    void load_erh(const char* filename, std::span<const uint8_t> content);
    void load_erb(const char* filename, std::span<const uint8_t> content);
    void register_global_var(const char* name, bool is_string, uint32_t dimension, bool watch);
    void finialize_load_srcs();
    bool do_execution(_Atomic(bool)* stop_flag, uint64_t max_inst_cnt);
    bool get_is_halted();
    void reset_exec_to_ip(EraExecIpInfo ip);
    EraFuncInfo get_func_info(const char* name);
    MEraEngineStackTrace get_stack_trace();
    static std::string_view get_version();

private:
    MEraEngineInterop* m_engine;
};
