#include "pch.h"
#include "EngineControl.h"
#if __has_include("EngineControl.g.cpp")
#include "EngineControl.g.cpp"
#endif

#include <random>
#include <fstream>
#include <filesystem>
#include <variant>
#include <winrt/Tenkai.UI.ViewManagement.h>

#include "EngineUnhandledExceptionEventArgs.g.cpp"
#include "DevTools/MainPage.h"
#include "MEraEngine.hpp"
#include "Tenkai.hpp"
#include "util.hpp"

using namespace winrt;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Media::Imaging;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Core;
using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::System::Threading;

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

std::string to_string(DiagnosticLevel value) {
    switch (value) {
    case DIAGNOSTIC_LEVEL_ERROR: return "错误";
    case DIAGNOSTIC_LEVEL_WARNING: return "警告";
    case DIAGNOSTIC_LEVEL_NOTE: return "提示";
    case DIAGNOSTIC_LEVEL_HELP: return "帮助";
    case DIAGNOSTIC_LEVEL_SUGGESTION: return "建议";
    default: return "<未知>";
    }
}

hstring to_hstring(DiagnosticLevel value) {
    return to_hstring(to_string(value));
}

Windows::UI::Color color_from_diagnostic_level(DiagnosticLevel level) {
    switch (level) {
    case DIAGNOSTIC_LEVEL_ERROR: return Colors::Red();
    case DIAGNOSTIC_LEVEL_WARNING: return Colors::Orange();
    case DIAGNOSTIC_LEVEL_NOTE: return Colors::Blue();
    case DIAGNOSTIC_LEVEL_HELP: return Colors::Green();
    case DIAGNOSTIC_LEVEL_SUGGESTION: return Colors::Purple();
    default: return Colors::Black();
    }
}

namespace winrt::MEraEmuWin::implementation {
    auto recur_dir_iter(hstring const& base, hstring const& dir) {
        return std::filesystem::recursive_directory_iterator((base + dir).c_str());
    }
    template<typename T>
    bool checked_add(T a, T b, T& out) noexcept {
        constexpr auto MAX = std::numeric_limits<T>::max();
        constexpr auto MIN = std::numeric_limits<T>::min();
        if (a > 0 && b > 0) {
            if (a > MAX - b) { return false; }
        }
        if (a < 0 && b < 0) {
            if (a < MIN - b) { return false; }
        }
        out = a + b;
        return true;
    }
    template<typename T>
    bool checked_mul(T a, T b, T& out) noexcept {
        constexpr auto MAX = std::numeric_limits<T>::max();
        constexpr auto MIN = std::numeric_limits<T>::min();
        if (a == 0 || b == 0) {
            out = 0;
            return true;
        }
        if ((a > 0 && b > 0) || (a < 0 && b < 0)) {
            if (a > MAX / b) { return false; }
        }
        else {
            if (a > b) { std::swap(a, b); }
            if (a < MIN / b) { return false; }
        }
        out = a * b;
        return true;
    }
    template<typename T>
    std::optional<T> checked_add(T a, T b) noexcept {
        T out;
        if (!checked_add(a, b, out)) { return std::nullopt; }
        return out;
    }
    template<typename T>
    std::optional<T> checked_mul(T a, T b) noexcept {
        T out;
        if (!checked_mul(a, b, out)) { return std::nullopt; }
        return out;
    }
    template<typename T>
    std::optional<T> parse(std::wstring_view str) noexcept {
        if (str.empty()) { return std::nullopt; }
        std::wstring_view::size_type n = str.size();
        std::wstring_view::size_type i;
        T r{};
        bool is_negative{};
        for (i = 0; i < n; i++) {
            auto ch = str[i];
            if (i == 0 && ch == L'-') {
                is_negative = true;
                if (str.size() == 1) { return std::nullopt; }
                continue;
            }
            if (!(L'0' <= ch && ch <= L'9')) {
                return std::nullopt;
            }
            if (!checked_mul<T>(r, 10, r)) { return std::nullopt; }
            if (!checked_add<T>(r, ch - L'0', r)) { return std::nullopt; }
        }
        if (i != n) { return std::nullopt; }
        if (is_negative) {
            return checked_mul<T>(r, -1);
        }
        else {
            return r;
        }
    }

    enum class FileBomKind {
        None,
        Utf8,
        Utf16LE,
        Utf16BE,
    };
    struct FileBomDetectResult {
        FileBomKind kind;
        uint32_t read_size;
        uint32_t bom_size;
    };
    FileBomDetectResult consume_detect_file_bom(HANDLE hFile, uint8_t buf[4]) {
        DWORD read_size;
        check_bool(ReadFile(hFile, buf, 3, &read_size, nullptr));
        if (read_size >= 3) {
            if (buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf) {
                return { FileBomKind::Utf8, read_size, 3 };
            }
        }
        if (read_size >= 2) {
            if (buf[0] == 0xff && buf[1] == 0xfe) {
                return { FileBomKind::Utf16LE, read_size, 2 };
            }
            if (buf[0] == 0xfe && buf[1] == 0xff) {
                return { FileBomKind::Utf16BE, read_size, 2 };
            }
        }
        return { FileBomKind::None, read_size, 0 };
    }
    std::pair<std::unique_ptr<uint8_t[]>, size_t> read_utf8_file(std::filesystem::path const& path) {
        winrt::file_handle file{ CreateFileW(path.c_str(),
            GENERIC_READ,
            FILE_SHARE_READ,
            nullptr,
            OPEN_EXISTING,
            0,
            nullptr
        ) };
        try { check_bool(static_cast<bool>(file)); }
        catch (hresult_error const& e) {
            throw hresult_error(e.code(), format(L"Failed to open `{}`: {}", path.c_str(), e.message()));
        }
        DWORD high_size{};
        auto size = GetFileSize(file.get(), &high_size);
        if (high_size != 0) {
            throw hresult_error(E_FAIL, L"file is too large");
        }
        auto buf = new uint8_t[size + 1];
        tenkai::cpp_utils::ScopeExit se_buf([&] {
            delete[] buf;
        });
        DWORD read_size{};
        size_t buf_offset{};
        FileBomKind bom_kind;
        {
            auto [temp_bom_kind, read_size, bom_size] = consume_detect_file_bom(file.get(), buf);
            auto temp_payload_size = read_size - bom_size;
            memcpy(buf, buf + bom_size, temp_payload_size);
            buf_offset += temp_payload_size;
            size -= read_size;
            bom_kind = temp_bom_kind;
        }
        auto read_rest_fn = [&] {
            check_bool(ReadFile(file.get(), buf + buf_offset, size, &read_size, nullptr));
            if (read_size != size) {
                throw hresult_error(E_FAIL, L"Failed to read the whole file");
            }
            buf_offset += size;
            size = 0;
        };
        if (bom_kind == FileBomKind::Utf8 || bom_kind == FileBomKind::None) {
            // Read directly
            read_rest_fn();
        }
        else if (bom_kind == FileBomKind::Utf16LE) {
            // Convert to UTF-8
            //OutputDebugStringW(winrt::format(L"Converting UTF-16 LE file `{}` to UTF-8...\n", path.c_str()).c_str());
            read_rest_fn();
            auto utf16_src = std::wstring_view((wchar_t*)buf, buf_offset / sizeof(wchar_t));
            auto utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_src.data(), static_cast<int32_t>(utf16_src.size()), nullptr, 0, nullptr, nullptr);
            if (utf8_size == 0) {
                throw hresult_error(E_FAIL, L"Failed to convert UTF-16 LE to UTF-8");
            }
            auto utf8_buf = std::unique_ptr<uint8_t[]>(new uint8_t[utf8_size + 1]);
            auto final_utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_src.data(), static_cast<int32_t>(utf16_src.size()), (char*)utf8_buf.get(), utf8_size, nullptr, nullptr);
            if (final_utf8_size != utf8_size) {
                throw hresult_error(E_FAIL, L"Failed to convert UTF-16 LE to UTF-8");
            }
            delete[] std::exchange(buf, utf8_buf.release());
            buf_offset = final_utf8_size;
        }
        else if (bom_kind == FileBomKind::Utf16BE) {
            throw hresult_error(E_FAIL, L"UTF-16 BE is not supported");
        }
        else {
            throw hresult_error(E_FAIL, L"Unknown BOM kind");
        }

        se_buf.release();
        return { std::unique_ptr<uint8_t[]>{ buf }, buf_offset };
    }
    constexpr uint32_t to_u32(winrt::Windows::UI::Color value) noexcept {
        uint32_t a = value.A, r = value.R, g = value.G, b = value.B;
        return (a << 24) + (r << 16) + (g << 8) + (b << 0);
    }
    constexpr winrt::Windows::UI::Color to_winrt_color(uint32_t value) noexcept {
        return {
            .A = static_cast<uint8_t>(value >> 24),
            .R = static_cast<uint8_t>(value >> 16),
            .G = static_cast<uint8_t>(value >> 8),
            .B = static_cast<uint8_t>(value >> 0),
        };
    }
    constexpr wchar_t ascii_tolower(wchar_t ch) noexcept {
        if (L'A' <= ch && ch <= L'Z') {
            return ch - 'A' + 'a';
        }
        return ch;
    }
    constexpr bool istarts_with(std::wstring_view str, std::wstring_view pat) noexcept {
        if (size(str) < size(pat)) { return false; }
        std::wstring_view::size_type j{};
        for (std::wstring_view::size_type i{}; i != size(pat); i++) {
            if (ascii_tolower(str[i]) != ascii_tolower(pat[i])) {
                return false;
            }
        }
        return true;
    }
    constexpr bool iends_with(std::wstring_view str, std::wstring_view pat) noexcept {
        if (size(str) < size(pat)) { return false; }
        std::wstring_view::size_type j{};
        for (auto i = size(str) - size(pat); i != size(str); i++, j++) {
            if (ascii_tolower(str[i]) != ascii_tolower(pat[j])) {
                return false;
            }
        }
        return true;
    }
    constexpr bool ieq(std::wstring_view a, std::wstring_view b) noexcept {
        if (size(a) != size(b)) { return false; }
        for (std::wstring_view::size_type i{}; i != size(a); i++) {
            if (ascii_tolower(a[i]) != ascii_tolower(b[i])) {
                return false;
            }
        }
        return true;
    }
}

namespace winrt::MEraEmuWin::implementation {
    // NOTE: If InputRequest is dropped before fulfilled, it will be treated as interrupted.
    //       To convey the skip operation, call `try_fulfill_void` instead.
    struct InputRequest {
        //InputRequest(int64_t time_limit, bool show_time_prompt, hstring expiry_msg, bool is_one, bool can_skip) : {}
        InputRequest(int64_t& time_limit) : time_limit(time_limit) {}
        virtual ~InputRequest() {}

        // NOTE: For derived requests, hold a std::promise so that engine
        //       can receive the result or teardown notification.

        // Shared parameters
        // NOTE: This is also used by UI to indicate skipped ticks
        int64_t& time_limit; // Negatives stand for no limit
        bool show_time_prompt{ false };
        hstring expiry_msg{};
        bool show_expiry_msg{ false };
        bool is_one{ false };
        bool can_skip{ false };
        bool break_user_skip{ false };
        bool can_click{ false };

        //virtual void time_tick() = 0;
        virtual bool try_fulfill(hstring const& input) = 0;
        // NOTE: Recommended to override this for proper semantics (don't handle skip as empty input!)
        virtual bool try_fulfill_void() {
            return try_fulfill({});
        }
    };
    struct InputRequestI : InputRequest {
        std::optional<int64_t> default_value;
        std::promise<int64_t> promise;

        using InputRequest::InputRequest;

        ~InputRequestI() {
            /*if (default_value) {
                promise.set_value(*default_value);
            }*/
        }
        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                default_value = std::nullopt;
                return true;
            }
            if (auto r = parse<int64_t>(input)) {
                promise.set_value(*r);
                default_value = std::nullopt;
                return true;
            }
            return false;
        }
    };
    struct InputRequestS : InputRequest {
        std::optional<hstring> default_value;
        std::promise<hstring> promise;

        using InputRequest::InputRequest;

        ~InputRequestS() {
            /*if (default_value) {
                promise.set_value(*default_value);
            }*/
        }
        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                default_value = std::nullopt;
                return true;
            }
            promise.set_value(input);
            default_value = std::nullopt;
            return true;
        }
    };
    // Input request which swallows any input; can be used for merely waiting
    struct InputRequestVoid : InputRequest {
        std::promise<void> promise{};

        using InputRequest::InputRequest;

        ~InputRequestVoid() {
            // TODO: Don't fulfill if interrupted, use ControlFlow::Break(())
            promise.set_value();
        }
        bool try_fulfill(hstring const& input) override {
            return true;
        }
    };

    struct EngineThreadTask {
        EngineThreadTask(EngineThreadTaskKind kind) : kind(kind) {}
        EngineThreadTask(std::move_only_function<void()> f, bool clear_halt = false) :
            kind(clear_halt ? EngineThreadTaskKind::CustomFuncAndClearHaltState : EngineThreadTaskKind::CustomFunc),
            f(std::move(f)) {
        }
        //virtual ~EngineThreadTask() {}

        EngineThreadTaskKind kind;
        std::move_only_function<void()> f;
    };

    struct EngineSharedData : std::enable_shared_from_this<EngineSharedData> {
        EngineSharedData() {}
        ~EngineSharedData() {}

        hstring game_base_dir;
        //CoreDispatcher ui_dispatcher{ nullptr };
        DispatcherQueue ui_dispatcher_queue{ nullptr };
        //weak_ref<EngineControl> ui_ctrl{ nullptr };
        EngineControl* ui_ctrl{};
        IAsyncAction thread_task_op{ nullptr };
        std::atomic_bool ui_is_alive{ false };
        std::atomic_bool ui_redraw_block_engine{ false };
        std::atomic_bool ui_queue_work_debounce{ false };
        std::atomic_bool thread_started{ false };
        std::atomic_bool thread_is_alive{ false };
        std::exception_ptr thread_exception{ nullptr };
        bool has_execution_error{ false };
        MEraEngine engine{ nullptr };
    };

    struct MEraEmuWinEngineSysCallback : ::MEraEngineSysCallback {
        // SAFETY: Engine is destructed before EngineSharedData
        MEraEmuWinEngineSysCallback(EngineSharedData* sd, util::sync::spsc::Sender<std::move_only_function<void()>> const* ui_task_tx) :
            m_sd(sd), m_ui_task_tx(ui_task_tx) {
        }

        void on_error(EraDiagnosticProvider const& provider) override {
            auto count = provider.get_entry_count();
            for (uint64_t i = 0; i < count; i++) {
                auto entry = std::move(provider.get_entry(i).value());
                auto level = entry.level;
                auto filename = to_hstring(entry.filename);
                auto span = entry.span;
                auto message = to_hstring(entry.message);
                auto msg_clr = color_from_diagnostic_level(level);
                if (level == DIAGNOSTIC_LEVEL_ERROR) {
                    m_sd->has_execution_error = true;
                }
                if (auto resolved_opt = provider.resolve_src_span(entry.filename, span)) {
                    auto& resolved = *resolved_opt;
                    // Convert 0-based to 1-based
                    resolved.loc.col += 1;

                    // TODO: Also print code snippet
                    auto final_msg = format(L"{}({},{}): {}: {}\nSnippet: {}",
                        filename,
                        resolved.loc.line, resolved.loc.col,
                        to_hstring(level),
                        message,
                        to_hstring(resolved.snippet)
                    );
                    queue_ui_work([=, sd = m_sd] {
                        auto old_clr = sd->ui_ctrl->EngineForeColor();
                        sd->ui_ctrl->EngineForeColor(msg_clr);
                        sd->ui_ctrl->RoutinePrintSourceButton(
                            final_msg, filename,
                            resolved.loc.line, resolved.loc.col,
                            ERA_PEF_IS_LINE
                        );
                        sd->ui_ctrl->EngineForeColor(old_clr);
                    });
                }
                else {
                    auto final_msg = format(L"{}(<failed to resolve SrcSpan({}, {})>): {}: {}",
                        filename,
                        span.start, span.len,
                        to_hstring(level),
                        message
                    );
                    queue_ui_work([=, sd = m_sd] {
                        auto old_clr = sd->ui_ctrl->EngineForeColor();
                        sd->ui_ctrl->EngineForeColor(msg_clr);
                        sd->ui_ctrl->RoutinePrintSourceButton(
                            final_msg, filename,
                            1, 1,
                            ERA_PEF_IS_LINE
                        );
                        sd->ui_ctrl->EngineForeColor(old_clr);
                    });
                }
            }
        }
        uint64_t on_get_rand() override {
            return m_rand_gen();
        }
        void on_print(std::string_view content, PrintExtendedFlags flags) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content), flags] {
                sd->ui_ctrl->RoutinePrint(content, flags);
            });
            if (flags & ERA_PEF_IS_WAIT) {
                on_wait(true, false);
            }
        }
        void on_html_print(std::string_view content) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutineHtmlPrint(content);
            });
        }
        void on_wait(bool any_key, bool is_force) override {
            int64_t time_limit{ -1 };
            auto input_req = std::make_unique<InputRequestVoid>(time_limit);
            input_req->can_skip = true;
            input_req->break_user_skip = is_force;
            input_req->can_click = true;
            auto future = input_req->promise.get_future();
            queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                sd->ui_ctrl->RoutineInput(std::move(input_req));
            });
            return future.get();
        }
        void on_twait(int64_t duration, bool is_force) override {
            if (duration > 0) {
                if (duration <= m_tick_compensation) {
                    m_tick_compensation -= duration;
                    duration = 0;
                }
                else {
                    duration -= m_tick_compensation;
                    m_tick_compensation = 0;
                }
            }
            auto input_req = std::make_unique<InputRequestVoid>(duration);
            tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                if (duration < 0) {
                    m_tick_compensation = -duration;
                }
            });
            input_req->can_skip = !is_force;
            input_req->break_user_skip = true;
            auto future = input_req->promise.get_future();
            queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                sd->ui_ctrl->RoutineInput(std::move(input_req));
            });
            return future.get();
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestI>(time_limit);
                input_req->default_value = default_value;
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return { continue_tag, future.get() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, const char*> on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestS>(time_limit);
                input_req->default_value = default_value.transform([](auto x) { return to_hstring(x); });
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return { continue_tag, m_str_cache.c_str() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            try {
                if (time_limit > 0) {
                    if (time_limit <= m_tick_compensation) {
                        m_tick_compensation -= time_limit;
                        time_limit = 0;
                    }
                    else {
                        time_limit -= m_tick_compensation;
                        m_tick_compensation = 0;
                    }
                }
                auto input_req = std::make_unique<InputRequestI>(time_limit);
                tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                    if (time_limit < 0) {
                        m_tick_compensation = -time_limit;
                    }
                });
                input_req->default_value = default_value;
                input_req->show_time_prompt = show_prompt;
                input_req->expiry_msg = to_hstring(expiry_msg);
                input_req->show_expiry_msg = true;
                input_req->can_skip = can_click;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return { continue_tag, future.get() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, const char*> on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            try {
                if (time_limit > 0) {
                    if (time_limit <= m_tick_compensation) {
                        m_tick_compensation -= time_limit;
                        time_limit = 0;
                    }
                    else {
                        time_limit -= m_tick_compensation;
                        m_tick_compensation = 0;
                    }
                }
                auto input_req = std::make_unique<InputRequestS>(time_limit);
                tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                    if (time_limit < 0) {
                        m_tick_compensation = -time_limit;
                    }
                });
                input_req->default_value = to_hstring(default_value);
                input_req->show_time_prompt = show_prompt;
                input_req->expiry_msg = to_hstring(expiry_msg);
                input_req->show_expiry_msg = true;
                input_req->can_skip = can_click;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return { continue_tag, m_str_cache.c_str() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_oneinput_int(std::optional<int64_t> default_value) override {
            // TODO: on_oneinput_int
            return { continue_tag, std::optional<int64_t>() };
        }
        ControlFlow<std::monostate, const char*> on_oneinput_str(std::optional<std::string_view> default_value) override {
            // TODO: on_oneinput_str
            return { continue_tag, nullptr };
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_toneinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            // TODO: on_toneinput_int
            return { continue_tag, std::optional<int64_t>() };
        }
        ControlFlow<std::monostate, const char*> on_toneinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            // TODO: on_toneinput_str
            return { continue_tag, nullptr };
        }
        void on_reuselastline(std::string_view content) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutineReuseLastLine(content);
            });
        }
        void on_clearline(int64_t count) override {
            if (count <= 0) { return; }
            queue_ui_work([sd = m_sd, count = (uint64_t)count] {
                sd->ui_ctrl->RoutineClearLine(count);
            });
        }
        int64_t on_var_get_int(std::string_view name, size_t idx) override {
            if (name == "@COLOR") {
                std::promise<uint32_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(to_u32(sd->ui_ctrl->EngineForeColor()) & ~0xff000000);
                });
                return future.get();
            }
            if (name == "@DEFCOLOR") {
                return D2D1::ColorF::Silver;
            }
            if (name == "@BGCOLOR") {
                std::promise<uint32_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(to_u32(sd->ui_ctrl->EngineBackColor()) & ~0xff000000);
                });
                return future.get();
            }
            if (name == "@DEFBGCOLOR") {
                return D2D1::ColorF::Black;
            }
            if (name == "@FOCUSCOLOR") {
                return D2D1::ColorF::Yellow;
            }
            if (name == "@STYLE") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentFontStyle());
                });
                return future.get();
            }
            if (name == "@REDRAW") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetRedrawState());
                });
                return future.get();
            }
            if (name == "@ALIGN") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentLineAlignment());
                });
                return future.get();
            }
            if (name == "@TOOLTIP_DELAY") {
                // TODO: @TOOLTIP_DELAY
                return 0;
            }
            if (name == "@TOOLTIP_DURATION") {
                // TODO: @TOOLTIP_DURATION
                return 0;
            }
            if (name == "@SKIPDISP") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetSkipDisplay());
                });
                return future.get();
            }
            if (name == "@MESSKIP") {
                // TODO: @MESSKIP
                return 0;
            }
            if (name == "@ANIMETIMER") {
                // TODO: @ANIMETIMER
                return 0;
            }
            if (name == "@PRINTCPERLINE") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.printc_per_line);
                });
                return future.get();
            }
            if (name == "@PRINTCLENGTH") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.printc_char_count);
                });
                return future.get();
            }
            if (name == "@LINEISEMPTY") {
                std::promise<bool> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cur_composing_line.parts.empty());
                });
                return future.get();
            }
            if (name == "SCREENWIDTH") {
                // TODO: SCREENWIDTH
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_ui_width / 20);
                });
                return future.get();
            }
            if (name == "SCREENPIXELWIDTH") {
                // NOTE: Returns width in logical pixels
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_ui_width);
                });
                return future.get();
            }
            if (name == "LINECOUNT") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value((int64_t)sd->ui_ctrl->m_ui_lines.size());
                });
                return future.get();
            }
            throw std::exception("no such variable");
        }
        const char* on_var_get_str(std::string_view name, size_t idx) override {
            if (name == "@FONT") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentFontName());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "WINDOW_TITLE") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->EngineTitle());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "DRAWLINESTR") {
                std::promise<std::string> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    auto screen_width = sd->ui_ctrl->m_ui_width / 20;
                    promise.set_value(std::string(screen_width, '-'));
                });
                m_str_cache = future.get();
                return m_str_cache.c_str();
            }
            if (name == "SAVEDATA_TEXT") {
                // TODO: SAVEDATA_TEXT
                return nullptr;
            }
            throw std::exception("no such variable");
        }
        void on_var_set_int(std::string_view name, size_t idx, int64_t val) override {
            if (name == "@COLOR") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineForeColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                return;
            }
            if (name == "@BGCOLOR") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineBackColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                return;
            }
            if (name == "@STYLE") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetCurrentFontStyle(val);
                });
                return;
            }
            if (name == "@REDRAW") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetRedrawState(val);
                });
                return;
            }
            if (name == "@ALIGN") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetCurrentLineAlignment(val);
                });
                return;
            }
            if (name == "@TOOLTIP_DELAY") {
                // TODO: @TOOLTIP_DELAY
                return;
            }
            if (name == "@TOOLTIP_DURATION") {
                // TODO: @TOOLTIP_DURATION
                return;
            }
            if (name == "@SKIPDISP") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetSkipDisplay(val);
                });
                return;
            }
            // TODO...
            if (name == "@ANIMETIMER") {
                // TODO: @ANIMETIMER
                return;
            }
            // TODO: Prohibit setting variables @DEF*COLOR?
            // TODO...
            throw std::exception("no such variable");
        }
        void on_var_set_str(std::string_view name, size_t idx, std::string_view val) override {
            if (name == "@FONT") {
                queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->SetCurrentFontName(val);
                });
                return;
            }
            if (name == "WINDOW_TITLE") {
                queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->EngineTitle(val);
                });
                return;
            }
            throw std::exception("no such variable");
        }
        void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content), value = to_hstring(value), flags] {
                sd->ui_ctrl->RoutinePrintButton(content, value, flags);
            });
        }
        int64_t on_gcreate(int64_t gid, int64_t width, int64_t height) override
        {
            return 0;
        }
        int64_t on_gcreatefromfile(int64_t gid, std::string_view path) override
        {
            return 0;
        }
        int64_t on_gdispose(int64_t gid) override
        {
            return 0;
        }
        int64_t on_gcreated(int64_t gid) override
        {
            return 0;
        }
        int64_t on_gdrawsprite(int64_t gid, std::string_view sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const& color_matrix) override
        {
            return 0;
        }
        int64_t on_gclear(int64_t gid, int64_t color) override
        {
            return 0;
        }
        int64_t on_spritecreate(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height) override
        {
            return 0;
        }
        int64_t on_spritedispose(std::string_view name) override
        {
            return 0;
        }
        int64_t on_spritecreated(std::string_view name) override
        {
            return 0;
        }
        int64_t on_spriteanimecreate(std::string_view name, int64_t width, int64_t height) override
        {
            return 0;
        }
        int64_t on_spriteanimeaddframe(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y, int64_t delay) override
        {
            return 0;
        }
        int64_t on_spritewidth(std::string_view name) override
        {
            return 0;
        }
        int64_t on_spriteheight(std::string_view name) override
        {
            return 0;
        }
        std::unique_ptr<MEraEngineHostFile> on_open_host_file(std::string_view path, bool can_write) override {
            file_handle fh(CreateFileW(
                to_hstring(path).c_str(),
                GENERIC_READ | (can_write ? GENERIC_WRITE : 0),
                FILE_SHARE_READ,
                nullptr,
                can_write ? OPEN_ALWAYS : OPEN_EXISTING,
                0,
                nullptr
            ));
            check_bool((bool)fh);

            struct Win32File : MEraEngineHostFile {
                Win32File(file_handle fh) : m_fh(std::move(fh)) {}
                ~Win32File() {}

                uint64_t read(std::span<uint8_t> buf) override {
                    DWORD io_bytes;
                    check_bool(ReadFile(m_fh.get(), buf.data(), buf.size(), &io_bytes, nullptr));
                    return io_bytes;
                }
                void write(std::span<const uint8_t> buf) override {
                    DWORD io_bytes;
                    check_bool(WriteFile(m_fh.get(), buf.data(), buf.size(), &io_bytes, nullptr));
                    assert(buf.size() == io_bytes);
                }
                void flush() override {
                    check_bool(FlushFileBuffers(m_fh.get()));
                }
                void truncate() override {
                    SetFilePointer(m_fh.get(), 0, nullptr, FILE_BEGIN);
                    //check_bool(SetFilePointer(m_fh.get(), 0, nullptr, FILE_BEGIN) != INVALID_SET_FILE_POINTER);
                    check_bool(SetEndOfFile(m_fh.get()));
                }
                uint64_t seek(int64_t pos, EraCompilerFileSeekMode mode) override {
                    int native_mode = 0;
                    switch (mode) {
                    case ERA_COMPILER_FILE_SEEK_MODE_START:
                        native_mode = FILE_BEGIN;
                        break;
                    case ERA_COMPILER_FILE_SEEK_MODE_END:
                        native_mode = FILE_END;
                        break;
                    case ERA_COMPILER_FILE_SEEK_MODE_CURRENT:
                        native_mode = FILE_CURRENT;
                        break;
                    default:
                        std::terminate();
                    }
                    LONG high = (LONG)(pos >> 32);
                    LONG low = (LONG)pos;
                    auto r = SetFilePointer(m_fh.get(), low, &high, native_mode);
                    check_bool(r != INVALID_SET_FILE_POINTER || GetLastError() == NO_ERROR);
                    return (uint32_t)r + (((uint64_t)(uint32_t)high) << 32);
                }

            private:
                file_handle m_fh;
            };

            return std::make_unique<Win32File>(std::move(fh));
        }
        bool on_check_host_file_exists(std::string_view path) override {
            return std::filesystem::exists(path);
        }
        int64_t on_check_font(std::string_view font_name) override
        {
            return 0;
        }
        uint64_t on_get_host_time() override {
            auto t = std::chrono::system_clock::now().time_since_epoch();
            return (uint64_t)std::chrono::duration_cast<std::chrono::milliseconds>(t).count();
        }
        int64_t on_get_config_int(std::string_view name) override {
            if (name == "一行の高さ") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.line_height);
                });
                return future.get();
            }
            if (name == "フォントサイズ") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.font_size);
                });
                return future.get();
            }
            if (name == "ウィンドウ幅") {
                return on_var_get_int("SCREENPIXELWIDTH", 0);
            }
            if (name == "表示するセーブデータ数") {
                return 20;
            }
            throw std::runtime_error(to_string(std::format(L"no such int config: {}", to_hstring(name))));
        }
        const char* on_get_config_str(std::string_view name) override {
            // TODO: on_get_config_str
            if (name == "描画インターフェース") {
                return "Direct2D";
            }
            throw std::runtime_error(to_string(std::format(L"no such str config: {}", to_hstring(name))));
        }
        int64_t on_get_key_state(int64_t key_code) override
        {
            return 0;
        }

    private:
        void queue_ui_work(std::move_only_function<void()> work) {
            if (m_ui_task_tx->send(work) && !m_sd->ui_queue_work_debounce.load(std::memory_order_relaxed)) {
                // Wake up UI thread
                m_sd->ui_redraw_block_engine.wait(true, std::memory_order_relaxed);
                // Debounce to avoid too many UI update calls
                m_sd->ui_queue_work_debounce.store(true, std::memory_order_relaxed);
                // NOTE: CoreDispatcher.RunAsync will cause memory leak, so use
                //       DispatcherQueue.TryEnqueue instead
                m_sd->ui_dispatcher_queue.TryEnqueue(DispatcherQueuePriority::Low, [sd = m_sd->shared_from_this()] {
                    sd->ui_queue_work_debounce.store(false, std::memory_order_relaxed);
                    if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                    // SAFETY: We are in the UI thread; no one could be destructing
                    //         EngineControl.
                    sd->ui_ctrl->UpdateEngineUI();
                });
            }
        }

        EngineSharedData* const m_sd;
        util::sync::spsc::Sender<std::move_only_function<void()>> const* const m_ui_task_tx;
        std::string m_str_cache;
        std::mt19937_64 m_rand_gen{ std::random_device{}() };

    public:
        int64_t m_tick_compensation{};
    };

    com_ptr<IDWriteFactory2> g_dwrite_factory;
    com_ptr<IWICImagingFactory> g_wic_factory;
    void ensure_global_factory() {
        if (g_dwrite_factory) { return; }
        {
            decltype(g_dwrite_factory) factory;
            check_hresult(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED,
                guid_of<decltype(*factory)>(),
                reinterpret_cast<::IUnknown**>(factory.put())
            ));
            g_dwrite_factory = std::move(factory);
        }
        {
            g_wic_factory = create_instance<IWICImagingFactory>(CLSID_WICImagingFactory);
        }
    }

    struct EngineUIPrintLineDataEffect {
        uint32_t starti, len;
        uint32_t color;
        uint32_t style;
    };
    struct EngineUIPrintLineData {
        hstring txt;
        com_ptr<IDWriteTextFormat> txt_fmt;
        com_ptr<IDWriteTextLayout4> txt_layout;
        std::vector<com_ptr<::IUnknown>> inline_objs;   // Usually images
        uint64_t line_height{};
        uint64_t acc_height{};  // Including current line
        std::vector<EngineUIPrintLineDataEffect> effects;
        std::vector<EngineUIPrintLineDataButton> buttons;

        EngineUIPrintLineData(hstring const& txt, com_ptr<IDWriteTextFormat> const& txt_fmt) :
            txt(txt), txt_fmt(txt_fmt) {
        }

        void update_width(EngineControl* ctrl, uint64_t width) {
            if (!txt_layout) { return ensure_layout(ctrl, width); }
            check_hresult(txt_layout->SetMaxWidth(static_cast<float>(width)));

            flush_metrics();
        }
        void ensure_layout(EngineControl* ctrl, uint64_t width) {
            if (txt_layout) { return; }
            com_ptr<IDWriteTextLayout> tmp_layout;
            check_hresult(g_dwrite_factory->CreateTextLayout(
                txt.c_str(), static_cast<UINT32>(txt.size()),
                txt_fmt.get(),
                static_cast<float>(width),
                1e6,
                tmp_layout.put()
            ));
            tmp_layout.as(txt_layout);
            // TODO: Follow user font settings in the future
            check_hresult(txt_layout->SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM,
                ctrl->m_cfg.line_height, ctrl->m_cfg.line_height * 0.8));

            flush_metrics();
        }
        void flush_effects(EngineControl* ctrl) {
            txt_layout->SetDrawingEffect(nullptr, { 0, txt.size() });
            for (auto const& effect : effects) {
                txt_layout->SetDrawingEffect(
                    ctrl->GetOrCreateSolidColorBrush(effect.color),
                    { effect.starti, effect.len });
            }
        }

    private:
        void flush_metrics() {
            //acc_height -= line_height;
            DWRITE_TEXT_METRICS1 metrics;
            check_hresult(txt_layout->GetMetrics(&metrics));
            line_height = static_cast<uint64_t>(metrics.height);
            //acc_height += line_height;
        }
    };

    EngineControl::EngineControl() {
        m_default_font_name = L"MS Gothic";
        ensure_global_factory();
    }
    EngineControl::~EngineControl() {
        UISideDisconnect();
        if (m_devtools_wnd) {
            m_devtools_wnd.Close();
            m_devtools_wnd = nullptr;
        }
    }
    void EngineControl::InitializeComponent() {
        EngineControlT::InitializeComponent();

        // Register for scale notification...
        auto bkg_swapchain_panel = BackgroundSwapchainPanel();
        bkg_swapchain_panel.CompositionScaleChanged([this](SwapChainPanel const& sender, auto&&) {
            m_xscale = sender.CompositionScaleX();
            m_yscale = sender.CompositionScaleY();
            if (m_vsis_noref) {
                FlushEngineImageOutputLayout(true);
            }
        });
        m_xscale = bkg_swapchain_panel.CompositionScaleX();
        m_yscale = bkg_swapchain_panel.CompositionScaleY();
        // And width notification.
        SizeChanged([this](auto&&, auto&&) {
            UpdateUIWidth((uint64_t)ActualWidth());
            // Always bring view to bottom
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        });
        EngineOutputImage().SizeChanged([this](auto&&, auto&&) {
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        });
        RootScrollViewer().ViewChanged([this](auto&&, auto&&) {
            auto pt = m_cur_pt;
            auto root_sv = RootScrollViewer();
            if (root_sv.ScrollableHeight() <= 0) {
                // Slow path
                pt = root_sv.TransformToVisual(EngineOutputImage()).TransformPoint(pt);
            }
            else {
                // Fast path
                pt.Y = float(pt.Y + root_sv.VerticalOffset());
            }
            UpdateAndInvalidateActiveButton(pt);
        });

        // HACK: Prevent automatic bring into view
        UserInputTextBox().BringIntoViewRequested([](auto&&, BringIntoViewRequestedEventArgs const& e) {
            e.Handled(true);
        });

        // Initialize input countdown timer
        m_input_countdown_timer.Interval(std::chrono::milliseconds{ 1 });
        m_input_countdown_timer.Tick({ this, &EngineControl::OnInputCountDownTick });
    }
    void EngineControl::ReturnToTitle() {
        m_outstanding_input_req = nullptr;
        m_cur_printc_count = 0;
        QueueEngineTask(std::make_unique<EngineThreadTask>(EngineThreadTaskKind::ReturnToTitle));

        m_ui_lines.clear();
        check_hresult(m_vsis_noref->Resize(0, 0));
    }
    bool EngineControl::IsStarted() {
        return m_sd && m_sd->thread_is_alive.load(std::memory_order_relaxed);
    }
    void EngineControl::IsDevToolsOpen(bool value) {
        if (value == IsDevToolsOpen()) { return; }

        if (value) {
            m_devtools_wnd = Tenkai::UI::Xaml::Window();
            auto page = make<DevTools::implementation::MainPage>();
            auto p = page.as<DevTools::implementation::MainPage>();
            m_devtools_wnd.Content(page);
            UpdateDevToolsWindow();
            m_devtools_wnd.Activate();
            page.SetConnectedEngineControl(*this);
            m_devtools_wnd.View().Closing([this](auto&&, auto&&) {
                m_devtools_wnd = nullptr;
            });
        }
        else {
            m_devtools_wnd.Close();
            m_devtools_wnd = nullptr;
        }
    }
    bool EngineControl::IsDevToolsOpen() {
        return m_devtools_wnd != nullptr;
    }
    void EngineControl::EngineOutputImage_PointerMoved(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        e.Handled(true);
        auto root_sv = RootScrollViewer();
        m_cur_pt = e.GetCurrentPoint(root_sv).Position();
        auto pt = m_cur_pt;
        if (root_sv.ScrollableHeight() <= 0) {
            // Slow path
            pt = root_sv.TransformToVisual(EngineOutputImage()).TransformPoint(pt);
        }
        else {
            // Fast path
            pt.Y += root_sv.VerticalOffset();
        }
        UpdateAndInvalidateActiveButton(pt);
    }
    void EngineControl::EngineOutputImage_PointerExited(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        m_cur_pt = { -1, -1 };
        UpdateAndInvalidateActiveButton(m_cur_pt);
    }
    void EngineControl::EngineOutputImage_PointerCanceled(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        m_cur_pt = { -1, -1 };
        UpdateAndInvalidateActiveButton(m_cur_pt);
    }
    void EngineControl::EngineOutputImage_Tapped(IInspectable const& sender, TappedRoutedEventArgs const& e) {
        e.Handled(true);
        m_cur_pt = e.GetPosition(RootScrollViewer());
        auto pt = e.GetPosition(EngineOutputImage());
        UpdateAndInvalidateActiveButton(pt);
        bool handled_button{};
        if (m_cur_active_button.line < size(m_ui_lines)) {
            auto const& cur_line = m_ui_lines[m_cur_active_button.line];
            if (m_cur_active_button.button_idx < size(cur_line.buttons)) {
                auto const& cur_button = cur_line.buttons[m_cur_active_button.button_idx];
                // Tapping a button
                handled_button = true;
                auto input_button_fn = [this](EngineUIPrintLineDataButton::InputButton const& v) -> fire_and_forget {
                    auto input_tb = UserInputTextBox();
                    auto old_text = input_tb.Text();
                    input_tb.Text(v.input);
                    TryFulfillInputRequest(false);
                    input_tb.Text(old_text);
                    co_return;
                };
                auto source_button_fn = [this](EngineUIPrintLineDataButton::SourceButton const& v) -> fire_and_forget {
                    ContentDialog cd;
                    auto cmd = std::format(L"/C code -g \"{}:{}:{}\"", v.path, v.line, v.column);
                    cd.XamlRoot(XamlRoot());
                    cd.Title(box_value(L"执行外部命令?"));
                    cd.Content(box_value(winrt::format(L""
                        "MEraEmu 将在你的系统中执行以下 Shell 命令:\n{}\n"
                        "如果不信任此命令, 请不要继续。确实要继续执行吗?",
                        std::wstring_view{ cmd }.substr(3)
                    )));
                    cd.PrimaryButtonText(L"是, 执行");
                    cd.CloseButtonText(L"取消");
                    cd.DefaultButton(ContentDialogButton::Primary);
                    cd.PrimaryButtonClick([&](auto&&, auto&&) {
                        STARTUPINFOW si{ sizeof si };
                        PROCESS_INFORMATION pi;
                        bool succeeded = CreateProcessW(
                            L"C:\\Windows\\System32\\cmd.exe", cmd.data(),
                            nullptr, nullptr,
                            false, CREATE_NO_WINDOW,
                            nullptr, nullptr,
                            &si,
                            &pi
                        );
                        if (succeeded) {
                            CloseHandle(pi.hProcess);
                            CloseHandle(pi.hThread);
                        }
                    });
                    co_await cd.ShowAsync();
                };
                std::visit(overloaded{ input_button_fn, source_button_fn }, cur_button.data);
            }
        }
        if (!handled_button) {
            // Bring to bottom
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
            // Try to skip input requests
            if (auto& input_req = m_outstanding_input_req) {
                if (input_req->can_click) {
                    input_req->try_fulfill_void();
                    input_req = nullptr;
                }
            }
        }
    }
    void EngineControl::EngineOutputImage_RightTapped(IInspectable const& sender, RightTappedRoutedEventArgs const& e) {
        e.Handled(true);

        // Bring to bottom
        RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        // Try to skip input requests
        if (auto& input_req = m_outstanding_input_req) {
            if (input_req->can_click || input_req->can_skip) {
                input_req->try_fulfill_void();
                input_req = nullptr;
                m_user_skipping = true;
            }
        }
    }
    void EngineControl::UserInputTextBox_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        if (e.Key() == VirtualKey::Enter) {
            e.Handled(true);

            // Try to fulfill input requests
            TryFulfillInputRequest(true);
        }
    }
    void EngineControl::Bootstrap(hstring const& game_base_dir) try {
        UISideDisconnect();
        m_sd = std::make_shared<EngineSharedData>();
        m_sd->game_base_dir = game_base_dir;
        m_sd->ui_dispatcher_queue = DispatcherQueue::GetForCurrentThread();
        //m_sd->ui_ctrl = get_weak();
        m_sd->ui_ctrl = this;
        // Create channels
        auto [ui_task_tx, ui_task_rx] = util::sync::spsc::sync_channel<std::move_only_function<void()>>(64);
        auto [engine_task_tx, engine_task_rx] = util::sync::spsc::sync_channel<std::unique_ptr<EngineThreadTask>>(64);
        m_ui_task_rx = std::move(ui_task_rx);
        m_engine_task_tx = std::move(engine_task_tx);
        // Start a dedicated background thread
        m_sd->thread_task_op = ThreadPool::RunAsync([sd = m_sd, ui_task_tx = std::move(ui_task_tx), engine_task_rx = std::move(engine_task_rx)](IAsyncAction const& op) {
            SetThreadDescription(GetCurrentThread(), L"MEraEmu Engine Thread");

            sd->thread_is_alive.store(true, std::memory_order_relaxed);

            auto& engine = sd->engine;
            auto queue_ui_work = [&](std::move_only_function<void()> work) {
                auto is_vacant = ui_task_tx.is_vacant();
                if (ui_task_tx.send(work) && is_vacant) {
                    // Wake up UI thread
                    sd->ui_redraw_block_engine.wait(true, std::memory_order_relaxed);
                    // NOTE: CoreDispatcher.RunAsync will cause memory leak, so use
                    //       DispatcherQueue.TryEnqueue instead
                    sd->ui_dispatcher_queue.TryEnqueue(DispatcherQueuePriority::Low, [sd] {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        // SAFETY: We are in the UI thread; no one could be destructing
                        //         EngineControl.
                        sd->ui_ctrl->UpdateEngineUI();
                    });
                }
            };

            auto mark_thread_started = [&] {
                sd->thread_started.store(true, std::memory_order_release);
                sd->thread_started.notify_one();
            };

            auto task_disconnect = [&] {
                sd->thread_is_alive.store(false, std::memory_order_relaxed);
                sd->thread_is_alive.notify_one();
                // Triggers UI update, which in turn responds to task disconnection
                queue_ui_work([] {});
            };

            tenkai::cpp_utils::ScopeExit se_thread([&] {
                task_disconnect();
                // In case of exception, don't hang the UI thread
                mark_thread_started();
                engine = nullptr;
            });

            try {
                sd->ui_is_alive.wait(false, std::memory_order_acquire);

                MEraEmuWinEngineSysCallback* callback{};
                auto builder = [&] {
                    auto callback_box = std::make_unique<MEraEmuWinEngineSysCallback>(sd.get(), &ui_task_tx);
                    callback = callback_box.get();
                    return MEraEngineBuilder(std::move(callback_box));
                }();

                // Register global variables
                // Engine -- register int
                auto eri = [&](const char* name) {
                    builder.register_variable(name, false, 1, true);
                };
                // Engine -- register str
                auto ers = [&](const char* name) {
                    builder.register_variable(name, true, 1, true);
                };
                eri("@COLOR");
                eri("@DEFCOLOR");
                eri("@BGCOLOR");
                eri("@DEFBGCOLOR");
                eri("@FOCUSCOLOR");
                eri("@STYLE");
                ers("@FONT");
                eri("@REDRAW");
                eri("@ALIGN");
                eri("@TOOLTIP_DELAY");
                eri("@TOOLTIP_DURATION");
                eri("@SKIPDISP");
                eri("@MESSKIP");
                eri("@ANIMETIMER");
                eri("@PRINTCPERLINE");
                eri("@PRINTCLENGTH");
                eri("@LINEISEMPTY");
                ers("WINDOW_TITLE");
                ers("DRAWLINESTR");
                eri("SCREENWIDTH");
                eri("SCREENPIXELWIDTH");
                eri("LINECOUNT");
                ers("SAVEDATA_TEXT");
                eri("RANDDATA");

                auto return_to_title = [&] {
                    EraFuncInfo func_info;
                    try {
                        func_info = engine.get_func_info("SYSPROC_BEGIN_TITLE").value();
                    }
                    // Ignore if function does not exist
                    catch (...) { return; }
                    if (func_info.frame_info.args.size() != 0) {
                        throw hresult_error(E_FAIL, L"malformed entry function");
                    }
                    EraExecIp ip{
                        .chunk = func_info.chunk_idx,
                        .offset = func_info.bc_offset,
                    };
                    engine.reset_exec_to_ip(ip);
                };

                auto run_ui_task = [&](std::unique_ptr<EngineThreadTask> task, bool loaded) {
                    if (!loaded) { return; }
                    if (task->kind == EngineThreadTaskKind::ReturnToTitle) {
                        return_to_title();
                    }
                    else if (task->kind == EngineThreadTaskKind::CustomFunc) {
                        task->f();
                    }
                    else {
                        throw hresult_error(E_FAIL, L"unexpected task kind");
                    }
                };

                // Collect files used by engine
                {
                    std::vector<std::filesystem::path> misc_csvs;
                    std::vector<std::filesystem::path> chara_csvs;
                    auto load_csv = [&](std::filesystem::path const& csv, EraCsvLoadKind kind) {
                        auto [data, size] = read_utf8_file(csv);
                        builder.load_csv(to_string(csv.c_str()).c_str(), { data.get(), size }, kind);
                    };
                    for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"CSV")) {
                        if (!entry.is_regular_file()) { continue; }
                        auto const& path = entry.path();
                        auto filename = path.filename();
                        std::wstring_view sv{ filename.c_str() };
                        if (ieq(sv, L"_Rename.csv")) { load_csv(path, ERA_CSV_LOAD_KIND__RENAME); }
                        else if (ieq(sv, L"_Replace.csv")) { load_csv(path, ERA_CSV_LOAD_KIND__REPLACE); }
                        else if (ieq(sv, L"VariableSize.csv")) { load_csv(path, ERA_CSV_LOAD_KIND_VARIABLE_SIZE); }
                        else {
                            if (!iends_with(sv, L".csv")) { continue; }
                            if (istarts_with(sv, L"chara")) {
                                chara_csvs.push_back(path);
                            }
                            else {
                                misc_csvs.push_back(path);
                            }
                        }
                    }
                    std::vector<std::filesystem::path> erhs;
                    std::vector<std::filesystem::path> erbs;
                    for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"ERB")) {
                        if (!entry.is_regular_file()) { continue; }
                        auto filename = entry.path().filename();
                        if (iends_with(filename.c_str(), L".erh")) {
                            erhs.push_back(entry.path());
                        }
                        else if (iends_with(filename.c_str(), L".erb")) {
                            erbs.push_back(entry.path());
                        }
                        else {
                            // Do nothing
                        }
                    }

                    // Source loading takes a lot of time, so mark thread as started here
                    mark_thread_started();

                    // Use a dedicated thread to read ERB files
                    auto [erb_tx, erb_rx] = util::sync::spsc::sync_channel<std::tuple<std::filesystem::path, std::unique_ptr<uint8_t[]>, size_t>>(64);
                    auto erb_thread = std::jthread([&, erb_tx = std::move(erb_tx)] {
                        for (auto& erb : erbs) {
                            if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                            auto [data, size] = read_utf8_file(erb);
                            std::tuple<std::filesystem::path, std::unique_ptr<uint8_t[]>, size_t> erb_data{ erb, std::move(data), size };
                            if (!erb_tx.send(erb_data)) { return; }
                        }
                    });

                    // Load CSV files
                    for (auto& csv : misc_csvs) {
                        auto filename = csv.filename();
                        std::wstring_view sv{ filename.c_str() };
                        if (ieq(sv, L"ABL.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_ABL); }
                        else if (ieq(sv, L"EXP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EXP); }
                        else if (ieq(sv, L"TALENT.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_TALENT); }
                        else if (ieq(sv, L"PALAM.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_PALAM); }
                        else if (ieq(sv, L"TRAIN.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_TRAIN); }
                        else if (ieq(sv, L"MARK.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_MARK); }
                        else if (ieq(sv, L"ITEM.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_ITEM); }
                        else if (ieq(sv, L"BASE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_BASE); }
                        else if (ieq(sv, L"SOURCE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_SOURCE); }
                        else if (ieq(sv, L"EX.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EX); }
                        else if (ieq(sv, L"STR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STR); }
                        else if (ieq(sv, L"EQUIP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EQUIP); }
                        else if (ieq(sv, L"TEQUIP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_EQUIP); }
                        else if (ieq(sv, L"FLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_FLAG); }
                        else if (ieq(sv, L"TFLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_FLAG); }
                        else if (ieq(sv, L"CFLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_FLAG); }
                        else if (ieq(sv, L"TCVAR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_C_VAR); }
                        else if (ieq(sv, L"CSTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_STR); }
                        else if (ieq(sv, L"STAIN.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STAIN); }
                        else if (ieq(sv, L"CDFLAG1.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_D_FLAG1); }
                        else if (ieq(sv, L"CDFLAG2.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_D_FLAG2); }
                        else if (ieq(sv, L"STRNAME.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STR_NAME); }
                        else if (ieq(sv, L"TSTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_STR); }
                        else if (ieq(sv, L"SAVESTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_SAVE_STR); }
                        else if (ieq(sv, L"GLOBAL.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GLOBAL); }
                        else if (ieq(sv, L"GLOBALS.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GLOBALS); }
                        else if (ieq(sv, L"GAMEBASE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GAME_BASE); }
                        else {
                            // Do nothing
                        }
                    }
                    for (auto& csv : chara_csvs) {
                        load_csv(csv, ERA_CSV_LOAD_KIND_CHARA_);
                    }
                    builder.finish_load_csv();

                    auto try_handle_thread_event = [&] {
                        if (auto task = engine_task_rx.try_recv()) {
                            run_ui_task(std::move(*task), false);
                        }
                    };

                    // Load ERB files
                    for (auto& erh : erhs) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto [data, size] = read_utf8_file(erh);
                        builder.load_erh(to_string(erh.c_str()).c_str(), { data.get(), size });
                    }
                    builder.finish_load_erh();
                    /*for (auto& erb : erbs) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto [data, size] = read_utf8_file(erb);
                        builder.load_erb(to_string(erb.c_str()).c_str(), { data.get(), size });
                    }*/
                    while (auto erb_opt = erb_rx.recv()) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto& [name, data, size] = *erb_opt;
                        builder.load_erb(to_string(name.c_str()).c_str(), { data.get(), size });
                    }
                }

                // Finialize compilation
                if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                engine = builder.build();

                return_to_title();

                // Disable auto redraw before running VM
                queue_ui_work([sd] {
                    sd->ui_ctrl->SetRedrawState(0);
                });

                // Main loop
                EraExecutionBreakReason stop_reason = ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS;
                uint64_t instructions_to_exec = UINT64_MAX;
                bool force_exec = false;
                // (?) Notify UI thread that engine is about to execute instructions
                queue_ui_work([sd, stop_reason] {
                    sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                });
                while (sd->ui_is_alive.load(std::memory_order_relaxed)) {
                    // If not reached max instructions, we treat engine as halted
                    auto is_halted = [&] {
                        return is_execution_break_reason_fatal(stop_reason);
                    };
                    auto clear_halted = [&] {
                        stop_reason = ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS;
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                    };
                    auto set_halted = [&] {
                        stop_reason = ERA_EXECUTION_BREAK_REASON_DEBUG_BREAK_INSTRUCTION;
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                    };
                    while (force_exec || !is_halted()) {
                        //std::atomic_bool alwaystrue{ true };
                        stop_reason = engine.do_execution(
                            //force_exec ? alwaystrue : engine_task_rx.is_vacant_var(),
                            engine_task_rx.is_vacant_var(),
                            instructions_to_exec);
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                        break;
                    }
                    force_exec = false;
                    if (instructions_to_exec != UINT64_MAX) {
                        // Limit reached, auto halt
                        instructions_to_exec = UINT64_MAX;
                        set_halted();
                    }

                    callback->m_tick_compensation = 0;
                    if (sd->has_execution_error) {
                        // Dump stack trace when execution error occurs
                        sd->has_execution_error = false;
                        struct SrcData {
                            hstring path;
                            uint32_t line, column;
                            hstring msg;
                        };
                        std::vector<SrcData> print_msgs;
                        auto stack_trace = engine.get_stack_trace();
                        for (const auto& frame : stack_trace.frames | std::views::reverse) {
                            auto ip = frame.ip;
                            auto func_info = engine.get_func_info_by_ip(ip).value();
                            auto chunk_info = engine.get_chunk_info(func_info.chunk_idx).value();
                            auto src_info = engine.get_src_info_from_ip(ip).value();
                            auto path = to_hstring(src_info.filename);
                            auto resolved = engine.resolve_src_span(src_info.filename, src_info.span).value();
                            auto msg = format(L"  {}({},{}):{}\nSnippet: {}",
                                path,
                                resolved.loc.line,
                                resolved.loc.col + 1,
                                to_hstring(func_info.name),
                                to_hstring(resolved.snippet)
                            );
                            print_msgs.emplace_back(
                                path,
                                resolved.loc.line, resolved.loc.col + 1,
                                std::move(msg)
                            );
                        }
                        queue_ui_work([sd, msgs = std::move(print_msgs)] {
                            sd->ui_ctrl->SetSkipDisplay(0);

                            sd->ui_ctrl->RoutinePrint(L"函数堆栈跟踪 (最近调用者最先显示):", ERA_PEF_IS_LINE);
                            for (auto const& msg : msgs) {
                                sd->ui_ctrl->RoutinePrintSourceButton(msg.msg, msg.path,
                                    msg.line, msg.column, ERA_PEF_IS_LINE);
                            }

                            sd->ui_ctrl->SetRedrawState(2);
                        });
                    }
                    // Handle one task event
                    do {
                        auto task_opt = engine_task_rx.recv();
                        if (!task_opt) { break; }
                        auto& task = *task_opt;
                        if (task->kind == EngineThreadTaskKind::ReturnToTitle) {
                            return_to_title();
                            clear_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::SetHaltState) {
                            set_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::ClearHaltState) {
                            clear_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::SingleStepAndHalt) {
                            instructions_to_exec = 1;
                            clear_halted();
                            force_exec = true;
                            break;
                        }
                        else if (task->kind == EngineThreadTaskKind::CustomFunc) {
                            if (task->f) {
                                task->f();
                            }
                        }
                        else if (task->kind == EngineThreadTaskKind::CustomFuncAndClearHaltState) {
                            if (task->f) {
                                task->f();
                            }
                            clear_halted();
                        }
                        //run_ui_task(std::move(task), true);
                    } while (!engine_task_rx.is_vacant());
                }
            }
            catch (...) {
                sd->thread_exception = std::current_exception();
            }
        }, WorkItemPriority::Normal, WorkItemOptions::TimeSliced);
        m_sd->ui_is_alive.store(true, std::memory_order_release);
        m_sd->ui_is_alive.notify_one();

        // Initialize engine UI component
        InitEngineUI();

        // Check whether engine has panicked
        m_sd->thread_started.wait(false, std::memory_order_acquire);
        if (!m_sd->thread_is_alive.load(std::memory_order_relaxed)) {
            assert(m_sd->thread_exception);
            std::rethrow_exception(m_sd->thread_exception);
        }

        // If DevTools is open, connect to it
        if (IsDevToolsOpen()) {
            m_devtools_wnd.Content().as<DevTools::MainPage>().SetConnectedEngineControl(*this);
        }
    }
    catch (std::exception const& e) {
        throw hresult_error(E_FAIL, to_hstring(e.what()));
    }
    void EngineControl::UISideDisconnect() {
        // No longer relevant, so also clear the event handlers
        m_ev_EngineExecutionInterrupted.clear();
        // Close DevTools connection
        if (IsDevToolsOpen()) {
            m_devtools_wnd.Content().as<DevTools::MainPage>().SetConnectedEngineControl(nullptr);
        }
        // Clear engine resources
        m_outstanding_input_req = nullptr;
        if (auto sd = std::exchange(m_sd, nullptr)) {
            if (sd->thread_task_op) {
                sd->thread_task_op.Cancel();
            }
            sd->ui_is_alive.store(false, std::memory_order_relaxed);
            sd->ui_is_alive.notify_one();
            sd->ui_redraw_block_engine.store(false, std::memory_order_relaxed);
            sd->ui_redraw_block_engine.notify_one();
            // Close the channel to signal the engine thread to stop
            m_engine_task_tx = nullptr;
            // Wait for engine thread to stop
            // NOTE: Don't execute any engine tasks after this point, for example
            //       m_outstanding_input_req may be assigned a new value.
            while (m_ui_task_rx.recv()) {}
        }
    }
    void EngineControl::QueueEngineTask(std::unique_ptr<EngineThreadTask> task) {
        if (!m_sd) { return; }
        // In order for engine to handle tasks properly, we need to interrupt pending input requests
        if (m_outstanding_input_req) {
            m_outstanding_input_req = nullptr;
        }
        m_engine_task_tx.send(task);
    }
    void EngineControl::QueueEngineTask(EngineThreadTaskKind task_kind) {
        QueueEngineTask(std::make_unique<EngineThreadTask>(task_kind));
    }
    void EngineControl::QueueEngineFuncTask(std::move_only_function<void(MEraEngine const&)> f, bool clear_halt) {
        QueueEngineTask(std::make_unique<EngineThreadTask>([sd = m_sd.get(), f = std::move(f)]() mutable {
            f(sd->engine);
        }, clear_halt));
    }
    void EngineControl::OnEngineExecutionInterrupted(EraExecutionBreakReason reason) {
        m_last_execution_break_reason = reason;
        m_ev_EngineExecutionInterrupted(reason);
    }
    void EngineControl::UpdateDevToolsWindow() {
        if (!IsDevToolsOpen()) { return; }

        // Update window title
        m_devtools_wnd.Title(format(L"{} - MEraEmu DevTools", EngineTitle()));
    }
    void EngineControl::InitEngineUI() {
        // Reset UI resources
        m_vsis_noref = nullptr;
        m_vsis_d2d_noref = nullptr;
        m_d2d_ctx = nullptr;
        // TODO: Proper UI scale
        //m_xscale = m_yscale = XamlRoot().RasterizationScale();
        m_ui_lines.clear();
        m_cur_composing_line = {};
        m_reused_last_line = false;
        m_cur_line_alignment = {};
        m_cur_font_style = {};
        m_cur_font_name = m_default_font_name;
        m_auto_redraw = true;
        m_skip_display = false;
        m_no_skip_display_cnt = 0;
        m_cur_printc_count = 0;
        m_user_skipping = false;
        m_cur_pt = { -1, -1 };
        m_brush_map.clear();
        m_font_map.clear();
        ClearValue(m_EngineForeColorProperty);
        ClearValue(m_EngineBackColorProperty);
        UpdateUIWidth(std::exchange(m_ui_width, {}));
        VirtualSurfaceImageSource img_src(0, 0);
        EngineOutputImage().Source(img_src);
        m_vsis_noref = img_src.as<IVirtualSurfaceImageSourceNative>().get();
        m_vsis_d2d_noref = img_src.as<ISurfaceImageSourceNativeWithD2D>().get();
        // Initialize Direct2D immediately after the creation of EngineOutputImage
        InitD2DDevice(false);
        // TODO: Handle Windows.UI.Xaml.Media.CompositionTarget.SurfaceContentsLost
        // TODO...

        // Enable redraw notification
        struct RedrawCallback : implements<RedrawCallback, IVirtualSurfaceUpdatesCallbackNative> {
            RedrawCallback(EngineControl* ui_ctrl) : m_ui_ctrl(ui_ctrl) {}

            STDMETHOD(UpdatesNeeded)() {
                return tenkai::winrt::ExceptionBoundary([&] {
                    // TODO: SAFETY
                    m_ui_ctrl->RedrawDirtyEngineImageOutput();
                });
            }

        private:
            EngineControl* const m_ui_ctrl;
        };
        m_vsis_noref->RegisterForUpdatesNeeded(make<RedrawCallback>(this).get());

        // Set UI focus
        VisualStateManager::GoToState(*this, L"ExecutionStarted", true);
        UserInputTextBox().Focus(FocusState::Programmatic);
    }
    void EngineControl::UpdateEngineUI() {
        // Process UI works sent from the engine thread
        while (auto work = m_ui_task_rx.try_recv()) {
            (*work)();
        }

        // After processing works, update layout so that OS can fire redraw events
        // TODO...
        //m_vsis_noref->Resize();

        // Handle engine thread termination
        if (!m_sd->thread_is_alive.load(std::memory_order_relaxed)) {
            VisualStateManager::GoToState(*this, L"ExecutionEnded", true);
            if (m_sd->thread_exception) {
                EmitUnhandledExceptionEvent(m_sd->thread_exception);
            }
        }
    }
    void EngineControl::EmitUnhandledExceptionEvent(std::exception_ptr ex) {
        hresult code;
        hstring ex_msg;
        try { std::rethrow_exception(m_sd->thread_exception); }
        catch (...) {
            code = to_hresult();
            ex_msg = to_message();
        }
        m_ev_UnhandledException(*this, make<EngineUnhandledExceptionEventArgs>(code, ex_msg));
    }
    void EngineControl::RedrawDirtyEngineImageOutput() {
        int height = GetCalculatedUIHeight();
        //check_hresult(m_vsis_noref->Resize(m_ui_width, height));
        DWORD update_rt_cnt{};
        check_hresult(m_vsis_noref->GetUpdateRectCount(&update_rt_cnt));
        std::vector<RECT> update_rts(update_rt_cnt);
        check_hresult(m_vsis_noref->GetUpdateRects(update_rts.data(), update_rt_cnt));

        // Update every dirty rectangle
        for (auto const& update_rt : update_rts) {
            com_ptr<ID2D1DeviceContext> ctx;
            POINT offset{};
            check_hresult(m_vsis_d2d_noref->BeginDraw(update_rt,
                guid_of<decltype(ctx)>(), ctx.put_void(), &offset));
            tenkai::cpp_utils::ScopeExit se_begin_draw([&] {
                // Deliberately ignores errors
                m_vsis_d2d_noref->EndDraw();
            });
            // TODO: Offset, clipping, ...
            ctx->SetTransform(
                D2D1::Matrix3x2F::Scale(m_xscale, m_yscale) *
                D2D1::Matrix3x2F::Translation(offset.x - update_rt.left, offset.y - update_rt.top)
            );
            /*ctx->PushAxisAlignedClip(
                D2D1::RectF(
                    update_rt.left, update_rt.top, update_rt.right, update_rt.bottom),
                D2D1_ANTIALIAS_MODE_ALIASED
            );*/
            ctx->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_ALIASED);
            ctx->SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
            int dip_rt_top = update_rt.top / m_yscale;
            int dip_rt_bottom = update_rt.bottom / m_yscale;
            // Clear stale bitmap area
            ctx->Clear(D2D1::ColorF(D2D1::ColorF::White, 0));
            {
                /*ctx->DrawLine(D2D1::Point2F(0, dip_rt_top), D2D1::Point2F(1e4, dip_rt_top),
                    GetOrCreateSolidColorBrush(D2D1::ColorF::Green | 0xff000000));
                ctx->DrawLine(D2D1::Point2F(0, dip_rt_bottom), D2D1::Point2F(1e4, dip_rt_bottom),
                    GetOrCreateSolidColorBrush(D2D1::ColorF::Yellow | 0xff000000));*/
                    /*ctx->DrawLine(D2D1::Point2F(0, offset.y), D2D1::Point2F(1e4, offset.y),
                        GetOrCreateSolidColorBrush(D2D1::ColorF::Green | 0xff000000));
                    ctx->DrawLine(D2D1::Point2F(0, offset.y + (update_rt.bottom - update_rt.top)), D2D1::Point2F(1e4, offset.y + (update_rt.bottom - update_rt.top)),
                        GetOrCreateSolidColorBrush(D2D1::ColorF::Yellow | 0xff000000));*/
            }
            // Obtain lines requiring redraws
            auto ib = begin(m_ui_lines), ie = end(m_ui_lines);
            int line_start = std::ranges::upper_bound(ib, ie, dip_rt_top,
                [](auto const& a, auto const& b) { return a < b; },
                [](auto const& e) { return e.acc_height; }
            ) - ib;
            int line_end = std::ranges::upper_bound(ib, ie, dip_rt_bottom,
                [](auto const& a, auto const& b) { return a < b; },
                [](auto const& e) { return e.acc_height; }
            ) - ib;
            if (line_end < size(m_ui_lines)) { line_end++; }

            // Fallback brush (not actually used in practice, as all texts have
            // color effects applied)
            auto brush = GetOrCreateSolidColorBrush(D2D1::ColorF::Silver | 0xff000000);

            for (int line = line_start; line < line_end; line++) {
                auto& line_data = m_ui_lines[line];
                int offx = 0;
                int offy = line_data.acc_height - line_data.line_height;

                // If we have an active button at this line, apply and flush effects
                if (m_cur_active_button.line == line && m_cur_active_button.button_idx < size(line_data.buttons)) {
                    auto const& btn_data = line_data.buttons[m_cur_active_button.button_idx];
                    EngineUIPrintLineDataEffect effect{
                        .starti = btn_data.starti,
                        .len = btn_data.len,
                        .color = D2D1::ColorF::Yellow | 0xff000000,
                        .style = 0,
                    };
                    line_data.effects.push_back(effect);
                    tenkai::cpp_utils::ScopeExit se_effect([&] {
                        line_data.effects.pop_back();
                        try { line_data.flush_effects(this); }
                        catch (...) {}
                    });
                    line_data.flush_effects(this);
                    ctx->DrawTextLayout(D2D1::Point2F(offx, offy), line_data.txt_layout.get(),
                        brush);
                }
                else {
                    ctx->DrawTextLayout(D2D1::Point2F(offx, offy), line_data.txt_layout.get(),
                        brush);
                }
            }
            /* Stash:
            // Large transform causes blurry text, so we must manually handle it
            ctx->SetTransform(
                D2D1::Matrix3x2F::Scale(m_xscale, m_yscale) *
                D2D1::Matrix3x2F::Translation(offset.x, offset.y)
            );
            ctx->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_ALIASED);
            int dip_rt_top = update_rt.top / m_yscale;
            int dip_rt_bottom = update_rt.bottom / m_yscale;
            // Clear stale bitmap area
            ctx->Clear(D2D1::ColorF(D2D1::ColorF::White, 0));
            // Obtain lines requiring redraws
            auto ib = begin(m_ui_lines), ie = end(m_ui_lines);
            int line_start = std::ranges::upper_bound(ib, ie, dip_rt_top,
                [](auto const& a, auto const& b) { return a < b; },
                [](auto const& e) { return e.acc_height; }
            ) - ib;
            int line_end = std::ranges::upper_bound(ib, ie, dip_rt_bottom,
                [](auto const& a, auto const& b) { return a < b; },
                [](auto const& e) { return e.acc_height; }
            ) - ib;
            if (line_end < size(m_ui_lines)) { line_end++; }

            // Fallback brush (not actually used in practice, as all texts have
            // color effects applied)
            auto brush = GetOrCreateSolidColorBrush(D2D1::ColorF::Silver | 0xff000000);

            for (int line = line_start; line < line_end; line++) {
                auto& line_data = m_ui_lines[line];
                int offx = 0;
                int offy = line_data.acc_height - line_data.line_height;
                float foffx = offx - update_rt.left / m_xscale;
                float foffy = offy - update_rt.top / m_yscale + 0.3f;
                ctx->DrawTextLayout(D2D1::Point2F(foffx, foffy), line_data.txt_layout.get(),
                    brush);
            }
            */
        }

        if (m_sd->ui_redraw_block_engine.exchange(false, std::memory_order_relaxed)) {
            m_sd->ui_redraw_block_engine.notify_one();
        }
    }
    void EngineControl::FlushEngineImageOutputLayout(bool invalidate_all) {
        auto height = GetCalculatedUIHeight();
        int real_width = m_ui_width * m_xscale;
        int real_height = height * m_yscale;
        int old_real_height = m_last_redraw_dirty_height * m_yscale;
        check_hresult(m_vsis_noref->Resize(real_width, height * m_yscale));
        if (invalidate_all) {
            check_hresult(m_vsis_noref->Invalidate({ 0, 0, real_width, real_height }));
        }
        else {
            check_hresult(m_vsis_noref->Invalidate({ 0, old_real_height, real_width, real_height }));
        }
        m_last_redraw_dirty_height = height;
        // TODO: Is this an XAML bug?
        EngineOutputImage().InvalidateMeasure();
    }
    void EngineControl::InitD2DDevice(bool force_software) {
        D3D_FEATURE_LEVEL feature_levels[]{
            D3D_FEATURE_LEVEL_11_1,
            D3D_FEATURE_LEVEL_11_0,
            D3D_FEATURE_LEVEL_10_1,
            D3D_FEATURE_LEVEL_10_0,
            D3D_FEATURE_LEVEL_9_3,
            D3D_FEATURE_LEVEL_9_2,
            D3D_FEATURE_LEVEL_9_1,
        };
        com_ptr<ID3D11Device> d3d_dev;
        check_hresult(D3D11CreateDevice(
            nullptr,
            force_software ? D3D_DRIVER_TYPE_WARP : D3D_DRIVER_TYPE_HARDWARE,
            0,
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            feature_levels,
            static_cast<UINT>(std::size(feature_levels)),
            D3D11_SDK_VERSION,
            d3d_dev.put(),
            nullptr,
            nullptr
        ));
        auto dxgi_dev = d3d_dev.as<IDXGIDevice3>();

        com_ptr<ID2D1Device> d2d_dev;
        check_hresult(D2D1CreateDevice(dxgi_dev.get(), nullptr, d2d_dev.put()));
        check_hresult(d2d_dev->CreateDeviceContext(
            D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
            m_d2d_ctx.put()
        ));

        // Associate output image with created Direct2D device
        check_hresult(m_vsis_d2d_noref->SetDevice(d2d_dev.get()));
    }
    void EngineControl::UpdateUIWidth(uint64_t new_width) {
        if (new_width == m_ui_width) { return; }
        new_width /= 4;
        new_width *= 4;
        m_ui_width = new_width;
        EngineOutputImage().Width((double)new_width);
        uint64_t last_height{};
        if (size(m_ui_lines) > 5000) {
            // Multi-threaded process
            auto total_cnt = size(m_ui_lines);
            auto split_cnt = (total_cnt + total_cnt - 1) / 5000;
            if (split_cnt > 4) {
                split_cnt = 4;
            }
            std::vector<std::future<void>> workers;
            for (size_t i = 1; i < split_cnt; i++) {
                auto start_pos = total_cnt * i / split_cnt;
                auto end_pos = total_cnt * (i + 1) / split_cnt;
                std::promise<void> worker_promise;
                workers.push_back(worker_promise.get_future());
                ThreadPool::RunAsync([=, promise = std::move(worker_promise)](auto&&) mutable {
                    try {
                        for (size_t i = start_pos; i < end_pos; i++) {
                            auto& line = m_ui_lines[i];
                            line.update_width(this, new_width);
                        }
                        promise.set_value();
                    }
                    catch (...) { promise.set_exception(std::current_exception()); }
                });
            }
            for (size_t i = 0; i < total_cnt * 1 / split_cnt; i++) {
                auto& line = m_ui_lines[i];
                line.update_width(this, new_width);
                line.acc_height = last_height + line.line_height;
                last_height = line.acc_height;
            }
            for (auto& worker : workers) {
                worker.get();
            }
            for (size_t i = total_cnt * 1 / split_cnt; i < total_cnt; i++) {
                auto& line = m_ui_lines[i];
                line.acc_height = last_height + line.line_height;
                last_height = line.acc_height;
            }
        }
        else {
            // Process directly on the UI thread
            for (auto& line : m_ui_lines) {
                line.update_width(this, new_width);
                line.acc_height = last_height + line.line_height;
                last_height = line.acc_height;
            }
        }

        if (m_vsis_noref) {
            int height = GetCalculatedUIHeight();
            auto new_w = static_cast<int>(m_ui_width * m_xscale);
            auto new_h = static_cast<int>(height * m_yscale);
            check_hresult(m_vsis_noref->Resize(new_w, new_h));
            m_vsis_noref->Invalidate({ 0, 0, new_w, new_h });
        }
    }
    uint64_t EngineControl::GetCalculatedUIHeight() {
        if (m_ui_lines.empty()) { return 0; }
        return m_ui_lines.back().acc_height;
    }
    size_t EngineControl::GetLineIndexFromHeight(uint64_t height) {
        auto ib = begin(m_ui_lines), ie = end(m_ui_lines);
        return std::ranges::upper_bound(ib, ie, height,
            [](auto const& a, auto const& b) { return a < b; },
            [](auto const& e) { return e.acc_height; }
        ) - ib;
    }
    void EngineControl::InvalidateLineAtIndex(size_t line) {
        if (line >= size(m_ui_lines)) { return; }
        auto width = static_cast<long>(m_ui_width * m_xscale);
        auto const& cur_line = m_ui_lines[line];
        auto height_1 = static_cast<long>((cur_line.acc_height - cur_line.line_height) * m_yscale);
        auto height_2 = static_cast<long>(cur_line.acc_height * m_yscale);
        check_hresult(m_vsis_noref->Invalidate({ 0, height_1, width, height_2 }));
    }
    void EngineControl::UpdateAndInvalidateActiveButton(Point const& pt) {
        auto line = GetLineIndexFromHeight(pt.Y);
        // Check for buttons
        ActiveButtonData new_active_button;
        if ((pt.X >= 0 && pt.Y >= 0) && line < size(m_ui_lines)) {
            auto const& cur_line = m_ui_lines[line];
            if (!cur_line.buttons.empty()) {
                // Perform hit test on text
                BOOL is_trailing_hit;
                BOOL is_inside;
                DWRITE_HIT_TEST_METRICS hit_test_metrics;
                check_hresult(cur_line.txt_layout->HitTestPoint(
                    pt.X, pt.Y - (cur_line.acc_height - cur_line.line_height),
                    &is_trailing_hit, &is_inside, &hit_test_metrics
                ));
                if (is_inside) {
                    auto cur_pos = hit_test_metrics.textPosition;
                    for (size_t i = 0; i < size(cur_line.buttons); i++) {
                        auto const& cur_btn = cur_line.buttons[i];
                        if (cur_btn.starti <= cur_pos && cur_pos < cur_btn.starti + cur_btn.len) {
                            // Found the target button
                            new_active_button.line = line;
                            new_active_button.button_idx = i;
                            break;
                        }
                    }
                }
            }
        }
        // If we are on a button, request redraw for that (/ those) lines
        if (new_active_button != m_cur_active_button) {
            InvalidateLineAtIndex(m_cur_active_button.line);
            m_cur_active_button = new_active_button;
            InvalidateLineAtIndex(new_active_button.line);
        }
    }
    bool EngineControl::TryFulfillInputRequest(bool clear_input) {
        if (auto& input_req = m_outstanding_input_req) {
            auto input_tb = UserInputTextBox();
            auto input = input_tb.Text();
            if (clear_input) {
                input_tb.Text({});
            }
            if (input_req->try_fulfill(input)) {
                m_input_countdown_timer.Stop();
                input_req = nullptr;

                // Echo back
                if (!input.empty()) { RoutinePrint(input, ERA_PEF_IS_LINE); }

                return true;
            }
        }
        return false;
    }
    ID2D1SolidColorBrush* EngineControl::GetOrCreateSolidColorBrush(uint32_t color) {
        auto& entry = m_brush_map[color];
        if (!entry) {
            check_hresult(m_d2d_ctx->CreateSolidColorBrush(
                D2D1::ColorF(color, (color >> 24) / 255.f), entry.put()));
        }
        return entry.get();
    }
    IDWriteTextFormat* EngineControl::GetOrCreateTextFormat(hstring const& font_family) {
        auto& entry = m_font_map[font_family];
        if (!entry) {
            check_hresult(g_dwrite_factory->CreateTextFormat(
                font_family.c_str(),
                nullptr,
                DWRITE_FONT_WEIGHT_REGULAR,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                (float)m_cfg.font_size,
                L"",
                entry.put()
            ));
        }
        return entry.get();
    }
    void EngineControl::OnInputCountDownTick(IInspectable const&, IInspectable const&) {
        auto& input_req = m_outstanding_input_req;
        if (!input_req) {
            // TODO: Is this unreachable?
            m_input_countdown_timer.Stop();
            return;
        }

        // Check remaining time
        auto cur_t = winrt::clock::now();
        auto remaining_time = std::chrono::milliseconds(input_req->time_limit) -
            (cur_t - m_input_start_t);
        if (remaining_time.count() <= 0) {
            // Expired
            input_req->time_limit = std::chrono::duration_cast<std::chrono::milliseconds>(remaining_time).count();
            tenkai::cpp_utils::ScopeExit se_input_req([&] {
                input_req->try_fulfill_void();
                input_req = nullptr;
            });
            m_input_countdown_timer.Stop();
            if (input_req->show_expiry_msg) {
                auto expire_msg = input_req->expiry_msg;
                if (!expire_msg.empty()) {
                    RoutinePrint(expire_msg, ERA_PEF_IS_LINE);
                    // HACK: Enforce a redraw so that we won't lose texts because of
                    //       engine-thread-queued UI work running too early
                    if (m_auto_redraw) {
                        m_sd->ui_redraw_block_engine.store(true, std::memory_order_relaxed);
                    }
                }
            }
        }
        else {
            // Not expired, print messages if needed
            if (input_req->show_time_prompt) {
                auto prompt = format(L"还剩 {:.1f} 秒",
                    std::chrono::duration<double>(remaining_time).count()
                );
                if (m_input_last_prompt != prompt) {
                    m_input_last_prompt = prompt;
                    RoutineReuseLastLine(prompt);
                    FlushEngineImageOutputLayout(false);
                }
            }
        }
    }
    void EngineControl::FlushCurPrintLine() {
        if (!m_cur_composing_line.parts.empty()) {
            RoutinePrint({}, ERA_PEF_IS_LINE);
        }
    }
    void EngineControl::RoutinePrint(hstring content, PrintExtendedFlags flags) {
        if (GetEffectiveSkipDisplay()) {
            // Engine requested to skip output
            return;
        }

        bool updated = false;

        auto push_cur_composing_line_fn = [this, &updated] {
            // Handle reused last line
            if (m_reused_last_line) {
                RoutineClearLine(1);
                m_reused_last_line = false;
            }

            auto height = GetCalculatedUIHeight();

            com_ptr<IDWriteTextFormat> txt_fmt;
            try {
                txt_fmt.copy_from(GetOrCreateTextFormat(m_cur_font_name));
            }
            catch (...) {
                // Font does not exist, fallback
                m_cur_font_name = m_default_font_name;
                txt_fmt.copy_from(GetOrCreateTextFormat(m_default_font_name));
            }

            m_ui_lines.emplace_back(hstring{}, txt_fmt);
            auto& cur_line = m_ui_lines.back();
            uint32_t implicit_button_scan_start{};
            auto materialize_implicit_buttons_fn = [&] {
                if (implicit_button_scan_start >= cur_line.txt.size()) { return; }
                std::wstring_view cur_str{ cur_line.txt };
                cur_str = cur_str.substr(implicit_button_scan_start);

                // PRECONDITION: it != rend(vec)
                auto vec_idx_from_rev_it_fn = [](auto const& vec, auto const& it) {
                    return (uint32_t)(std::distance(it, vec.rend()) - 1);
                };
                struct FindResult {
                    uint32_t start_pos, end_pos;
                    int64_t value;
                };
                // Corresponding regex: \[ *-?\d+ *\]
                auto find_fn = [&]() -> std::optional<FindResult> {
                    if (cur_str.empty()) { return std::nullopt; }
                    auto ib = rbegin(cur_str), ie = rend(cur_str);
                    auto it = ib;
#define FIND_ADVANCE(it) do { if (++(it) == ie) { return std::nullopt; } } while (0)
                    while (true) {
                        while (*it != L']') { FIND_ADVANCE(it); }
                        // Read ']'
                        auto it_end = it;
                        FIND_ADVANCE(it);
                        while (*it == L' ') { FIND_ADVANCE(it); }
                        // Read digits
                        if (!(L'0' <= *it && *it <= L'9')) { continue; }
                        auto it_num_end = it;
                        while (L'0' <= *it && *it <= L'9') { FIND_ADVANCE(it); }
                        // Read '-'
                        if (*it == L'-') { FIND_ADVANCE(it); }
                        auto it_num_start = it - 1;
                        while (*it == L' ') { FIND_ADVANCE(it); }
                        // Read '['
                        if (*it != L'[') { continue; }
                        // Accept
                        auto num_start_pos = vec_idx_from_rev_it_fn(cur_str, it_num_start);
                        auto num_end_pos = vec_idx_from_rev_it_fn(cur_str, it_num_end) + 1;
                        auto num_len = num_end_pos - num_start_pos;
                        if (auto value = parse<int64_t>(cur_str.substr(num_start_pos, num_len))) {
                            return FindResult{
                                .start_pos = vec_idx_from_rev_it_fn(cur_str, it),
                                .end_pos = vec_idx_from_rev_it_fn(cur_str, it_end) + 1,
                                .value = *value,
                            };
                        }
                        // Value overflowed; don't treat it as a valid button
                        continue;
                    }
#undef FIND_ADVANCE
                };
                auto opt_find_result = find_fn();
                while (opt_find_result) {
                    auto& find_result = *opt_find_result;
                    EngineUIPrintLineDataButton btn_data{
                        .starti = find_result.start_pos,
                        .len = find_result.end_pos - find_result.start_pos,
                        .data = EngineUIPrintLineDataButton::InputButton{ to_hstring(find_result.value) }
                    };
                    if (find_result.end_pos != cur_str.size()) {
                        // Button `[D] XXX`
                        btn_data.len = cur_str.size() - find_result.start_pos;
                        btn_data.starti += implicit_button_scan_start;
                        cur_line.buttons.push_back(std::move(btn_data));

                        cur_str = cur_str.substr(0, find_result.start_pos);
                        opt_find_result = find_fn();
                    }
                    else {
                        // Button `XXX [D]`
                        cur_str = cur_str.substr(0, find_result.start_pos);
                        opt_find_result = find_fn();
                        if (opt_find_result) {
                            cur_str = cur_str.substr(0, opt_find_result->end_pos);
                            auto endi = btn_data.starti + btn_data.len;
                            btn_data.starti = opt_find_result->end_pos;
                            btn_data.len = endi - btn_data.starti;
                        }
                        else {
                            auto endi = btn_data.starti + btn_data.len;
                            btn_data.starti = 0;
                            btn_data.len = endi;
                        }
                        btn_data.starti += implicit_button_scan_start;
                        cur_line.buttons.push_back(std::move(btn_data));
                    }
                }

                implicit_button_scan_start = cur_line.txt.size();
            };
            for (auto& part : m_cur_composing_line.parts) {
                if (part.forbid_button || part.is_isolated) {
                    // End current part and handle buttons
                    materialize_implicit_buttons_fn();
                }

                auto starti = cur_line.txt.size();
                // TODO: Improve string concatenation performance
                cur_line.txt = cur_line.txt + part.str;
                cur_line.effects.push_back({ .starti = starti, .len = part.str.size(),
                    .color = part.color, .style = m_cur_font_style });
                if (part.explicit_buttons) {
                    cur_line.buttons.push_back(std::move(*part.explicit_buttons));
                }

                // Handle part flags
                if (part.forbid_button) {
                    // Skip current part
                    implicit_button_scan_start = cur_line.txt.size();
                }
                if (part.is_isolated) {
                    // Finish current part early
                    materialize_implicit_buttons_fn();
                }
            }
            materialize_implicit_buttons_fn();
            // Reset composing line
            m_cur_composing_line = {};

            cur_line.ensure_layout(this, m_ui_width);
            height += cur_line.line_height;
            cur_line.acc_height = height;
            cur_line.flush_effects(this);

            m_cur_printc_count = 0;

            updated = true;
        };

        //auto color = to_u32(EngineForeColor());
        uint32_t color;
        if (flags & ERA_PEF_IGNORE_COLOR) {
            color = to_u32(unbox_value<Color>(
                m_EngineForeColorProperty
                .GetMetadata(xaml_typename<MEraEmuWin::EngineControl>())
                .DefaultValue()
            ));
        }
        else {
            color = to_u32(EngineForeColor());
        }
        bool forbid_button{};
        bool is_isolated{};
        if (flags & ERA_PEF_IS_SINGLE) {
            if (!m_cur_composing_line.parts.empty()) {
                push_cur_composing_line_fn();
            }
        }
        if (flags & (ERA_PEF_LEFT_PAD | ERA_PEF_RIGHT_PAD)) {
            // PRINTC / PRINTLC
            is_isolated = true;

            bool align_right = flags & ERA_PEF_RIGHT_PAD;
            auto str_width = rust_get_wstring_width((const uint16_t*)content.c_str());
            if (str_width < m_cfg.printc_char_count) {
                int space_cnt = m_cfg.printc_char_count - str_width;
                if (align_right) {
                    content = format(L"{:{}}{}", L"", space_cnt, content);
                }
                else {
                    content = format(L"{}{:{}}", content, L"", space_cnt);
                }
            }
            m_cur_printc_count++;
        }
        if (flags & ERA_PEF_FORCE_PLAIN) {
            forbid_button = true;
        }
        std::wstring_view content_sv{ content };
        size_t newline_pos;
        while ((newline_pos = content_sv.find(L'\n')) != content_sv.npos) {
            auto subsv = content_sv.substr(0, newline_pos);
            content_sv = content_sv.substr(newline_pos + 1);
            if (!subsv.empty()) {
                m_cur_composing_line.parts.push_back({
                    .str = hstring(subsv), .color = color,
                    .forbid_button = forbid_button, .is_isolated = is_isolated });
            }
            push_cur_composing_line_fn();
        }
        if (!content_sv.empty()) {
            m_cur_composing_line.parts.push_back({
                .str = hstring(content_sv), .color = color,
                .forbid_button = forbid_button, .is_isolated = is_isolated });
        }
        if (flags & ERA_PEF_IS_LINE || m_cur_printc_count >= m_cfg.printc_per_line) {
            push_cur_composing_line_fn();
        }
        else if ((flags & ERA_PEF_IS_SINGLE) && !m_cur_composing_line.parts.empty()) {
            push_cur_composing_line_fn();
        }

        // Resize to trigger updates
        if (updated && m_auto_redraw) {
            FlushEngineImageOutputLayout(false);
        }
    }
    void EngineControl::RoutineHtmlPrint(hstring const& content) {
        // TODO: RoutineHtmlPrint
        RoutinePrint(content, ERA_PEF_IS_LINE);
    }
    void EngineControl::RoutineInput(std::unique_ptr<InputRequest> request) {
        assert(request);

        if (!m_engine_task_tx.is_vacant()) {
            // Abandon the input request until the engine has processed the task we sent
            return;
        }

        // Flush intermediate print contents first
        FlushCurPrintLine();
        FlushEngineImageOutputLayout(false);

        m_input_start_t = winrt::clock::now();
        m_input_last_prompt = {};
        m_input_countdown_timer.Stop();
        auto& input_req = m_outstanding_input_req;
        input_req = std::move(request);
        // Break user skip if requested
        if (input_req->break_user_skip) {
            m_user_skipping = false;
        }
        // Check if we can fulfill the input request immediately
        if (input_req->is_one) {
            if (TryFulfillInputRequest(false)) {
                UserInputTextBox().Text({});
                return;
            }
        }
        // If user is skipping, skip current request
        if (m_user_skipping && input_req->can_skip) {
            std::exchange(input_req, nullptr)->try_fulfill_void();
            return;
        }
        else {
            m_user_skipping = false;
        }
        // Handle timed input
        if (input_req->time_limit >= 0) {
            // TODO: More precise countdown handling
            if (input_req->time_limit > 0) {
                m_input_countdown_timer.Start();
            }
            OnInputCountDownTick(nullptr, nullptr);
        }
    }
    void EngineControl::RoutineReuseLastLine(hstring const& content) {
        FlushCurPrintLine();

        if (m_reused_last_line) {
            RoutineClearLine(1);
            m_reused_last_line = false;
        }
        // TODO: Cut off newlines in content
        RoutinePrint(content, ERA_PEF_IS_LINE);
        // Mark as reused last line
        m_reused_last_line = true;
    }
    void EngineControl::RoutineClearLine(uint64_t count) {
        if (count == 0) { return; }
        auto old_count = (uint64_t)size(m_ui_lines);
        if (count >= old_count) {
            m_ui_lines.clear();
        }
        else {
            auto ie = end(m_ui_lines);
            m_ui_lines.erase(ie - count, ie);
        }
        if (m_auto_redraw) {
            FlushEngineImageOutputLayout(false);
        }
        else {
            m_last_redraw_dirty_height = std::min(m_last_redraw_dirty_height, GetCalculatedUIHeight());
        }
    }
    void EngineControl::RoutinePrintSourceButton(hstring const& content, hstring const& path,
        uint32_t line, uint32_t column, PrintExtendedFlags flags
    ) {
        if (content.empty()) { return; }
        uint32_t ui_line_start = size(m_ui_lines);
        uint32_t ui_line_offset{};
        for (auto const& part : m_cur_composing_line.parts) {
            ui_line_offset += part.str.size();
        }
        RoutinePrint(content, flags | ERA_PEF_FORCE_PLAIN);
        // Apply to composed lines
        for (size_t i = ui_line_start; i < size(m_ui_lines); i++) {
            auto& line_data = m_ui_lines[i];
            line_data.buttons.push_back(
                EngineUIPrintLineDataButton{
                    .starti = ui_line_offset,
                    .len = line_data.txt.size() - ui_line_offset,
                    .data = EngineUIPrintLineDataButton::SourceButton{ path, line, column },
                }
                );
            ui_line_offset = 0;
        }
        // Apply to current composing line
        if (!m_cur_composing_line.parts.empty()) {
            auto& part = m_cur_composing_line.parts.back();
            part.explicit_buttons = EngineUIPrintLineDataButton{
                .starti = ui_line_offset,
                .len = part.str.size(),
                .data = EngineUIPrintLineDataButton::SourceButton{ path, line, column },
            };
        }
    }
    void EngineControl::RoutinePrintButton(hstring const& content, hstring const& value, PrintExtendedFlags flags) {
        if (content.empty()) { return; }
        uint32_t ui_line_start = size(m_ui_lines);
        uint32_t ui_line_offset{};
        for (auto const& part : m_cur_composing_line.parts) {
            ui_line_offset += part.str.size();
        }
        RoutinePrint(content, flags | ERA_PEF_FORCE_PLAIN);
        // Apply to composed lines
        for (size_t i = ui_line_start; i < size(m_ui_lines); i++) {
            auto& line_data = m_ui_lines[i];
            line_data.buttons.push_back(
                EngineUIPrintLineDataButton{
                    .starti = ui_line_offset,
                    .len = line_data.txt.size() - ui_line_offset,
                    .data = EngineUIPrintLineDataButton::InputButton{ value },
                }
                );
            ui_line_offset = 0;
        }
        // Apply to current composing line
        if (!m_cur_composing_line.parts.empty()) {
            auto& part = m_cur_composing_line.parts.back();
            part.explicit_buttons = EngineUIPrintLineDataButton{
                .starti = ui_line_offset,
                .len = part.str.size(),
                .data = EngineUIPrintLineDataButton::InputButton{ value },
            };
        }
    }

    void EngineControl::SetCurrentLineAlignment(int64_t value) {
        m_cur_line_alignment = (uint32_t)value;
    }
    int64_t EngineControl::GetCurrentLineAlignment() {
        return m_cur_line_alignment;
    }
    void EngineControl::SetCurrentFontStyle(int64_t value) {
        m_cur_font_style = (uint32_t)value;
    }
    int64_t EngineControl::GetCurrentFontStyle() {
        return m_cur_font_style;
    }
    void EngineControl::SetCurrentFontName(hstring const& value) {
        // TODO: Verify font name?
        m_cur_font_name = value.empty() ? m_default_font_name : value;
    }
    hstring EngineControl::GetCurrentFontName() {
        return m_cur_font_name;
    }
    void EngineControl::SetRedrawState(int64_t value) {
        if (value == 0) {
            m_auto_redraw = false;
        }
        else if (value == 1) {
            m_auto_redraw = true;
        }
        else {
            m_auto_redraw = true;
            FlushEngineImageOutputLayout(false);
        }
    }
    int64_t EngineControl::GetRedrawState() {
        return m_auto_redraw;
    }
    void EngineControl::SetSkipDisplay(int64_t value) {
        if (value == 0) {
            m_auto_redraw = false;
        }
        else {
            m_auto_redraw = true;
        }
    }
    int64_t EngineControl::GetSkipDisplay() {
        return m_skip_display;
    }
    int64_t EngineControl::GetEffectiveSkipDisplay() {
        return m_no_skip_display_cnt > 0 ? false : GetSkipDisplay();
    }
    void EngineControl::PushNoSkipDisplay() {
        if (m_no_skip_display_cnt++ == 0) {
            // Do nothing
        }
    }
    void EngineControl::PopNoSkipDisplay() {
        if (m_no_skip_display_cnt == 0) {
            // TODO: Treat extra pops as an error?
            return;
        }
        if (--m_no_skip_display_cnt == 0) {
            // Do nothing
        }
    }

#define DP_CLASS EngineControl
    DP_DEFINE(EngineForeColor, box_value(Windows::UI::Colors::Silver()));
    DP_DEFINE(EngineBackColor, box_value(Windows::UI::Colors::Black()));
    DP_DEFINE(EngineTitle, box_value(L"MEraEmu"), [](DependencyObject const& sender, DependencyPropertyChangedEventArgs const& e) {
        auto that = sender.as<EngineControl>();
        that->UpdateDevToolsWindow();
    });
    DP_DEFINE_METHOD(EngineForeColor, Color);
    DP_DEFINE_METHOD(EngineBackColor, Color);
    DP_DEFINE_METHOD(EngineTitle, hstring const&);
#undef DP_CLASS
}
