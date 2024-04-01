﻿#include "pch.h"
#include "EngineControl.h"
#if __has_include("EngineControl.g.cpp")
#include "EngineControl.g.cpp"
#endif

#include "EngineUnhandledExceptionEventArgs.g.cpp"

#include "MEraEngine.hpp"

#include "Tenkai.hpp"

#include <random>
#include <fstream>
#include <filesystem>
#include <variant>

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
    std::optional<T> parse(hstring const& str) noexcept {
        if (str.empty()) { return std::nullopt; }
        hstring::size_type n = str.size();
        hstring::size_type i;
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
        if (size >= 3) {
            check_bool(ReadFile(file.get(), buf, 3, &read_size, nullptr));
            assert(read_size == 3);
            buf_offset += 3;
            size -= 3;
            // Strip BOM header
            if (buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf) {
                buf_offset = 0;
            }
        }
        check_bool(ReadFile(file.get(), buf + buf_offset, size, &read_size, nullptr));
        assert(read_size == size);
        buf_offset += size;
        size = 0;
        // HACK: Append trailing newline
        buf[buf_offset] = L'\n';
        buf_offset++;

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

        //virtual void time_tick() = 0;
        virtual bool try_fulfill(hstring const& input) = 0;
    };
    struct InputRequestI : InputRequest {
        std::optional<int64_t> default_value;
        std::promise<int64_t> promise;

        using InputRequest::InputRequest;

        ~InputRequestI() {
            if (default_value) {
                promise.set_value(*default_value);
            }
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
            if (default_value) {
                promise.set_value(*default_value);
            }
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
            promise.set_value();
        }
        bool try_fulfill(hstring const& input) override {
            return true;
        }
    };

    enum class EngineThreadTaskKind {
        None,
        ReturnToTitle,
    };
    struct EngineThreadTask {
        EngineThreadTask(EngineThreadTaskKind kind) : kind(kind) {}
        virtual ~EngineThreadTask() {}

        EngineThreadTaskKind kind;
        //virtual void do_work();
    };

    enum class EngineThreadState {
        Died,
        Vacant,
        Occupied,
    };

    struct EngineSharedData : std::enable_shared_from_this<EngineSharedData> {
        ~EngineSharedData() {}

        hstring game_base_dir;
        CoreDispatcher ui_dispatcher{ nullptr };
        DispatcherQueue ui_dispatcher_queue{ nullptr };
        //weak_ref<EngineControl> ui_ctrl{ nullptr };
        EngineControl* ui_ctrl{};
        IAsyncAction thread_task_op{ nullptr };
        std::atomic_bool ui_is_alive{ false };
        std::atomic_bool ui_redraw_block_engine{ false };
        // Also used as an event queue indicator (?)
        std::atomic_bool engine_stop_flag{ true };
        //std::atomic_bool thread_is_alive{ false };
        std::atomic<EngineThreadState> thread_state{ EngineThreadState::Died };
        std::atomic_bool thread_started{ false };
        std::exception_ptr thread_exception{ nullptr };
        //std::atomic<EngineThreadTask*> thread_task{};
        std::unique_ptr<EngineThreadTask> thread_task;
        bool has_execution_error{ false };

        std::mutex ui_mutex;
        std::vector<std::move_only_function<void()>> ui_works;

        // NOTE: Called by UI thread
        void ui_disconnect() {
            if (thread_task_op) {
                thread_task_op.Cancel();
            }
            engine_stop_flag.store(true, std::memory_order_relaxed);
            ui_is_alive.store(false, std::memory_order_relaxed);
            ui_is_alive.notify_one();
            // Abandon queued works
            [&] {
                std::scoped_lock guard(ui_mutex);
                return std::exchange(ui_works, {});
            }();
            ui_redraw_block_engine.store(false, std::memory_order_relaxed);
            ui_redraw_block_engine.notify_one();
            queue_thread_work(EngineThreadTaskKind::None);
        }
        // NOTE: Called by engine (background) thread
        void task_disconnect() {
            auto state = thread_state.exchange(EngineThreadState::Died, std::memory_order_relaxed);
            thread_state.notify_one();
            // Triggers UI update, which in turn responds to task disconnection
            queue_ui_work([] {});
            if (state == EngineThreadState::Occupied) {
                engine_stop_flag.wait(false, std::memory_order_acquire);
                thread_task = nullptr;
                engine_stop_flag.store(false, std::memory_order_release);
                engine_stop_flag.notify_one();
            }
        }
        void queue_ui_work(std::move_only_function<void()> work) {
            if (!ui_is_alive.load(std::memory_order_relaxed)) {
                //throw hresult_canceled();
                // Fail silently
                return;
            }
            bool has_no_work;
            {
                std::scoped_lock guard(ui_mutex);
                has_no_work = ui_works.empty();
                ui_works.push_back(std::move(work));
            }
            if (has_no_work) {
                // Wake up UI thread
                ui_redraw_block_engine.wait(true, std::memory_order_relaxed);
                // NOTE: CoreDispatcher.RunAsync will cause memory leak, so use
                //       DispatcherQueue.TryEnqueue instead
                ui_dispatcher_queue.TryEnqueue(DispatcherQueuePriority::Low, [self = shared_from_this()] {
                    if (!self->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                    // SAFETY: We are in the UI thread; no one could be destructing
                    //         EngineControl.
                    self->ui_ctrl->UpdateEngineUI();
                });
            }
        }
        void queue_thread_work(EngineThreadTaskKind kind) {
            auto state = EngineThreadState::Vacant;
            while (!thread_state.compare_exchange_weak(state, EngineThreadState::Occupied, std::memory_order_acq_rel)) {
                if (state == EngineThreadState::Died) { return; }
                // TODO: Is wait unnecessary?
                thread_state.wait(state, std::memory_order_relaxed);
                state = EngineThreadState::Vacant;
            }
            thread_task = std::make_unique<EngineThreadTask>(kind);
            // Interrupt engine thread so that it can process queued works
            engine_stop_flag.store(true, std::memory_order_release);
            engine_stop_flag.notify_one();
            // SAFETY: Stop flag is reverted even when engine thread is disconnecting
            engine_stop_flag.wait(true, std::memory_order_acquire);
            // Task was processed by engine thread, we can return now
        }
        void wait_for_thread_disconnect() {
            while (true) {
                auto state = thread_state.load(std::memory_order_acquire);
                if (state == EngineThreadState::Died) { break; }
                thread_state.wait(state, std::memory_order_relaxed);
            }
        }
    };

    struct MEraEmuWinEngineSysCallback : ::MEraEngineSysCallback {
        // SAFETY: Engine is destructed before EngineSharedData
        MEraEmuWinEngineSysCallback(EngineSharedData* sd) : m_sd(sd) {}

        void on_compile_error(EraScriptErrorInfo const& info) override {
            auto filename = to_hstring(info.filename);
            auto src_info = info.src_info;
            auto is_error = info.is_error;
            auto msg = to_hstring(info.msg);
            auto final_msg = format(L"{}({},{}): 编译{}: {}",
                filename, src_info.line, src_info.column,
                is_error ? L"错误" : L"警告", msg
            );
            auto msg_clr = is_error ? Colors::Red() : Colors::Yellow();
            m_sd->queue_ui_work([=, sd = m_sd] {
                auto old_clr = sd->ui_ctrl->EngineForeColor();
                sd->ui_ctrl->EngineForeColor(msg_clr);
                sd->ui_ctrl->RoutinePrintSourceButton(
                    final_msg, filename,
                    src_info.line, src_info.column,
                    ERA_PEF_IS_LINE
                );
                sd->ui_ctrl->EngineForeColor(old_clr);
            });
        }
        void on_execute_error(EraScriptErrorInfo const& info) override {
            auto filename = to_hstring(info.filename);
            auto src_info = info.src_info;
            auto is_error = info.is_error;
            auto msg = to_hstring(info.msg);
            auto final_msg = format(L"{}({},{}): 运行{}: {}",
                filename, src_info.line, src_info.column,
                is_error ? L"错误" : L"警告", msg
            );
            auto msg_clr = is_error ? Colors::Red() : Colors::Yellow();
            m_sd->queue_ui_work([=, sd = m_sd] {
                auto old_clr = sd->ui_ctrl->EngineForeColor();
                sd->ui_ctrl->EngineForeColor(msg_clr);
                sd->ui_ctrl->RoutinePrintSourceButton(
                    final_msg, filename,
                    src_info.line, src_info.column,
                    ERA_PEF_IS_LINE
                );
                sd->ui_ctrl->EngineForeColor(old_clr);
            });
            // SAFETY: Engine and callback are on the same thread
            if (is_error) {
                m_sd->has_execution_error = true;
            }
            //m_sd->engine_stop_flag.store(true, std::memory_order_relaxed);
        }
        uint64_t on_get_rand() override {
            return m_rand_gen();
        }
        void on_print(std::string_view content, PrintExtendedFlags flags) override {
            m_sd->queue_ui_work([sd = m_sd, content = to_hstring(content), flags] {
                sd->ui_ctrl->RoutinePrint(content, flags);
            });
            if (flags & ERA_PEF_IS_WAIT) {
                on_wait(true, false);
            }
        }
        void on_html_print(std::string_view content) override {
            m_sd->queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutineHtmlPrint(content);
            });
        }
        void on_wait(bool any_key, bool is_force) override {
            int64_t time_limit{ -1 };
            auto input_req = std::make_unique<InputRequestVoid>(time_limit);
            input_req->can_skip = true;
            input_req->break_user_skip = is_force;
            auto future = input_req->promise.get_future();
            m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
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
            m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                sd->ui_ctrl->RoutineInput(std::move(input_req));
            });
            return future.get();
        }
        std::optional<int64_t> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestI>(time_limit);
                input_req->default_value = default_value;
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                auto future = input_req->promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return future.get();
            }
            catch (std::future_error const& e) { return std::nullopt; }
        }
        const char* on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestS>(time_limit);
                input_req->default_value = default_value.transform([](auto x) { return to_hstring(x); });
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                auto future = input_req->promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            catch (std::future_error const& e) { return nullptr; }
        }
        std::optional<int64_t> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
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
                auto future = input_req->promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return future.get();
            }
            catch (std::future_error const& e) { return std::nullopt; }
        }
        const char* on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
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
                auto future = input_req->promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            catch (std::future_error const& e) { return nullptr; }
        }
        std::optional<int64_t> on_oneinput_int(std::optional<int64_t> default_value) override
        {
            return std::optional<int64_t>();
        }
        const char* on_oneinput_str(std::optional<std::string_view> default_value) override
        {
            return nullptr;
        }
        std::optional<int64_t> on_toneinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override
        {
            return std::optional<int64_t>();
        }
        const char* on_toneinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override
        {
            return nullptr;
        }
        void on_reuselastline(std::string_view content) override {
            m_sd->queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutineReuseLastLine(content);
            });
        }
        void on_clearline(int64_t count) override {
            if (count <= 0) { return; }
            m_sd->queue_ui_work([sd = m_sd, count = (uint64_t)count] {
                sd->ui_ctrl->RoutineClearLine(count);
            });
        }
        int64_t on_var_get_int(std::string_view name, uint32_t idx) override {
            if (name == "@COLOR") {
                std::promise<uint32_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
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
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
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
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentFontStyle());
                });
                return future.get();
            }
            if (name == "@REDRAW") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetRedrawState());
                });
                return future.get();
            }
            if (name == "@ALIGN") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
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
                // TODO: @SKIPDISP
                return 0;
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
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.printc_per_line);
                });
                return future.get();
            }
            if (name == "@PRINTCLENGTH") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.printc_char_count);
                });
                return future.get();
            }
            if (name == "@LINEISEMPTY") {
                std::promise<bool> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cur_composing_line.parts.empty());
                });
                return future.get();
            }
            if (name == "SCREENWIDTH") {
                // TODO: SCREENWIDTH
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_ui_width / 20);
                });
                return future.get();
            }
            if (name == "SCREENPIXELWIDTH") {
                // NOTE: Returns width in logical pixels
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_ui_width);
                });
                return future.get();
            }
            if (name == "LINECOUNT") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value((int64_t)sd->ui_ctrl->m_ui_lines.size());
                });
                return future.get();
            }
            throw std::exception("no such variable");
        }
        const char* on_var_get_str(std::string_view name, uint32_t idx) override {
            if (name == "@FONT") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentFontName());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "WINDOW_TITLE") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->EngineTitle());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "DRAWLINESTR") {
                std::promise<std::string> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
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
        void on_var_set_int(std::string_view name, uint32_t idx, int64_t val) override {
            if (name == "@COLOR") {
                m_sd->queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineForeColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                return;
            }
            if (name == "@BGCOLOR") {
                m_sd->queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineBackColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                return;
            }
            if (name == "@STYLE") {
                m_sd->queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetCurrentFontStyle(val);
                });
                return;
            }
            if (name == "@REDRAW") {
                m_sd->queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetRedrawState(val);
                });
                return;
            }
            if (name == "@ALIGN") {
                m_sd->queue_ui_work([sd = m_sd, val]() {
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
            if (name == "@ANIMETIMER") {
                // TODO: @ANIMETIMER
                return;
            }
            // TODO: Prohibit setting variables @DEF*COLOR?
            // TODO...
            throw std::exception("no such variable");
        }
        void on_var_set_str(std::string_view name, uint32_t idx, std::string_view val) override {
            if (name == "@FONT") {
                m_sd->queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->SetCurrentFontName(val);
                });
                return;
            }
            if (name == "WINDOW_TITLE") {
                m_sd->queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->EngineTitle(val);
                });
                return;
            }
            throw std::exception("no such variable");
        }
        void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) override {
            m_sd->queue_ui_work([sd = m_sd, content = to_hstring(content), value = to_hstring(value), flags] {
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
                ~Win32File() {
                    auto pos = tell();
                    std::format("{}", pos);
                }

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
                void seek(int64_t pos, MEraEngineFileSeekMode mode) override {
                    int native_mode = 0;
                    switch (mode) {
                    case M_ERA_ENGINE_FILE_SEEK_MODE_START:
                        native_mode = FILE_BEGIN;
                        break;
                    case M_ERA_ENGINE_FILE_SEEK_MODE_END:
                        native_mode = FILE_END;
                        break;
                    case M_ERA_ENGINE_FILE_SEEK_MODE_CURRENT:
                        native_mode = FILE_CURRENT;
                        break;
                    default:
                        std::terminate();
                    }
                    LONG high = (LONG)(pos >> 32);
                    LONG low = (LONG)pos;
                    auto r = SetFilePointer(m_fh.get(), low, &high, native_mode);
                    //check_bool(r != INVALID_SET_FILE_POINTER);
                }
                uint64_t tell() override {
                    LONG high{};
                    auto low = SetFilePointer(m_fh.get(), 0, &high, FILE_CURRENT);
                    //check_bool(low != INVALID_SET_FILE_POINTER);
                    return low + (static_cast<uint64_t>(high) << 32);
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
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.line_height);
                });
                return future.get();
            }
            if (name == "フォントサイズ") {
                std::promise<int64_t> promise;
                auto future = promise.get_future();
                m_sd->queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->m_cfg.font_size);
                });
                return future.get();
            }
            if (name == "ウィンドウ幅") {
                return on_var_get_int("SCREENPIXELWIDTH", 0);
            }
            throw std::runtime_error(std::format("no such int config: {}", name));
        }
        const char* on_get_config_str(std::string_view name) override {
            // TODO: on_get_config_str
            if (name == "描画インターフェース") {
                return "Direct2D+VSIS";
            }
            throw std::runtime_error(std::format("no such str config: {}", name));
        }
        int64_t on_get_key_state(int64_t key_code) override
        {
            return 0;
        }

    private:
        EngineSharedData* const m_sd;
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
            txt(txt), txt_fmt(txt_fmt) {}

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
        if (m_sd) {
            m_outstanding_input_req = nullptr;
            m_sd->ui_disconnect();
            m_sd->wait_for_thread_disconnect();
        }
    }
    void EngineControl::InitializeComponent() {
        EngineControlT::InitializeComponent();

        // Register for scale notification
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
        // And width notification
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
                pt.Y += root_sv.VerticalOffset();
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
        m_sd->queue_thread_work(EngineThreadTaskKind::ReturnToTitle);

        m_ui_lines.clear();
        check_hresult(m_vsis_noref->Resize(0, 0));
    }
    bool EngineControl::IsStarted() {
        return m_sd && m_sd->thread_state.load(std::memory_order_relaxed) != EngineThreadState::Died;
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
                auto input_button_fn = [](EngineUIPrintLineDataButton::InputButton const& v) -> fire_and_forget {
                    // TODO: InputButton
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
        }
    }
    void EngineControl::UserInputTextBox_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        if (e.Key() == VirtualKey::Enter) {
            e.Handled(true);

            // Try to fulfill input requests
            if (auto& input_req = m_outstanding_input_req) {
                auto input_tb = UserInputTextBox();
                auto input = input_tb.Text();
                input_tb.Text({});
                if (input_req->try_fulfill(input)) {
                    m_input_countdown_timer.Stop();
                    input_req = nullptr;
                    // Echo back
                    if (!input.empty()) { RoutinePrint(input, ERA_PEF_IS_LINE); }
                }
            }
        }
    }
    void EngineControl::Bootstrap(hstring const& game_base_dir) try {
        if (m_sd) {
            m_outstanding_input_req = nullptr;
            m_sd->ui_disconnect();
        }
        m_sd = std::make_shared<EngineSharedData>();
        m_sd->game_base_dir = game_base_dir;
        m_sd->ui_dispatcher = Dispatcher();
        m_sd->ui_dispatcher_queue = DispatcherQueue::GetForCurrentThread();
        //m_sd->ui_ctrl = get_weak();
        m_sd->ui_ctrl = this;
        // Start a dedicated background thread
        m_sd->thread_task_op = ThreadPool::RunAsync([sd = m_sd](IAsyncAction const& op) {
            SetThreadDescription(GetCurrentThread(), L"MEraEmu Engine Thread");

            sd->thread_state.store(EngineThreadState::Vacant, std::memory_order_relaxed);
            sd->thread_state.notify_one();

            auto mark_thread_started = [&] {
                sd->thread_started.store(true, std::memory_order_release);
                sd->thread_started.notify_one();
            };

            tenkai::cpp_utils::ScopeExit se_thread([&] {
                sd->task_disconnect();
                mark_thread_started();
            });

            try {
                sd->ui_is_alive.wait(false, std::memory_order_acquire);

                MEraEngine engine;
                MEraEmuWinEngineSysCallback* callback;
                {
                    auto callback_box = std::make_unique<MEraEmuWinEngineSysCallback>(sd.get());
                    callback = callback_box.get();
                    engine.install_sys_callback(std::move(callback_box));
                }

                // Register global variables
                // Engine -- register int
                auto eri = [&](const char* name) {
                    engine.register_global_var(name, false, 1, true);
                };
                // Engine -- register str
                auto ers = [&](const char* name) {
                    engine.register_global_var(name, true, 1, true);
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

                auto run_ui_task = [&](std::unique_ptr<EngineThreadTask> task, bool loaded) {
                    if (!loaded) { return; }
                    if (task->kind == EngineThreadTaskKind::ReturnToTitle) {
                        // TODO: Reset execution to title
                        EraFuncInfo func_info;
                        try {
                            func_info = engine.get_func_info("SYSPROC_BEGIN_TITLE");
                        }
                        // Ignore if function does not exist
                        catch (...) { return; }
                        if (func_info.args_cnt != 0) {
                            throw hresult_error(E_FAIL, L"malformed entry function");
                        }
                        engine.reset_exec_to_ip(func_info.entry);
                    }
                };

                // Collect files used by engine
                {
                    std::vector<std::filesystem::path> misc_csvs;
                    std::vector<std::filesystem::path> chara_csvs;
                    auto load_csv = [&](std::filesystem::path const& csv, EraCsvLoadKind kind) {
                        auto [data, size] = read_utf8_file(csv);
                        engine.load_csv(to_string(csv.c_str()).c_str(), { data.get(), size }, kind);
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

                    auto try_handle_thread_event = [&] {
                        if (!sd->engine_stop_flag.load(std::memory_order_acquire)) { return; }
                        if (sd->thread_state.load(std::memory_order_relaxed) != EngineThreadState::Occupied) {
                            // Likely the UI side has just disconnected
                            return;
                        }
                        tenkai::cpp_utils::ScopeExit se_stop_flag([&] {
                            sd->engine_stop_flag.store(false, std::memory_order_release);
                            sd->engine_stop_flag.notify_one();
                            sd->thread_state.store(EngineThreadState::Vacant, std::memory_order_release);
                            sd->thread_state.notify_one();
                        });
                        if (sd->thread_task) {
                            run_ui_task(std::move(sd->thread_task), false);
                        }
                    };

                    // Load ERB files
                    for (auto& erh : erhs) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto [data, size] = read_utf8_file(erh);
                        engine.load_erh(to_string(erh.c_str()).c_str(), { data.get(), size });
                    }
                    for (auto& erb : erbs) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto [data, size] = read_utf8_file(erb);
                        engine.load_erb(to_string(erb.c_str()).c_str(), { data.get(), size });
                    }
                }

                // Finialize compilation
                if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                engine.finialize_load_srcs();

                // Disable auto redraw before running VM
                sd->queue_ui_work([sd] {
                    sd->ui_ctrl->SetRedrawState(0);
                });

                // Main loop
                while (sd->ui_is_alive.load(std::memory_order_relaxed)) {
                    while (!engine.get_is_halted()) {
                        if (sd->engine_stop_flag.load(std::memory_order_relaxed)) {
                            break;
                        }
                        engine.do_execution(&sd->engine_stop_flag, UINT64_MAX);
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
                        for (const auto& frame : stack_trace.frames) {
                            auto path = to_hstring(frame.file_name);
                            auto msg = winrt::format(L"  {}({},{}):{}",
                                path,
                                frame.src_info.line,
                                frame.src_info.column,
                                to_hstring(frame.func_name)
                            );
                            print_msgs.emplace_back(
                                path,
                                frame.src_info.line, frame.src_info.column,
                                std::move(msg)
                            );
                        }
                        sd->queue_ui_work([sd, msgs = std::move(print_msgs)] {
                            sd->ui_ctrl->RoutinePrint(L"函数堆栈跟踪 (最近调用者最先显示):", ERA_PEF_IS_LINE);
                            for (auto const& msg : msgs) {
                                sd->ui_ctrl->RoutinePrintSourceButton(msg.msg, msg.path,
                                    msg.line, msg.column, ERA_PEF_IS_LINE);
                            }
                            sd->ui_ctrl->SetRedrawState(2);
                        });
                    }
                    sd->engine_stop_flag.wait(false, std::memory_order_acquire);
                    if (sd->thread_state.load(std::memory_order_relaxed) != EngineThreadState::Occupied) {
                        // Likely the UI side has just disconnected
                        break;
                    }
                    tenkai::cpp_utils::ScopeExit se_stop_flag([&] {
                        sd->engine_stop_flag.store(false, std::memory_order_release);
                        sd->engine_stop_flag.notify_one();
                        sd->thread_state.store(EngineThreadState::Vacant, std::memory_order_release);
                        sd->thread_state.notify_one();
                    });
                    if (sd->thread_task) {
                        run_ui_task(std::move(sd->thread_task), true);
                    }
                }

                // TODO: Do we really need this delay?
                // HACK: Delay thread tear down to prevent ordering issues
                Sleep(100);
            }
            catch (...) {
                sd->thread_exception = std::current_exception();
            }
        }, WorkItemPriority::Normal, WorkItemOptions::TimeSliced);
        m_sd->engine_stop_flag.store(false, std::memory_order_relaxed);
        m_sd->ui_is_alive.store(true, std::memory_order_release);
        m_sd->ui_is_alive.notify_one();

        // Initialize engine UI component
        InitEngineUI();

        // Check whether engine has panicked
        m_sd->thread_started.wait(false, std::memory_order_acquire);
        if (m_sd->thread_state.load(std::memory_order_relaxed) != EngineThreadState::Vacant) {
            assert(m_sd->thread_exception);
            std::rethrow_exception(m_sd->thread_exception);
        }
    }
    catch (std::exception const& e) {
        throw hresult_error(E_FAIL, to_hstring(e.what()));
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
        decltype(m_sd->ui_works) works;
        {
            std::scoped_lock guard(m_sd->ui_mutex);
            std::swap(works, m_sd->ui_works);
        }
        for (auto& work : std::move(works)) {
            work();
        }

        // After processing works, update layout so that OS can fire redraw events
        // TODO...
        //m_vsis_noref->Resize();

        // Handle engine thread termination
        if (m_sd->thread_state.load(std::memory_order_relaxed) == EngineThreadState::Died) {
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
        for (auto& line : m_ui_lines) {
            line.update_width(this, new_width);
            line.acc_height = last_height + line.line_height;
            last_height = line.acc_height;
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
                input_req = nullptr;
            });
            m_input_countdown_timer.Stop();
            if (input_req->show_expiry_msg) {
                auto expire_msg = input_req->expiry_msg;
                if (!expire_msg.empty()) {
                    RoutinePrint(expire_msg, ERA_PEF_IS_LINE);
                    // HACK: Enforce a redraw so that we won't lose texts because of
                    //       engine-thread-queued UI work running early
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

            // TODO: Improve string concatenation performance
            m_ui_lines.emplace_back(hstring{}, txt_fmt);
            auto& cur_line = m_ui_lines.back();
            for (auto const& part : m_cur_composing_line.parts) {
                auto starti = cur_line.txt.size();
                cur_line.txt = cur_line.txt + part.str;
                cur_line.effects.push_back({ .starti = starti, .len = part.str.size(),
                    .color = part.color, .style = m_cur_font_style });
            }
            m_cur_composing_line = {};

            cur_line.ensure_layout(this, m_ui_width);
            height += cur_line.line_height;
            cur_line.acc_height = height;
            cur_line.flush_effects(this);

            m_cur_printc_count = 0;

            updated = true;
        };

        auto color = to_u32(EngineForeColor());
        if (flags & ERA_PEF_IS_SINGLE) {
            if (!m_cur_composing_line.parts.empty()) {
                push_cur_composing_line_fn();
            }
        }
        if (flags & (ERA_PEF_LEFT_PAD | ERA_PEF_RIGHT_PAD)) {
            // PRINTC / PRINTLC
            bool align_right = flags & ERA_PEF_RIGHT_PAD;
            auto str_width = helper_get_string_width(to_string(content).c_str());
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
        std::wstring_view content_sv{ content };
        size_t newline_pos;
        // TODO: forbid_button
        // TODO: is_isolated
        while ((newline_pos = content_sv.find(L'\n')) != content_sv.npos) {
            auto subsv = content_sv.substr(0, newline_pos);
            content_sv = content_sv.substr(newline_pos + 1);
            if (!subsv.empty()) {
                m_cur_composing_line.parts.push_back(
                    { .str = hstring(subsv), .color = color });
            }
            push_cur_composing_line_fn();
        }
        if (!content_sv.empty()) {
            m_cur_composing_line.parts.push_back(
                { .str = hstring(content_sv), .color = color });
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

        // Flush intermediate print contents first
        FlushCurPrintLine();
        FlushEngineImageOutputLayout(false);

        m_input_start_t = winrt::clock::now();
        m_input_last_prompt = {};
        m_input_countdown_timer.Stop();
        auto& input_req = m_outstanding_input_req;
        input_req = std::move(request);
        // Check if we can fulfill the input request immediately
        if (input_req->is_one) {
            auto input_tb = UserInputTextBox();
            auto input = input_tb.Text();
            if (input_req->try_fulfill(input)) {
                input_tb.Text({});
                input_req = nullptr;
                // Echo back
                if (!input.empty()) { RoutinePrint(input, ERA_PEF_IS_LINE); }
                return;
            }
        }
        // Handle timed input
        if (input_req->time_limit >= 0) {
            // TODO: More precise countdown handling
            if (input_req->time_limit > 0) {
                m_input_countdown_timer.Start();
            }
            OnInputCountDownTick(nullptr, nullptr);
        }
        // TODO: Check if user is skipping
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
        RoutinePrint(content, flags);
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
        // TODO: RoutinePrintButton
        RoutinePrint(format(L"[{}] {}", value, content), flags);
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

#define DP_CLASS EngineControl
    DP_DEFINE(EngineForeColor, box_value(Windows::UI::Colors::Silver()));
    DP_DEFINE(EngineBackColor, box_value(Windows::UI::Colors::Black()));
    DP_DEFINE(EngineTitle, box_value(L"MEraEmu"));
    DP_DEFINE_METHOD(EngineForeColor, Color);
    DP_DEFINE_METHOD(EngineBackColor, Color);
    DP_DEFINE_METHOD(EngineTitle, hstring const&);
#undef DP_CLASS
}
