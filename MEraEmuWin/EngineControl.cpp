#include "pch.h"
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
//#include <intrin.h>
#include <d3d11_2.h>
#include <d2d1_2.h>
#include <d2d1_2helper.h>
#include <dwrite_3.h>
#include <wincodec.h>

using namespace winrt;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Media::Imaging;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Core;
using namespace Windows::Foundation;
using namespace Windows::System::Threading;

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
        out = a + b;
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
                continue;
            }
            if (!(L'0' <= ch && ch <= L'9')) {
                return std::nullopt;
            }
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
        //auto file_size = std::filesystem::file_size(path);
        //std::ifstream fin(path, std::ios::in | std::ios::binary);
        //std::vector<uint8_t> buf;
        //buf.reserve(file_size);
        //auto it = std::istreambuf_iterator<char>(fin);
        //auto ie = std::istreambuf_iterator<char>();
        //// Check BOM header
        //for (int i = 0; i < 3 && it != ie; i++) {
        //    buf.push_back(*it);
        //    ++it;
        //}
        //if (buf.size() == 3 && (buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf)) {
        //    buf.resize(0);
        //}
        //std::copy(...);
        //return buf;
        winrt::file_handle file{ CreateFileW(path.c_str(),
            GENERIC_READ,
            FILE_SHARE_READ,
            nullptr,
            OPEN_EXISTING,
            0,
            nullptr
        ) };
        check_bool(static_cast<bool>(file));
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
}

namespace winrt::MEraEmuWin::implementation {
    struct InputRequest {
        //InputRequest(int64_t time_limit, bool show_time_prompt, hstring expiry_msg, bool is_one, bool can_skip) : {}
        virtual ~InputRequest() {}

        // NOTE: For derived requests, hold a std::promise so that engine
        //       can receive the result or teardown notification.

        // Shared parameters
        int64_t time_limit; // Negatives stand for no limit
        bool show_time_prompt;
        hstring expiry_msg;
        bool is_one;
        bool can_skip;

        //virtual void time_tick() = 0;
        virtual bool try_fulfill(hstring const& input) = 0;
    };
    struct InputRequestI : InputRequest {
        std::optional<int64_t> default_value;
        std::promise<int64_t> promise;

        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                return true;
            }
            if (auto r = parse<int64_t>(input)) {
                promise.set_value(*r);
                return true;
            }
            return false;
        }
    };
    struct InputRequestS : InputRequest {
        std::optional<hstring> default_value;
        std::promise<hstring> promise;

        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                return true;
            }
            promise.set_value(input);
            return true;
        }
    };

    struct EngineSharedData : std::enable_shared_from_this<EngineSharedData> {
        ~EngineSharedData() {}

        hstring game_base_dir;
        CoreDispatcher ui_dispatcher{ nullptr };
        //weak_ref<EngineControl> ui_ctrl{ nullptr };
        EngineControl* ui_ctrl{};
        IAsyncAction thread_task{ nullptr };
        std::atomic_bool ui_is_alive{ false };
        // NOTE: Packed with ui_is_alive; used by engine stop_flag
        std::atomic_bool ui_is_dead{ true };
        std::atomic_bool thread_is_alive{ false };
        std::atomic_bool thread_started{ false };
        std::exception_ptr thread_exception{ nullptr };

        std::mutex ui_mutex;
        std::vector<std::function<void()>> ui_works;

        // NOTE: Called by UI thread
        void ui_disconnect() {
            if (thread_task) {
                thread_task.Cancel();
            }
            ui_is_dead.store(true, std::memory_order_relaxed);
            ui_is_alive.store(false, std::memory_order_relaxed);
            ui_is_alive.notify_one();
            // Abandon queued works
            [&] {
                std::scoped_lock guard(ui_mutex);
                return std::exchange(ui_works, {});
            }();
        }
        // NOTE: Called by background thread
        void task_disconnect() {
            thread_is_alive.store(false, std::memory_order_relaxed);
            thread_is_alive.notify_one();
            // Triggers UI update, which in turn responds to task disconnection
            queue_ui_work([] {});
        }
        void queue_ui_work(std::function<void()> work) {
            if (!ui_is_alive.load(std::memory_order_relaxed)) {
                throw hresult_canceled();
            }
            bool has_no_work;
            {
                std::scoped_lock guard(ui_mutex);
                has_no_work = ui_works.empty();
                ui_works.push_back(std::move(work));
            }
            if (has_no_work) {
                // Wake up UI thread
                ui_dispatcher.RunAsync(CoreDispatcherPriority::Low, [self = shared_from_this()] {
                    if (!self->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                    // SAFETY: We are in the UI thread; no one could be destructing
                    //         EngineControl.
                    self->ui_ctrl->UpdateEngineUI();
                });
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
            m_sd->queue_ui_work([sd = m_sd, final_msg] {
                sd->ui_ctrl->RoutinePrint(final_msg, ERA_PEF_IS_LINE);
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
            m_sd->queue_ui_work([sd = m_sd, final_msg] {
                sd->ui_ctrl->RoutinePrint(final_msg, ERA_PEF_IS_LINE);
            });
        }
        uint64_t on_get_rand() override {
            return m_rand_gen();
        }
        void on_print(std::string_view content, PrintExtendedFlags flags) override {
            m_sd->queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutinePrint(content, ERA_PEF_IS_LINE);
            });
            // TODO: on_print() wait flag
        }
        void on_html_print(std::string_view content) override {
            // TODO: on_html_print
        }
        void on_wait(bool any_key, bool is_force) override
        {
        }
        void on_twait(int64_t duration, bool is_force) override
        {
        }
        std::optional<int64_t> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) override
        {
            return std::optional<int64_t>();
        }
        const char* on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) override
        {
            return nullptr;
        }
        std::optional<int64_t> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override
        {
            return std::optional<int64_t>();
        }
        const char* on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override
        {
            return nullptr;
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
        void on_reuselastline(std::string_view content) override
        {
        }
        void on_clearline(int64_t count) override
        {
        }
        int64_t on_var_get_int(std::string_view name, uint32_t idx) override
        {
            return 0;
        }
        const char* on_var_get_str(std::string_view name, uint32_t idx) override
        {
            return nullptr;
        }
        void on_var_set_int(std::string_view name, uint32_t idx, int64_t val) override
        {
        }
        void on_var_set_str(std::string_view name, uint32_t idx, std::string_view val) override
        {
        }
        void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) override
        {
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
        int64_t on_check_font(std::string_view font_name) override
        {
            return 0;
        }
        uint64_t on_get_host_time() override
        {
            return 0;
        }
        int64_t on_get_config_int(std::string_view name) override
        {
            return 0;
        }
        const char* on_get_config_str(std::string_view name) override
        {
            return nullptr;
        }
        int64_t on_get_key_state(int64_t key_code) override
        {
            return 0;
        }

    private:
        EngineSharedData* const m_sd;
        std::mt19937_64 m_rand_gen{ std::random_device{}() };
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

    struct EngineUIPrintLineData {
        hstring txt;
        com_ptr<IDWriteTextFormat> txt_fmt;
        com_ptr<IDWriteTextLayout4> txt_layout;
        std::vector<com_ptr<::IUnknown>> inline_objs;   // Usually images
        uint64_t line_height{};
        uint64_t acc_height{};    // Including current line

        EngineUIPrintLineData(hstring const& txt, com_ptr<IDWriteTextFormat> const& txt_fmt) :
            txt(txt), txt_fmt(txt_fmt) {}

        void update_width(uint64_t width) {
            if (!txt_layout) { return ensure_layout(width); }
            check_hresult(txt_layout->SetMaxWidth(static_cast<float>(width)));

            flush_metrics();
        }
        void ensure_layout(uint64_t width) {
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

            flush_metrics();
        }

    private:
        void flush_metrics() {
            //acc_height -= line_height;
            DWRITE_TEXT_METRICS1 metrics;
            check_hresult(txt_layout->GetMetrics(&metrics));
            line_height = static_cast<uint64_t>(metrics.height);
            //acc_height += line_height
        }
    };

    EngineControl::EngineControl() {
        ensure_global_factory();
    }
    EngineControl::~EngineControl() {
        if (m_sd) {
            m_sd->ui_disconnect();
        }
    }
    void EngineControl::InitializeComponent() {
        EngineControlT::InitializeComponent();

        // Register for scale notification
        BackgroundSwapchainPanel().CompositionScaleChanged([this](SwapChainPanel const& sender, auto&&) {
            m_xscale = sender.CompositionScaleX();
            m_yscale = sender.CompositionScaleY();
        });
        // And width notification
        SizeChanged([this](auto&&, auto&&) {
            UpdateUIWidth((uint64_t)ActualWidth());
        });
    }
    void EngineControl::Bootstrap(hstring const& game_base_dir) try {
        m_sd = std::make_shared<EngineSharedData>();
        m_sd->game_base_dir = game_base_dir;
        m_sd->ui_dispatcher = Dispatcher();
        //m_sd->ui_ctrl = get_weak();
        m_sd->ui_ctrl = this;
        // Start a dedicated background thread
        m_sd->thread_task = ThreadPool::RunAsync([sd = m_sd](IAsyncAction const& op) {
            sd->thread_is_alive.store(true, std::memory_order_relaxed);
            sd->thread_is_alive.notify_one();

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
                engine.install_sys_callback(std::make_unique<MEraEmuWinEngineSysCallback>(sd.get()));

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
                eri("LINECOUNT");
                ers("SAVEDATA_TEXT");

                // Collect files used by engine
                for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"CSV")) {
                    // TODO...
                }
                std::vector<std::filesystem::path> erbs;
                for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"ERB")) {
                    // TODO...
                    erbs.push_back(entry.path());
                }

                mark_thread_started();

                // Load all ERB files
                // TODO...
                for (auto& erb : erbs) {
                    auto [data, size] = read_utf8_file(erb);
                    engine.load_erb(to_string(erb.c_str()).c_str(), { data.get(), size });
                }
                std::exchange(erbs, {});

                // Finialize compilation
                engine.finialize_load_srcs();

                // Main loop
                //while (sd->ui_is_alive.load(std::memory_order_relaxed)) {
                //    // TODO...
                //    Sleep(1);
                //}
                engine.do_execution(&sd->ui_is_dead, UINT64_MAX);
                // TODO: Ensure engine has halted

                // HACK: Delay thread tear down to prevent ordering issues
                Sleep(100);
            }
            catch (...) {
                sd->thread_exception = std::current_exception();
            }
        }, WorkItemPriority::Normal, WorkItemOptions::TimeSliced);
        m_sd->ui_is_dead.store(false, std::memory_order_relaxed);
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
    }
    catch (std::exception const& e) {
        throw hresult_error(E_FAIL, to_hstring(e.what()));
    }
    void EngineControl::InitEngineUI() {
        m_vsis_noref = nullptr;
        m_vsis_d2d_noref = nullptr;
        // TODO: Proper UI scale
        //m_xscale = m_yscale = XamlRoot().RasterizationScale();
        m_ui_lines.clear();
        m_cur_composing_line = {};
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
                    m_ui_ctrl->UpdateEngineImageOutput();
                });
            }

        private:
            EngineControl* const m_ui_ctrl;
        };
        m_vsis_noref->RegisterForUpdatesNeeded(make<RedrawCallback>(this).get());

        // Set UI focus
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

        // TODO: Handle engine thread termination
        if (!m_sd->thread_is_alive.load(std::memory_order_relaxed)) {
            UserInputTextBox().IsEnabled(false);
            UserInputTextBox().Text(L"无法在已结束的会话中输入内容");

            // TODO: Handle engine thread exception
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
    void EngineControl::UpdateEngineImageOutput() {
        int height = 0;
        if (!m_ui_lines.empty()) {
            height = m_ui_lines.back().acc_height;
        }
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
            /*com_ptr<ID2D1SolidColorBrush> brush;
            ctx->CreateSolidColorBrush(D2D1::ColorF(D2D1::ColorF::Red), brush.put());
            ctx->DrawRectangle(D2D1::RectF(offset.x, offset.y, offset.x + 10, offset.y + 10),
                brush.get());*/
            ctx->SetTransform(
                D2D1::Matrix3x2F::Scale(m_xscale, m_yscale) *
                D2D1::Matrix3x2F::Translation(offset.x - update_rt.left, offset.y - update_rt.top)
            );
            /*ctx->PushAxisAlignedClip(
                D2D1::RectF(
                    update_rt.left, update_rt.top, update_rt.right, update_rt.bottom),
                D2D1_ANTIALIAS_MODE_ALIASED
            );*/
            //ctx->SetDpi(m_xscale * 96, m_yscale * 96);
            int dip_rt_top = update_rt.top / m_yscale;
            int dip_rt_bottom = update_rt.bottom / m_yscale;
            ctx->Clear(D2D1::ColorF(D2D1::ColorF::White, 0));
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

            com_ptr<ID2D1SolidColorBrush> brush;
            ctx->CreateSolidColorBrush(D2D1::ColorF(D2D1::ColorF::Red), brush.put());

            for (int line = line_start; line < line_end; line++) {
                auto& line_data = m_ui_lines[line];
                int offx = 0;
                int offy = line_data.acc_height - line_data.line_height;
                ctx->DrawTextLayout(D2D1::Point2F(offx, offy), line_data.txt_layout.get(),
                    brush.get());
            }
        }
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
        /*check_hresult(d2d_dev->CreateDeviceContext(
                D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
                d2d_dev_ctx.put()
        ));*/

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
            line.update_width(new_width);
            line.acc_height = last_height + line.line_height;
            last_height = line.acc_height;
        }

        if (m_vsis_noref) {
            int height = 0;
            if (!m_ui_lines.empty()) {
                height = m_ui_lines.back().acc_height;
            }
            auto new_w = static_cast<int>(m_ui_width * m_xscale);
            auto new_h = static_cast<int>(height * m_yscale);
            check_hresult(m_vsis_noref->Resize(new_w, new_h));
            m_vsis_noref->Invalidate({ 0, 0, new_w, new_h });
        }
    }
    void EngineControl::RoutinePrint(hstring const& content, PrintExtendedFlags flags) {
        // TODO...
        int height = 0;
        if (!m_ui_lines.empty()) {
            height = m_ui_lines.back().acc_height;
        }

        com_ptr<IDWriteTextFormat> txt_fmt;
        check_hresult(g_dwrite_factory->CreateTextFormat(
            L"Consolas",
            nullptr,
            DWRITE_FONT_WEIGHT_REGULAR,
            DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL,
            13.5,
            L"",
            txt_fmt.put()
        ));
        m_ui_lines.emplace_back(content, txt_fmt);
        auto& last_line = m_ui_lines.back();
        last_line.ensure_layout(m_ui_width);
        height += last_line.line_height;
        last_line.acc_height = height;

        // Resize to trigger updates
        check_hresult(m_vsis_noref->Resize(m_ui_width * m_xscale, height * m_yscale));
    }
    void EngineControl::RoutineWait(std::unique_ptr<InputRequest> request) {
        // TODO...
        std::format("");
    }

    /*void EngineControl::EngineForeColor(Color value) {
        if (EngineForeColor() == value) { return; }
        SetValue(m_EngineForeColorProperty, box_value(value));
    }
    Color EngineControl::EngineForeColor() {
        return unbox_value<Color>(GetValue(m_EngineForeColorProperty));
    }*/
#define DP_CLASS EngineControl
    DP_DEFINE(EngineForeColor, box_value(Windows::UI::Colors::White()));
    DP_DEFINE(EngineBackColor, box_value(Windows::UI::Colors::Black()));
    DP_DEFINE_METHOD(EngineForeColor, Color);
    DP_DEFINE_METHOD(EngineBackColor, Color);
#undef DP_CLASS
}
