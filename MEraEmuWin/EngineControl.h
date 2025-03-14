#pragma once

#include <winrt/Tenkai.h>
#include <winrt/Tenkai.UI.Xaml.h>
#include "EngineUnhandledExceptionEventArgs.g.h"
#include "EngineControl.g.h"
#include "CanvasVirtualControl.h"
#include "AppSettingsVM.h"

#include "MEraEngine.hpp"

#include "SoundPlayer.hpp"

#include "DPHelper.h"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct MainPage;
}

namespace winrt::MEraEmuWin::implementation {
    struct EngineSharedData;
    struct InputRequest;
    struct MEraEmuWinEngineSysCallback;
    struct EngineUIPrintLineData;
    struct InputRequest;
    struct EngineThreadTask;

    enum class EngineThreadTaskKind {
        None,
        ReturnToTitle,
        SetHaltState,
        ClearHaltState,
        SingleStepAndHalt,
        CustomFunc,
        CustomFuncAndClearHaltState,
        SyncSettingsWithFunc,
    };

    template <typename T>
    struct SequentialObjectAllocator {
        SequentialObjectAllocator() {}
        SequentialObjectAllocator(SequentialObjectAllocator const&) = delete;
        SequentialObjectAllocator(SequentialObjectAllocator&&) = delete;
        SequentialObjectAllocator& operator=(SequentialObjectAllocator const&) = delete;
        SequentialObjectAllocator& operator=(SequentialObjectAllocator&&) = delete;

        // NOTE: Returns 1-based index

        size_t allocate(T obj) {
            // First, try to find a free slot
            for (size_t i = 0; i < m_valid.size(); ++i) {
                if (!m_valid[i]) {
                    m_data[i] = std::move(obj);
                    m_valid[i] = true;
                    return i + 1;
                }
            }

            // Otherwise, append to the end
            size_t idx = m_data.size();
            m_data.push_back(std::move(obj));
            m_valid.push_back(true);
            return idx + 1;
        }

        void deallocate(size_t idx) {
            idx--;
            if (m_valid.at(idx)) {
                T t = std::move(m_data[idx]);
                (void)t;
            }
            else {
                throw std::invalid_argument("Invalid deallocation index");
            }
            m_valid[idx] = false;
        }

        bool is_valid(size_t idx) const {
            idx--;
            return idx < m_valid.size() && m_valid[idx];
        }

        T& at(size_t idx) {
            if (!is_valid(idx)) {
                throw std::invalid_argument("Invalid index");
            }
            return m_data[idx - 1];
        }

        T& operator[](size_t idx) {
            return m_data[idx - 1];
        }

        void clear() {
            m_data.clear();
            m_valid.clear();
        }

    private:
        std::vector<T> m_data;
        std::vector<bool> m_valid;
    };

    struct EngineUILength {
        int32_t len;
        enum class Type {
            FontPercent,
            Pixel,
        } type;

        EngineUILength() : len(0), type(Type::FontPercent) {}
        EngineUILength(int32_t len, Type type) : len(len), type(type) {}

        static EngineUILength FontPercent(int32_t len) {
            return EngineUILength(len, Type::FontPercent);
        }
        static EngineUILength Pixel(int32_t len) {
            return EngineUILength(len, Type::Pixel);
        }

        bool operator==(EngineUILength const& other) const {
            return len == other.len && type == other.type;
        }

        friend std::string to_string(EngineUILength const& value) {
            auto unit = [&] {
                using Type = winrt::MEraEmuWin::implementation::EngineUILength::Type;
                switch (value.type) {
                case Type::FontPercent: return "";
                case Type::Pixel: return "px";
                default: return "";
                }
            }();
            return std::to_string(value.len) + unit;
        }
        friend winrt::hstring to_hstring(EngineUILength const& value) {
            auto unit = [&] {
                using Type = winrt::MEraEmuWin::implementation::EngineUILength::Type;
                switch (value.type) {
                case Type::FontPercent: return L"";
                case Type::Pixel: return L"px";
                default: return L"";
                }
            }();
            return to_hstring(value.len) + unit;
        }
    };

    struct EngineUIPrintLineDataButton {
        // For input command
        struct InputButton {
            hstring input;
        };
        // For source errors & warnings
        struct SourceButton {
            hstring path;
            uint32_t line, column;
        };
        // For engine control on error
        struct EngineErrorControlButton {
            enum class ButtonType {
                Retry,
                Continue,
            } type;
        };

        using ButtonData = std::variant<InputButton, SourceButton, EngineErrorControlButton>;

        uint32_t starti, len;
        ButtonData data;
    };

    struct EngineUIPrintLineDataInlineObject {
        struct ShapeRect {
            EngineUILength x, y, width, height;
        };
        struct ShapeSpace {
            EngineUILength size;
        };
        struct Image {
            hstring sprite;
            EngineUILength width, height, ypos;
        };

        using InlineObjectData = std::variant<ShapeRect, ShapeSpace, Image>;

        struct InlineObject;

        uint32_t starti, len;
        com_ptr<InlineObject> obj;
    };

    struct EngineUnhandledExceptionEventArgs : EngineUnhandledExceptionEventArgsT<EngineUnhandledExceptionEventArgs> {
        EngineUnhandledExceptionEventArgs(hresult code, hstring const& msg) :
            m_code(code), m_msg(msg) {
        }

        hresult Code() const noexcept { return m_code; }
        hstring Message() const noexcept { return m_msg; }

    private:
        hresult m_code;
        hstring m_msg;
    };

    struct EngineControl : EngineControlT<EngineControl> {
        EngineControl();
        ~EngineControl();
        void InitializeComponent();

        void ReturnToTitle();
        void ApplySettings(MEraEmuWin::AppSettingsVM settings);
        Windows::Foundation::IAsyncAction ExportLogsToStream(Windows::Storage::Streams::IRandomAccessStream stream);
        bool IsStarted();
        void IsDevToolsOpen(bool value);
        bool IsDevToolsOpen();
        void AudioVolume(double value);
        double AudioVolume();

        event_token UnhandledException(Windows::Foundation::EventHandler<MEraEmuWin::EngineUnhandledExceptionEventArgs> h) {
            return m_ev_UnhandledException.add(h);
        }
        void UnhandledException(event_token et) noexcept { m_ev_UnhandledException.remove(et); }

        DP_DECLARE_PROP_METHOD(EngineTitle);

        DP_DEFINE_GETSETTER(EngineTitle, hstring const&);

        Windows::UI::Color EngineForeColor() const noexcept { return m_cur_fore_color; }
        void EngineForeColor(Windows::UI::Color value) noexcept { m_cur_fore_color = value; }
        Windows::UI::Color EngineBackColor() const noexcept { return m_cur_back_color; }
        void EngineBackColor(Windows::UI::Color value) noexcept {
            m_cur_back_color = value;
            BackgroundSolidBrush().Color(value);
        }

        void EngineOutputImage_PointerPressed(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void EngineOutputImage_PointerReleased(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void EngineOutputImage_PointerMoved(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void EngineOutputImage_PointerExited(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void EngineOutputImage_PointerCanceled(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void EngineOutputImage_Tapped(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::TappedRoutedEventArgs const& e);
        void EngineOutputImage_RightTapped(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::RightTappedRoutedEventArgs const& e);
        void UserInputTextBox_KeyDown(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs const& e);

        // Non-midl methods
        void Bootstrap(hstring const& game_base_dir);

        bool IsUserSkipping() const { return m_user_skipping; }

    private:
        friend winrt::MEraEmuWin::DevTools::implementation::MainPage;

        friend EngineSharedData;
        friend MEraEmuWinEngineSysCallback;
        friend EngineUIPrintLineData;
        friend EngineUIPrintLineDataInlineObject;

        struct PrintButtonRegionContext {
            uint32_t ui_line_start{ UINT32_MAX }, ui_line_offset{ UINT32_MAX };
        };

        void UISideDisconnect();
        void QueueEngineTask(std::unique_ptr<EngineThreadTask> task);
        void QueueEngineTask(EngineThreadTaskKind task_kind);
        void QueueEngineFuncTask(std::move_only_function<void(MEraEngine const&)> f, bool clear_halt);
        template <typename F>
        auto ExecEngineTask(F&& f, bool clear_halt = false) {
            using R = std::invoke_result_t<F, MEraEngine const&>;
            std::promise<R> prom;
            auto fut = prom.get_future();
            QueueEngineFuncTask([f = std::forward<F>(f), prom = std::move(prom)](auto&& e) mutable {
                try {
                    if constexpr (std::is_same_v<R, void>) {
                        f(e);
                        prom.set_value();
                    }
                    else {
                        prom.set_value(f(e));
                    }
                }
                catch (...) {
                    prom.set_exception(std::current_exception());
                }
            }, clear_halt);
            return util::winrt::apartment_aware_future(std::move(fut));
        }
        void OnEngineExecutionInterrupted(EraExecutionBreakReason reason);
        event_token EngineExecutionInterrupted(delegate<EraExecutionBreakReason> dg) {
            return m_ev_EngineExecutionInterrupted.add(dg);
        }
        void EngineExecutionInterrupted(event_token et) noexcept {
            m_ev_EngineExecutionInterrupted.remove(et);
        }
        EraExecutionBreakReason GetLastBreakReason() const { return m_last_execution_break_reason; }

        void UpdateDevToolsWindow();

        void InitEngineUI();
        void ClearEngineUIState();
        void UpdateEngineUI();
        void EmitUnhandledExceptionEvent(std::exception_ptr ex);
        void RedrawDirtyEngineImageOutput();
        // NOTE: If invalidate_all is false, it is assumed that previous lines along with
        //       UI metrics are unchanged. Only the newly added lines are redrawn.
        void UpdateEngineImageOutputLayout(bool invalidate_all, bool recreate_ui_lines = false);
        void InitD2DDevice(bool force_software);
        void InitD2DDevice() {
            InitD2DDevice(!m_app_settings->EnableHardwareAcceleration());
        }
        void RelayoutUILines(bool recreate_all);
        uint64_t GetAccUIHeightInLines(size_t line_idx = -1);
        // NOTE: Returns count of lines if height exceeds all lines
        size_t GetLineIndexFromHeight(uint64_t height);
        // Updates line metrics for the given line index. If you were to update multiple lines,
        // you should call this function in ascending order of line index.
        void UpdateLineAccMetrics(size_t i);
        void InvalidateLineAtIndex(size_t line);
        void UpdateAndInvalidateActiveButton(Windows::Foundation::Point const& pt);
        bool TryFulfillInputRequest(bool clear_input);
        ID2D1SolidColorBrush* GetOrCreateSolidColorBrush(uint32_t color);
        IDWriteTextFormat* GetOrCreateTextFormat(hstring const& font_family);
        IDWriteTextFormat* GetDefaultTextFormat() {
            return GetOrCreateTextFormat(m_app_settings->GameDefaultFontName());
        }
        float ConvFontUnitToPixels(float font_unit) const noexcept {
            return font_unit * m_ui_param_cache.font_size_px_f / 100;
        }
        float ConvPixelUnitToPixels(float font_unit) const noexcept {
            return font_unit * m_ui_param_cache.ui_scale;
        }
        float ConvLengthToPixels(EngineUILength const& len) const noexcept {
            using Type = EngineUILength::Type;
            switch (len.type) {
            case Type::FontPercent:
                return ConvFontUnitToPixels(len.len);
            case Type::Pixel:
                return ConvPixelUnitToPixels(len.len);
            default:
                // Should not happen
                return len.len;
            }
        }
        int32_t ConvLengthToPixelsI32(EngineUILength const& len) const noexcept {
            return static_cast<int32_t>(ConvLengthToPixels(len));
        }
        // F: fn(ID2D1DeviceContext3* ctx)
        // If successful, returns true. Otherwise, returns false.
        template <typename F>
        bool RunDrawingSession(F&& f) {
            if (!m_d2d_ctx) {
                return false;
            }
            m_d2d_ctx->BeginDraw();
            try {
                f(m_d2d_ctx.get());
                m_d2d_ctx->EndDraw();
                return true;
            }
            catch (...) {
                m_d2d_ctx->EndDraw();
                return false;
            }
        }

        void OnInputCountDownTick(IInspectable const&, IInspectable const&);
        void HandleEngineLeftClick(Windows::Foundation::Point groundtruth_pt, Windows::Foundation::Point engine_pt);
        bool FlushCurrentPrintLine(bool force_push = false);
        // NOTE: Wait flag is ignored deliberately
        void RoutinePrint(hstring content, PrintExtendedFlags flags);
        void RoutineHtmlPrint(hstring const& content, int64_t no_single);
        hstring RoutineHtmlPopPrintingStr();
        hstring RoutineHtmlGetPrintedStr(int64_t line_no);
        int64_t RoutineHtmlStringLen(hstring const& content, bool return_pixel);
        void RoutineInput(std::unique_ptr<InputRequest> request);
        void RoutineReuseLastLine(hstring const& content);
        void RoutineClearLine(uint64_t count);
        void RoutinePrintSourceButton(hstring const& content, hstring const& path,
            uint32_t line, uint32_t column, PrintExtendedFlags flags);
        void RoutinePrintButton(hstring const& content, hstring const& value, PrintExtendedFlags flags);
        void RoutinePrintEngineErrorControlButton(hstring const& content, EngineUIPrintLineDataButton::EngineErrorControlButton::ButtonType type);
        void RoutinePrintButtonGeneric(hstring const& content, PrintExtendedFlags flags, EngineUIPrintLineDataButton::ButtonData data);
        PrintButtonRegionContext BeginPrintButtonRegion();
        template <typename F>
        void EndPrintButtonRegion(PrintButtonRegionContext ctx, F&& f);
        /// Creates a new UI lines snapshot and restores the previous state (discards new lines)
        /// when the guard goes out of scope.
        auto MakeUILinesSnapshotGuard();
        int64_t RoutineGCreate(int64_t gid, int64_t width, int64_t height);
        int64_t RoutineGCreateFromFile(int64_t gid, hstring const& path);
        int64_t RoutineGDispose(int64_t gid);
        int64_t RoutineGCreated(int64_t gid);
        int64_t RoutineGDrawSprite(int64_t gid, hstring const& sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const* color_matrix);
        int64_t RoutineGClear(int64_t gid, uint32_t color);
        int64_t RoutineSpriteCreate(hstring const& name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y);
        int64_t RoutineSpriteDispose(hstring const& name);
        int64_t RoutineSpriteCreated(hstring const& name);
        // TODO...
        int64_t RoutineSpriteWidth(hstring const& name);
        int64_t RoutineSpriteHeight(hstring const& name);
        int64_t RoutineSpritePosX(hstring const& name);
        int64_t RoutineSpritePosY(hstring const& name);
        // TODO...
        int64_t RoutinePlaySound(hstring const& path, int64_t loop_count, bool is_bgm);
        int64_t RoutineStopSound(int64_t sound_id);

        int64_t GetCurrentUILinesCount();
        void SetCurrentLineAlignment(int64_t value);
        int64_t GetCurrentLineAlignment();
        void SetCurrentFontStyle(int64_t value);
        int64_t GetCurrentFontStyle();
        void SetCurrentFontName(hstring const& value);
        hstring GetCurrentFontName();
        void SetRedrawState(int64_t value);
        int64_t GetRedrawState();
        void SetSkipDisplay(int64_t value);
        int64_t GetSkipDisplay();
        int64_t GetEffectiveSkipDisplay();
        void PushNoSkipDisplay();
        void PopNoSkipDisplay();

        DP_DECLARE_PROP(EngineTitle);

        std::shared_ptr<EngineSharedData> m_sd;
        util::sync::spsc::Receiver<std::move_only_function<void()>> m_ui_task_rx{ nullptr };
        util::sync::spsc::Sender<std::unique_ptr<EngineThreadTask>> m_engine_task_tx{ nullptr };

        com_ptr<implementation::AppSettingsVM> m_app_settings = nullptr;
        EraExecutionBreakReason m_last_execution_break_reason = ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS;
        event<delegate<EraExecutionBreakReason>> m_ev_EngineExecutionInterrupted;
        Tenkai::UI::Xaml::Window m_devtools_wnd{ nullptr };
        event<Windows::Foundation::EventHandler<MEraEmuWin::EngineUnhandledExceptionEventArgs>> m_ev_UnhandledException;
        IVirtualSurfaceImageSourceNative* m_vsis_noref{};
        ISurfaceImageSourceNativeWithD2D* m_vsis_d2d_noref{};
        com_ptr<ID2D1DeviceContext3> m_d2d_ctx;
        com_ptr<ID2D1SpriteBatch> m_d2d_sprite_batch;
        std::unordered_map<uint32_t, com_ptr<ID2D1SolidColorBrush>> m_brush_map;
        std::unordered_map<hstring, com_ptr<IDWriteTextFormat>> m_font_map;
        com_ptr<IDWriteTextLayout4> m_empty_text_layout;
        std::vector<EngineUIPrintLineData> m_ui_lines, m_ui_lines_alt;
        bool m_reused_last_line{ false };
        // Introduced to handle issued CLEARLINE commands while REDRAW = 0. Range: [pos - count, pos)
        uint32_t m_soft_deleted_ui_lines_count{}, m_soft_deleted_ui_lines_pos{};
        uint32_t m_cur_line_alignment{};
        uint32_t m_cur_font_style{};
        hstring m_cur_font_name{};
        Windows::UI::Color m_cur_fore_color{}, m_cur_back_color{};
        bool m_auto_redraw{};
        bool m_skip_display{};
        uint32_t m_no_skip_display_cnt{};
        uint32_t m_last_redraw_dirty_height{};
        struct ComposingLineData {
            struct DataPart {
                hstring str{};
                // NOTE: We use values from settings as sentinels (follow default settings)
                uint32_t color{};
                uint32_t style{};
                hstring font_name{};
                com_ptr<EngineUIPrintLineDataInlineObject::InlineObject> inline_obj; // Sentinel: nullptr

                bool forbid_button{};   // true for PRINTPLAIN
                bool is_isolated{};     // true for PRINTC
            };
            std::vector<DataPart> parts;
            std::vector<EngineUIPrintLineDataButton> explicit_buttons;
        } m_cur_composing_line;
        std::unique_ptr<InputRequest> m_outstanding_input_req;
        winrt::clock::time_point m_input_start_t;
        hstring m_input_last_prompt;
        Windows::UI::Xaml::DispatcherTimer m_input_countdown_timer;
        struct {
            // Used to determine canvas size in pixels (DIP -> pixel conversion)
            float xscale{}, yscale{};
            // The actual UI scale factor. We deliberately ignore the world transform
            // (m_xscale, m_yscale) applied to the engine control.
            float ui_scale{};
            uint32_t canvas_width_px{}, canvas_height_px{};
            // Below are calculated values based on the above
            uint32_t line_char_capacity{};
            float line_height_px_f{};
            float font_size_px_f{};
        } m_ui_param_cache;
        int64_t m_cur_printc_count{};
        bool m_user_skipping{};
        struct ActiveButtonData {
            size_t line;
            uint32_t button_idx;

            ActiveButtonData() : line(size_t(-1)), button_idx() {}
            auto operator<=>(ActiveButtonData const& rhs) const noexcept = default;
            bool is_default() const noexcept { return line == -1; }
        } m_cur_active_button{};
        struct {
            Windows::Foundation::Point pt{ -1, -1 };
            bool left_button_down{};
            bool right_button_down{};

            bool is_valid() const noexcept { return pt.X != -1; }
        } m_cur_pointer;

        struct GraphicsObject {
            hstring file_path;
            uint32_t width{}, height{};
            com_ptr<ID2D1Bitmap1> bitmap;
            bool is_bad{};  // true if the file is not found or invalid

            GraphicsObject(hstring const& file_path) : file_path(file_path) {}
            GraphicsObject(uint32_t width, uint32_t height) : width(width), height(height) {}

            void ensure_loaded(EngineControl* ctrl);
            bool try_ensure_loaded(EngineControl* ctrl);
            std::pair<uint32_t, uint32_t> get_dimensions() const noexcept {
                auto size = bitmap->GetPixelSize();
                return { size.width, size.height };
            }
        };
        std::map<int64_t, GraphicsObject> m_graphics_objects;
        struct SpriteObject {
            int64_t gid;
            int32_t x, y, width, height, offset_x, offset_y;

            SpriteObject(int64_t gid, int32_t x, int32_t y, int32_t width, int32_t height, int32_t offset_x, int32_t offset_y) :
                gid(gid), x(x), y(y), width(width), height(height), offset_x(offset_x), offset_y(offset_y) {
            }

            void ensure_loaded(EngineControl* ctrl) {
                auto sprite_it = ctrl->m_graphics_objects.find(gid);
                if (sprite_it == ctrl->m_graphics_objects.end()) {
                    throw std::invalid_argument("Invalid sprite object");
                }
                sprite_it->second.ensure_loaded(ctrl);
            }
            bool try_ensure_loaded(EngineControl* ctrl) {
                auto sprite_it = ctrl->m_graphics_objects.find(gid);
                if (sprite_it == ctrl->m_graphics_objects.end()) {
                    return false;
                }
                return sprite_it->second.try_ensure_loaded(ctrl);
            }
            auto& get_graphics_object(EngineControl* ctrl) const {
                auto sprite_it = ctrl->m_graphics_objects.find(gid);
                if (sprite_it == ctrl->m_graphics_objects.end()) {
                    throw std::invalid_argument("Invalid sprite object");
                }
                return sprite_it->second;
            }
        };
        std::map<hstring, SpriteObject, std::less<>> m_sprite_objects;
        struct SoundData {
            concurrency::task<SoundPlaybackHub> init_task{};
            SoundPlaybackHub hub{ nullptr };
            double initial_volume{ 1.0 };

            auto ensure_inited_async(EngineControl* ctrl) {
                return util::winrt::apartment_aware_task(ensure_inited_async_inner(ctrl));
            }

        private:
            concurrency::task<void> ensure_inited_async_inner(EngineControl* ctrl) {
                if (hub) { co_return; }
                bool is_init = false;
                if (init_task == decltype(init_task){}) {
                    init_task = SoundPlaybackHub::create_async();
                    is_init = true;
                }
                auto strong_this = ctrl->get_strong();
                {
                    auto task = init_task;
                    hub = co_await task;
                }
                init_task = {};
                if (is_init) {
                    hub.register_on_exception([weak_this = ctrl->get_weak()](std::exception_ptr ex) {
                        if (auto ctrl = weak_this.get()) {
                            ctrl->EmitUnhandledExceptionEvent(std::move(ex));
                        }
                    });
                    hub.set_output_volume(initial_volume);
                }
            }
        } m_sound;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct EngineControl : EngineControlT<EngineControl, implementation::EngineControl> {};
}
