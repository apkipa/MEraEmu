#pragma once

#include <winrt/Tenkai.h>
#include <winrt/Tenkai.UI.Xaml.h>
#include "EngineUnhandledExceptionEventArgs.g.h"
#include "EngineControl.g.h"
#include "AppSettingsVM.h"

#include "MEraEngine.hpp"

#include "util.hpp"

#include <variant>
#include <future>

// TODO: Move to tools header
#define DP_NAMESPACE winrt::MEraEmuWin
//#define DP_CLASS EngineControl
#define DP_DECLARE(name)    \
    static Windows::UI::Xaml::DependencyProperty m_ ## name ## Property
#define DP_DECLARE_METHOD(name)                                         \
    static Windows::UI::Xaml::DependencyProperty name ## Property() {   \
        return m_ ## name ## Property;                                  \
    }
#define DP_DEFINE_METHOD(name, type)                                    \
    void DP_CLASS::name(type value) {                                   \
        using winrt::Windows::Foundation::IInspectable;                 \
        using RawT = std::remove_cvref_t<type>;                         \
        if constexpr (std::is_base_of_v<IInspectable, RawT>) {          \
            SetValue(m_ ## name ## Property, box_value(value));         \
        }                                                               \
        else {                                                          \
            if (name() == value) { return; }                            \
            SetValue(m_ ## name ## Property, box_value(value));         \
        }                                                               \
    }                                                                   \
    std::remove_cvref_t<type> DP_CLASS::name() {                        \
        using RawT = std::remove_cvref_t<type>;                         \
        return unbox_value<RawT>(GetValue(m_ ## name ## Property));     \
    }
#define DP_DEFINE(name, ...)                                                    \
    DependencyProperty DP_CLASS::m_ ## name ## Property =                       \
        DependencyProperty::Register(                                           \
            L"" #name,                                                          \
            winrt::xaml_typename<decltype(std::declval<DP_CLASS>().name())>(),  \
            winrt::xaml_typename<DP_NAMESPACE::DP_CLASS>(),                     \
            Windows::UI::Xaml::PropertyMetadata{ __VA_ARGS__ }                  \
        )

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
        //SyncSettings,
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

        uint32_t starti, len;
        std::variant<InputButton, SourceButton> data;
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
        bool IsStarted();
        void IsDevToolsOpen(bool value);
        bool IsDevToolsOpen();

        event_token UnhandledException(Windows::Foundation::EventHandler<MEraEmuWin::EngineUnhandledExceptionEventArgs> h) {
            return m_ev_UnhandledException.add(h);
        }
        void UnhandledException(event_token et) noexcept { m_ev_UnhandledException.remove(et); }

        void EngineForeColor(Windows::UI::Color value);
        Windows::UI::Color EngineForeColor();
        void EngineBackColor(Windows::UI::Color value);
        Windows::UI::Color EngineBackColor();
        void EngineTitle(hstring const& value);
        hstring EngineTitle();

        DP_DECLARE_METHOD(EngineForeColor);
        DP_DECLARE_METHOD(EngineBackColor);
        DP_DECLARE_METHOD(EngineTitle);

        // XAML helpers
        Windows::UI::Xaml::Media::SolidColorBrush ColorToBrush(Windows::UI::Color value) {
            return Windows::UI::Xaml::Media::SolidColorBrush(value);
        }

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
        void UpdateEngineUI();
        void EmitUnhandledExceptionEvent(std::exception_ptr ex);
        void RedrawDirtyEngineImageOutput();
        void FlushEngineImageOutputLayout(bool invalidate_all);
        void InitD2DDevice(bool force_software);
        void UpdateUIWidth(uint64_t new_width);
        uint64_t GetCalculatedUIHeight();
        // NOTE: Returns count of lines if height exceeds all lines
        size_t GetLineIndexFromHeight(uint64_t height);
        void InvalidateLineAtIndex(size_t line);
        void UpdateAndInvalidateActiveButton(Windows::Foundation::Point const& pt);
        bool TryFulfillInputRequest(bool clear_input);
        ID2D1SolidColorBrush* GetOrCreateSolidColorBrush(uint32_t color);
        IDWriteTextFormat* GetOrCreateTextFormat(hstring const& font_family);

        void OnInputCountDownTick(IInspectable const&, IInspectable const&);
        void FlushCurPrintLine();
        // NOTE: Wait flag is ignored deliberately
        void RoutinePrint(hstring content, PrintExtendedFlags flags);
        void RoutineHtmlPrint(hstring const& content);
        void RoutineInput(std::unique_ptr<InputRequest> request);
        void RoutineReuseLastLine(hstring const& content);
        void RoutineClearLine(uint64_t count);
        void RoutinePrintSourceButton(hstring const& content, hstring const& path,
            uint32_t line, uint32_t column, PrintExtendedFlags flags);
        void RoutinePrintButton(hstring const& content, hstring const& value, PrintExtendedFlags flags);

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

        DP_DECLARE(EngineForeColor);
        DP_DECLARE(EngineBackColor);
        DP_DECLARE(EngineTitle);

        std::shared_ptr<EngineSharedData> m_sd;
        util::sync::spsc::Receiver<std::move_only_function<void()>> m_ui_task_rx{ nullptr };
        util::sync::spsc::Sender<std::unique_ptr<EngineThreadTask>> m_engine_task_tx{ nullptr };

        MEraEmuWin::AppSettingsVM m_app_settings;
        EraExecutionBreakReason m_last_execution_break_reason = ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS;
        event<delegate<EraExecutionBreakReason>> m_ev_EngineExecutionInterrupted;
        Tenkai::UI::Xaml::Window m_devtools_wnd{ nullptr };
        event<Windows::Foundation::EventHandler<MEraEmuWin::EngineUnhandledExceptionEventArgs>> m_ev_UnhandledException;
        IVirtualSurfaceImageSourceNative* m_vsis_noref{};
        ISurfaceImageSourceNativeWithD2D* m_vsis_d2d_noref{};
        com_ptr<ID2D1DeviceContext> m_d2d_ctx;
        std::unordered_map<uint32_t, com_ptr<ID2D1SolidColorBrush>> m_brush_map;
        std::unordered_map<hstring, com_ptr<IDWriteTextFormat>> m_font_map;
        hstring m_default_font_name;
        uint64_t m_ui_width{};
        float m_xscale{ 1 }, m_yscale{ 1 };
        uint32_t m_focus_color{ D2D1::ColorF::Yellow };
        std::vector<EngineUIPrintLineData> m_ui_lines;
        bool m_reused_last_line{ false };
        uint32_t m_cur_line_alignment{};
        uint32_t m_cur_font_style{};
        hstring m_cur_font_name{};
        bool m_auto_redraw{};
        bool m_skip_display{};
        uint64_t m_no_skip_display_cnt{};
        uint64_t m_last_redraw_dirty_height{};
        struct ComposingLineData {
            struct ComposingLineDataPart {
                hstring str;
                uint32_t color;
                // TODO: Styles support (strikethrough, ...)
                bool forbid_button;     // true for PRINTPLAIN
                bool is_isolated;       // true for PRINTC
                std::optional<EngineUIPrintLineDataButton> explicit_buttons;
            };
            std::vector<ComposingLineDataPart> parts;
        } m_cur_composing_line;
        std::unique_ptr<InputRequest> m_outstanding_input_req;
        winrt::clock::time_point m_input_start_t;
        hstring m_input_last_prompt;
        Windows::UI::Xaml::DispatcherTimer m_input_countdown_timer;
        struct EngineConfig {
            float line_height = 16;
            float font_size = 14;
            int64_t printc_char_count = 25;
            int64_t printc_per_line = 5;
        } m_cfg;
        int64_t m_cur_printc_count{};
        bool m_user_skipping{};
        struct ActiveButtonData {
            size_t line;
            uint32_t button_idx;

            ActiveButtonData() : line(-1), button_idx() {}
            auto operator<=>(ActiveButtonData const& rhs) const noexcept = default;
            bool is_default() const noexcept { return line == -1; }
        } m_cur_active_button{};
        Windows::Foundation::Point m_cur_pt{};
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct EngineControl : EngineControlT<EngineControl, implementation::EngineControl> {};
}
