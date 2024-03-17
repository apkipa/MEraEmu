#pragma once

#include "winrt/Windows.UI.Xaml.h"
#include "winrt/Windows.UI.Xaml.Markup.h"
#include "winrt/Windows.UI.Xaml.Interop.h"
#include "winrt/Windows.UI.Xaml.Controls.Primitives.h"
#include "EngineUnhandledExceptionEventArgs.g.h"
#include "EngineControl.g.h"

#include "MEraEngine.hpp"

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

namespace winrt::MEraEmuWin::implementation {
    struct EngineSharedData;
    struct InputRequest;
    struct MEraEmuWinEngineSysCallback;
    struct EngineUIPrintLineData;
    struct InputRequest;

    struct EngineUnhandledExceptionEventArgs : EngineUnhandledExceptionEventArgsT<EngineUnhandledExceptionEventArgs> {
        EngineUnhandledExceptionEventArgs(hresult code, hstring const& msg) :
            m_code(code), m_msg(msg) {}

        hresult Code() noexcept { return m_code; }
        hstring Message() noexcept { return m_msg; }

    private:
        hresult m_code;
        hstring m_msg;
    };

    struct EngineControl : EngineControlT<EngineControl> {
        EngineControl();
        ~EngineControl();
        void InitializeComponent();

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

        void UserInputTextBox_KeyDown(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs const& e);

        // Non-midl methods
        void Bootstrap(hstring const& game_base_dir);

        void IsUserSkipping();

    private:
        friend EngineSharedData;
        friend MEraEmuWinEngineSysCallback;
        friend EngineUIPrintLineData;

        void InitEngineUI();
        void UpdateEngineUI();
        void EmitUnhandledExceptionEvent(std::exception_ptr ex);
        void UpdateEngineImageOutput();
        void InitD2DDevice(bool force_software);
        void UpdateUIWidth(uint64_t new_width);
        uint64_t GetCalculatedUIHeight();
        ID2D1SolidColorBrush* GetOrCreateSolidColorBrush(uint32_t color);
        IDWriteTextFormat* GetOrCreateTextFormat(hstring const& font_family);

        // NOTE: Wait flag is ignored deliberately
        void RoutinePrint(hstring const& content, PrintExtendedFlags flags);
        void RoutineHtmlPrint(hstring const& content);
        void RoutineInput(std::unique_ptr<InputRequest> request);

        DP_DECLARE(EngineForeColor);
        DP_DECLARE(EngineBackColor);
        DP_DECLARE(EngineTitle);

        std::shared_ptr<EngineSharedData> m_sd;
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
        struct ComposingLineData {
            struct ComposingLineDataPart {
                hstring str;
                uint32_t color;
                // TODO: Styles support (strikethrough, ...)
            };
            std::vector<ComposingLineDataPart> parts;
        } m_cur_composing_line;
        std::unique_ptr<InputRequest> m_outstanding_input_req;
        // TODO: Button style data
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct EngineControl : EngineControlT<EngineControl, implementation::EngineControl> {};
}
