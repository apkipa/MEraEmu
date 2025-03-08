#pragma once

#include "CanvasVirtualControl.g.h"

namespace winrt::MEraEmuWin::implementation {
    struct CanvasVirtualControlVsis;

    struct CanvasVirtualControl : CanvasVirtualControlT<CanvasVirtualControl> {
        CanvasVirtualControl();
        ~CanvasVirtualControl();

        void InitializeComponent();

        void OnEffectiveViewportChanged(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::EffectiveViewportChangedEventArgs const& e);

        // Non-midl methods
        IVirtualSurfaceImageSourceNative* GetVsisNative() const noexcept;

    private:
        friend CanvasVirtualControlVsis;

        IVirtualSurfaceImageSourceNative* m_inner_vsis_noref;
        ISurfaceImageSourceNativeWithD2D* m_inner_vsis_d2d_noref;
        com_ptr<CanvasVirtualControlVsis> m_canvas_vsis;
        uint32_t m_xoffset{}, m_yoffset{};
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct CanvasVirtualControl : CanvasVirtualControlT<CanvasVirtualControl, implementation::CanvasVirtualControl> {};
}
