#pragma once

#include "FixTextBoxFlyout.g.h"

#include "DPHelper.h"

namespace winrt::MEraEmuWin::implementation {
    struct FixTextBoxFlyout {
        static void SetIsActive(Windows::UI::Xaml::FrameworkElement const& element, bool value);
        static bool GetIsActive(Windows::UI::Xaml::FrameworkElement const& element);

        DP_DECLARE_PROP_METHOD(IsActive);

    private:
        static void OnIsActiveChanged(Windows::UI::Xaml::DependencyObject const& d, Windows::UI::Xaml::DependencyPropertyChangedEventArgs const& e);

        DP_DECLARE_PROP(IsActive);
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct FixTextBoxFlyout : FixTextBoxFlyoutT<FixTextBoxFlyout, implementation::FixTextBoxFlyout> {};
}
