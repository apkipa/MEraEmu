#pragma once

#include "StackPanel.g.h"

#include "DPHelper.h"

namespace winrt::MEraEmuWin::implementation {
    struct StackPanel : StackPanelT<StackPanel> {
        StackPanel();

        Windows::Foundation::Size MeasureOverride(Windows::Foundation::Size const& availableSize);
        Windows::Foundation::Size ArrangeOverride(Windows::Foundation::Size const& finalSize);

        DP_DECLARE_PROP_METHOD(Orientation);
        DP_DECLARE_PROP_METHOD(Padding);
        DP_DECLARE_PROP_METHOD(Spacing);
        DP_DECLARE_PROP_METHOD(ContentAlignment);

        DP_DEFINE_GETSETTER(Orientation, Windows::UI::Xaml::Controls::Orientation);
        DP_DEFINE_GETSETTER(Padding, Windows::UI::Xaml::Thickness);
        DP_DEFINE_GETSETTER(Spacing, double);
        DP_DEFINE_GETSETTER(ContentAlignment, MEraEmuWin::ContentAlignment1D);

    private:
        static void OnLayoutPropertyChanged(const Windows::UI::Xaml::DependencyObject& d, const Windows::UI::Xaml::DependencyPropertyChangedEventArgs& e);

        DP_DECLARE_PROP(Orientation);
        DP_DECLARE_PROP(Padding);
        DP_DECLARE_PROP(Spacing);
        DP_DECLARE_PROP(ContentAlignment);
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct StackPanel : StackPanelT<StackPanel, implementation::StackPanel> {};
}
