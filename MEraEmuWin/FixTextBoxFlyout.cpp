#include "pch.h"
#include "FixTextBoxFlyout.h"
#include "FixTextBoxFlyout.g.cpp"

#define DP_NAMESPACE ::winrt::MEraEmuWin
#define DP_CLASS FixTextBoxFlyout

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

namespace winrt::MEraEmuWin::implementation {
    void FixTextBoxFlyout::SetIsActive(FrameworkElement const& element, bool value) {
        element.SetValue(IsActiveProperty(), box_value(value));
    }
    bool FixTextBoxFlyout::GetIsActive(FrameworkElement const& element) {
        return unbox_value<bool>(element.GetValue(IsActiveProperty()));
    }
    void FixTextBoxFlyout::OnIsActiveChanged(DependencyObject const& d, DependencyPropertyChangedEventArgs const& e) {
        auto tb = d.try_as<TextBox>();
        if (!tb) { return; }

        if (auto val = unbox_value<bool>(e.NewValue())) {
            auto flyout = Microsoft::UI::Xaml::Controls::TextCommandBarFlyout();
            flyout.Placement(Primitives::FlyoutPlacementMode::BottomEdgeAlignedLeft);
            tb.ContextFlyout(flyout);
            flyout = Microsoft::UI::Xaml::Controls::TextCommandBarFlyout();
            flyout.Placement(Primitives::FlyoutPlacementMode::TopEdgeAlignedLeft);
            tb.SelectionFlyout(flyout);
        }
    }

    DP_DEFINE_ATTACHED_PROP_TY(IsActive, bool, box_value(false), &FixTextBoxFlyout::OnIsActiveChanged);
}
