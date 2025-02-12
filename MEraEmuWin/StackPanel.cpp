#include "pch.h"
#include "StackPanel.h"
#include "StackPanel.g.cpp"

#define DP_NAMESPACE ::winrt::MEraEmuWin
#define DP_CLASS StackPanel

using namespace winrt;
using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;

// Source: WinUI 3 (https://github.com/microsoft/microsoft-ui-xaml)

static Size CollapseThickness(Thickness const& thickness) {
    return Size{ float(thickness.Left + thickness.Right), float(thickness.Top + thickness.Bottom) };
}
static Rect DeflateRect(Rect const& rect, Thickness const& thickness) {
    return {
        (float)(rect.X + thickness.Left),
        (float)(rect.Y + thickness.Top),
        (float)std::max(0.0, rect.Width - (thickness.Left + thickness.Right)),
        (float)std::max(0.0, rect.Height - (thickness.Top + thickness.Bottom)),
    };
}

namespace winrt::MEraEmuWin::implementation {
    StackPanel::StackPanel() {}
    Size StackPanel::MeasureOverride(Size const& availableSize) {
        constexpr float FLOAT_INF = std::numeric_limits<float>::infinity();

        auto desiredSize = Size{};
        auto combinedThickness = CollapseThickness(Padding());
        auto orientation = Orientation();
        auto childConstraint = orientation == Controls::Orientation::Vertical ?
            Size{ availableSize.Width - combinedThickness.Width, FLOAT_INF } :
            Size{ FLOAT_INF, availableSize.Height - combinedThickness.Height };

        uint32_t visibleChildrenCount = 0;
        for (auto child : Children()) {
            child.Measure(childConstraint);
            auto childDesiredSize = child.DesiredSize();

            if (orientation == Controls::Orientation::Vertical) {
                desiredSize.Width = std::max(desiredSize.Width, childDesiredSize.Width);
                desiredSize.Height += childDesiredSize.Height;
            }
            else {
                desiredSize.Width += childDesiredSize.Width;
                desiredSize.Height = std::max(desiredSize.Height, childDesiredSize.Height);
            }

            if (child.Visibility() == Visibility::Visible) {
                visibleChildrenCount++;
            }
        }

        desiredSize.Width += combinedThickness.Width;
        desiredSize.Height += combinedThickness.Height;

        if (visibleChildrenCount > 1) {
            const auto combinedSpacing = float(Spacing() * (visibleChildrenCount - 1));
            if (orientation == Controls::Orientation::Vertical) {
                desiredSize.Height += combinedSpacing;
            }
            else {
                desiredSize.Width += combinedSpacing;
            }
        }

        return desiredSize;
    }
    Size StackPanel::ArrangeOverride(Size const& finalSize) {
        auto orientation = Orientation();
        const auto innerRect = DeflateRect(Rect{ 0, 0, finalSize.Width, finalSize.Height }, Padding());
        auto arrangeRect = innerRect;
        const auto spacing = Spacing();
        const auto contentAlignment = ContentAlignment();

        for (auto child : Children()) {
            auto childDesiredSize = child.DesiredSize();

            if (orientation == Controls::Orientation::Vertical) {
                arrangeRect.Height = childDesiredSize.Height;
                arrangeRect.Width = std::max(innerRect.Width, childDesiredSize.Width);

                // When child is stretched and has a MaxWidth, adjust the width of the child according to ContentAlignment
                if (contentAlignment == ContentAlignment1D::Start || contentAlignment == ContentAlignment1D::End) {
                    if (auto fe = child.try_as<FrameworkElement>()) {
                        auto horizontalAlignment = fe.HorizontalAlignment();
                        auto maxWidth = fe.MaxWidth();
                        if (horizontalAlignment == HorizontalAlignment::Stretch && !std::isnan(maxWidth)) {
                            arrangeRect.Width = std::min(arrangeRect.Width, (float)maxWidth);
                            if (contentAlignment == ContentAlignment1D::End) {
                                arrangeRect.X += innerRect.Width - arrangeRect.Width;
                            }
                        }
                    }
                }
            }
            else {
                arrangeRect.Width = childDesiredSize.Width;
                arrangeRect.Height = std::max(innerRect.Height, childDesiredSize.Height);

                // When child is stretched and has a MaxHeight, adjust the height of the child according to ContentAlignment
                if (contentAlignment == ContentAlignment1D::Start || contentAlignment == ContentAlignment1D::End) {
                    if (auto fe = child.try_as<FrameworkElement>()) {
                        auto verticalAlignment = fe.VerticalAlignment();
                        auto maxHeight = fe.MaxHeight();
                        if (verticalAlignment == VerticalAlignment::Stretch && !std::isnan(maxHeight)) {
                            arrangeRect.Height = std::min(arrangeRect.Height, (float)maxHeight);
                            if (contentAlignment == ContentAlignment1D::End) {
                                arrangeRect.Y += innerRect.Height - arrangeRect.Height;
                            }
                        }
                    }
                }
            }

            child.Arrange(arrangeRect);

            if (orientation == Controls::Orientation::Vertical) {
                arrangeRect.X = innerRect.X;
            }
            else {
                arrangeRect.Y = innerRect.Y;
            }

            if (child.Visibility() == Visibility::Visible) {
                if (orientation == Controls::Orientation::Vertical) {
                    arrangeRect.Y += float(childDesiredSize.Height + spacing);
                }
                else {
                    arrangeRect.X += float(childDesiredSize.Width + spacing);
                }
            }
        }

        return finalSize;
    }
    void StackPanel::OnLayoutPropertyChanged(const DependencyObject& d, const DependencyPropertyChangedEventArgs&) {
        auto self = d.as<StackPanel>();
        self->InvalidateMeasure();
        //self->InvalidateArrange();
    }

    DP_DEFINE_PROP(Orientation, box_value(Windows::UI::Xaml::Controls::Orientation::Horizontal), &StackPanel::OnLayoutPropertyChanged);
    DP_DEFINE_PROP(Padding, box_value(Windows::UI::Xaml::Thickness{ 0 }), &StackPanel::OnLayoutPropertyChanged);
    DP_DEFINE_PROP(Spacing, box_value(0.0), &StackPanel::OnLayoutPropertyChanged);
    DP_DEFINE_PROP(ContentAlignment, box_value(MEraEmuWin::ContentAlignment1D::Stretch), &StackPanel::OnLayoutPropertyChanged);
}
