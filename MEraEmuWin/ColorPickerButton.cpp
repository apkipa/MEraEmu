#include "pch.h"
#include "ColorPickerButton.h"
#include "ColorPickerButton.g.cpp"

using namespace winrt;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;

#define DP_NAMESPACE ::winrt::MEraEmuWin
#define DP_CLASS ColorPickerButton

namespace winrt::MEraEmuWin::implementation {
    ColorPickerButton::ColorPickerButton() {}

    DP_DEFINE_PROP(SelectedColor, box_value(Color(255, 0, 0, 0)));
}
