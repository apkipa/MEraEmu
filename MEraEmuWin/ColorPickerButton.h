#pragma once

#include "winrt/Windows.UI.Xaml.h"
#include "winrt/Windows.UI.Xaml.Markup.h"
#include "winrt/Windows.UI.Xaml.Interop.h"
#include "winrt/Windows.UI.Xaml.Controls.Primitives.h"
#include "ColorPickerButton.g.h"

#include "DPHelper.h"

namespace winrt::MEraEmuWin::implementation {
    struct ColorPickerButton : ColorPickerButtonT<ColorPickerButton> {
        ColorPickerButton();

        DP_DEFINE_GETSETTER(SelectedColor, Windows::UI::Color);

        DP_DECLARE_PROP_METHOD(SelectedColor);

    private:
        DP_DECLARE_PROP(SelectedColor);
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct ColorPickerButton : ColorPickerButtonT<ColorPickerButton, implementation::ColorPickerButton> {};
}
