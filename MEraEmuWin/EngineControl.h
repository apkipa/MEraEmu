#pragma once

#include "winrt/Windows.UI.Xaml.h"
#include "winrt/Windows.UI.Xaml.Markup.h"
#include "winrt/Windows.UI.Xaml.Interop.h"
#include "winrt/Windows.UI.Xaml.Controls.Primitives.h"
#include "EngineControl.g.h"

namespace winrt::MEraEmuWin::implementation
{
    struct EngineControl : EngineControlT<EngineControl>
    {
        EngineControl() 
        {
            // Xaml objects should not call InitializeComponent during construction.
            // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent
        }

        int32_t MyProperty();
        void MyProperty(int32_t value);

        void ClickHandler(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);
    };
}

namespace winrt::MEraEmuWin::factory_implementation
{
    struct EngineControl : EngineControlT<EngineControl, implementation::EngineControl>
    {
    };
}
