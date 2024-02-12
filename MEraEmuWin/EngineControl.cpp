#include "pch.h"
#include "EngineControl.h"
#if __has_include("EngineControl.g.cpp")
#include "EngineControl.g.cpp"
#endif

using namespace winrt;
using namespace Windows::UI::Xaml;

namespace winrt::MEraEmuWin::implementation
{
    int32_t EngineControl::MyProperty()
    {
        throw hresult_not_implemented();
    }

    void EngineControl::MyProperty(int32_t /* value */)
    {
        throw hresult_not_implemented();
    }

    void EngineControl::ClickHandler(IInspectable const&, RoutedEventArgs const&)
    {
        Button().Content(box_value(L"Clicked"));
    }
}
