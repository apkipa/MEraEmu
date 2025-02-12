#include "pch.h"
#include "Settings/DisplayPage.h"
#include "Settings/DisplayPage.g.cpp"

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Navigation;

namespace winrt::MEraEmuWin::Settings::implementation {
    DisplayPage::DisplayPage() {}
    void DisplayPage::OnNavigatedTo(NavigationEventArgs const& e) {
        m_parent_page_noref = e.Parameter().as<Settings::implementation::MainPage>().get();
    }
}
