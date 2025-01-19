#include "pch.h"
#include "Settings/GeneralPage.h"
#if __has_include("Settings/GeneralPage.g.cpp")
#include "Settings/GeneralPage.g.cpp"
#endif

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Navigation;

namespace winrt::MEraEmuWin::Settings::implementation {
    GeneralPage::GeneralPage() {}
    void GeneralPage::OnNavigatedTo(NavigationEventArgs const& e) {
        m_parent_page_noref = e.Parameter().as<Settings::implementation::MainPage>().get();
    }
}
