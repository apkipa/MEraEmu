#include "pch.h"
#include "Settings/GamePage.h"
#if __has_include("Settings/GamePage.g.cpp")
#include "Settings/GamePage.g.cpp"
#endif

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Navigation;

namespace winrt::MEraEmuWin::Settings::implementation {
    GamePage::GamePage() {}
    void GamePage::OnNavigatedTo(NavigationEventArgs const& e) {
        m_parent_page_noref = e.Parameter().as<Settings::implementation::MainPage>().get();
    }
}
