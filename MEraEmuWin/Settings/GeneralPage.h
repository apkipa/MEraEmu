#pragma once

#include "Settings/GeneralPage.g.h"
#include "Settings/MainPage.h"

namespace winrt::MEraEmuWin::Settings::implementation {
    struct GeneralPage : GeneralPageT<GeneralPage> {
        GeneralPage();

        Settings::MainPage ParentPage() { return *m_parent_page_noref; }

        void OnNavigatedTo(Windows::UI::Xaml::Navigation::NavigationEventArgs const& e);

    private:
        Settings::implementation::MainPage* m_parent_page_noref{ nullptr };
    };
}

namespace winrt::MEraEmuWin::Settings::factory_implementation {
    struct GeneralPage : GeneralPageT<GeneralPage, implementation::GeneralPage> {};
}
