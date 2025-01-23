#pragma once

#include "Settings/GamePage.g.h"
#include "Settings/MainPage.h"

namespace winrt::MEraEmuWin::Settings::implementation {
    struct GamePage : GamePageT<GamePage> {
        GamePage();

        Settings::MainPage ParentPage() { return *m_parent_page_noref; }

        void OnNavigatedTo(Windows::UI::Xaml::Navigation::NavigationEventArgs const& e);

    private:
        Settings::implementation::MainPage* m_parent_page_noref{ nullptr };
    };
}

namespace winrt::MEraEmuWin::Settings::factory_implementation {
    struct GamePage : GamePageT<GamePage, implementation::GamePage> {};
}
