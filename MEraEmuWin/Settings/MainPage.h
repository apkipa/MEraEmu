#pragma once

#include "Settings/MainPage.g.h"

namespace winrt::MEraEmuWin::Settings::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage();

        void InitializeComponent();

        MEraEmuWin::AppSettingsVM Settings() { return m_settings; }
        void Settings(MEraEmuWin::AppSettingsVM const& value) { m_settings = value; }

        void NavView_SelectionChanged(Microsoft::UI::Xaml::Controls::NavigationView const& sender, Microsoft::UI::Xaml::Controls::NavigationViewSelectionChangedEventArgs const& args);

    private:
        MEraEmuWin::AppSettingsVM m_settings{ nullptr };
    };
}

namespace winrt::MEraEmuWin::Settings::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
