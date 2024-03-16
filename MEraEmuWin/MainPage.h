#pragma once

#include "MainPage.g.h"

#include "EngineControl.h"

namespace winrt::MEraEmuWin::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage();
        void InitializeComponent();

    private:
        void SwitchTitleBar(bool enable);
        void ShowErrorDialog(hstring const& title, hstring const& content);
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
