#pragma once

#include "MainPage.g.h"

#include "EngineControl.h"

namespace winrt::MEraEmuWin::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage();
        ~MainPage();
        void InitializeComponent();

        void MenuFile_ResetEngine_Click(Windows::Foundation::IInspectable const&, Windows::UI::Xaml::RoutedEventArgs const&);
        void MenuFile_ReturnToTitle_Click(Windows::Foundation::IInspectable const&, Windows::UI::Xaml::RoutedEventArgs const&);
        void MenuFile_Exit_Click(Windows::Foundation::IInspectable const&, Windows::UI::Xaml::RoutedEventArgs const&);
        void MenuHelp_About_Click(Windows::Foundation::IInspectable const&, Windows::UI::Xaml::RoutedEventArgs const&);

    private:
        void BootstrapEngine();
        void SwitchTitleBar(bool enable);
        void ShowSimpleContentDialog(hstring const& title, hstring const& content);
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
