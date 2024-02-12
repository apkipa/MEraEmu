#pragma once

#include "MainPage.g.h"

namespace winrt::MEraEmuWin::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage();
        void InitializeComponent();

        void ClickHandler(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);
        void SwitchTitleBarButtonClick(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);
        void SwitchBackgroundTransparencyButtonClick(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);
        void AskBeforeExitButtonClick(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);
        fire_and_forget RestartAppButtonClick(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& args);

    private:
        void SwitchTitleBar(bool enable);

        bool m_ask_before_close{};
        bool m_is_asking{};
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
