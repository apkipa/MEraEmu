#include "pch.h"
#include "MainPage.h"
#include "MainPage.g.cpp"

#include <winrt/Tenkai.UI.Xaml.h>
#include <winrt/Tenkai.UI.ViewManagement.h>

#include <Tenkai.hpp>

using namespace winrt;
using namespace Windows::UI::Xaml;

namespace winrt::Tenkai {
    using UI::Xaml::Window;
}

namespace winrt::MEraEmuWin::implementation {
    MainPage::MainPage() {
        // TODO...
    }
    void MainPage::InitializeComponent() {
        MainPageT::InitializeComponent();

        SwitchTitleBar(true);
    }

    void MainPage::ClickHandler(IInspectable const&, RoutedEventArgs const&) {
        myButton().Content(box_value(L"Clicked"));
    }

    void MainPage::SwitchTitleBarButtonClick(IInspectable const&, RoutedEventArgs const&) {
        auto wnd = Tenkai::Window::GetCurrentMain();
        auto cur_is_extended = !wnd.ExtendsContentIntoTitleBar();
        SwitchTitleBar(cur_is_extended);
    }
    void MainPage::SwitchBackgroundTransparencyButtonClick(IInspectable const&, RoutedEventArgs const&) {
        auto wnd = Tenkai::Window::GetCurrentMain();
        auto wv = wnd.View();
        wv.IsBackgroundTransparent(!wv.IsBackgroundTransparent());
    }
    void MainPage::AskBeforeExitButtonClick(IInspectable const&, RoutedEventArgs const&) {
        if (m_ask_before_close) { return; }
        m_ask_before_close = true;

        auto wnd = Tenkai::Window::GetCurrentMain();
        auto wv = wnd.View();
        wv.Closing([this](auto&& sender, Tenkai::UI::ViewManagement::WindowViewClosingEventArgs const& e) -> fire_and_forget {
            using namespace Windows::UI::Xaml::Controls;

            auto that = get_strong();

            e.Handled(true);
            if (m_is_asking) { co_return; }
            m_is_asking = true;
            ContentDialog cd;
            cd.XamlRoot(this->myButton().XamlRoot());
            cd.Title(box_value(L"Are you sure you want to exit?"));
            cd.PrimaryButtonText(L"Yes");
            cd.CloseButtonText(L"No");
            auto result = co_await cd.ShowAsync();
            that->m_is_asking = false;
            if (result == ContentDialogResult::Primary) {
                Tenkai::AppService::Quit();
            }
        });
    }
    fire_and_forget MainPage::RestartAppButtonClick(IInspectable const&, RoutedEventArgs const&) {
        if (co_await Tenkai::AppService::RequestRestartAsync({})) {
            Tenkai::AppService::Quit();
        }
    }
    void MainPage::SwitchTitleBar(bool enable) {
        auto wnd = Tenkai::Window::GetCurrentMain();
        wnd.ExtendsContentIntoTitleBar(enable);
        auto tb = wnd.View().TitleBar();
        auto tb_ex = tb.try_as<Tenkai::UI::ViewManagement::ISupportsAdvancedCaptionCustomization>();
        if (enable) {
            wnd.SetTitleBar(TopDragRectangle());
            if (tb_ex) {
                //tb_ex.ButtonShape({ 46, 40 });
            }
            TopElasticRightSpace().Width(GridLengthHelper::FromPixels(tb.RightInset() - 20));
        }
        else {
            wnd.SetTitleBar(nullptr);
            TopElasticRightSpace().Width(GridLengthHelper::FromPixels(0));
        }
    }
}
