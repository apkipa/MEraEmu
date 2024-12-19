#include "pch.h"
#include "MainPage.h"
#include "MainPage.g.cpp"

#include <winrt/Tenkai.UI.Xaml.h>
#include <winrt/Tenkai.UI.ViewManagement.h>

#include <Tenkai.hpp>

#include <CommCtrl.h>

#pragma comment(lib, "Comctl32.lib")

using namespace winrt;
using namespace Windows::System;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Core;

namespace winrt::Tenkai {
    using UI::Xaml::Window;
}

LRESULT CALLBACK SubclassWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, UINT_PTR uIdSubclass, DWORD_PTR dwRefData) {
    auto main_page = (MEraEmuWin::implementation::MainPage*)dwRefData;
    try {
        if (msg == WM_SYSKEYUP && wParam == VK_MENU) {
            //Microsoft::UI::Xaml::Controls::MenuBarItem mbi{ nullptr };
            //auto menu_bar = main_page->MainMenuBar();
            //// Focus on menu
            //for (auto&& item : menu_bar.Items()) {
            //    if (!mbi) { mbi = item; }
            //    item.IsTabStop(true);
            //}
            //mbi.Focus(FocusState::Keyboard);
            //return 0;
        }
        else if (msg == WM_SYSCOMMAND && wParam == SC_KEYMENU && lParam == 0) {
            return 0;
        }
    }
    catch (...) {}
    return DefSubclassProc(hwnd, msg, wParam, lParam);
}

namespace winrt::MEraEmuWin::implementation {
    MainPage::MainPage() {
    }
    MainPage::~MainPage() {
        // Remove WndProc hook
        auto wnd = Tenkai::UI::Xaml::Window::GetCurrentMain();
        if (wnd) {
            auto hwnd = (HWND)wnd.View().Id().Value;
            RemoveWindowSubclass(hwnd, SubclassWndProc, 1);
        }
    }
    void MainPage::InitializeComponent() {
        MainPageT::InitializeComponent();

        SwitchTitleBar(true);

        auto dq = DispatcherQueue::GetForCurrentThread();
        auto engine_ctrl = MainEngineControl();
        engine_ctrl.UnhandledException([this](auto&&, MEraEmuWin::EngineUnhandledExceptionEventArgs const& e) {
            ShowSimpleContentDialog(L"运行引擎时出错", hstring(
                std::format(L"引擎汇报了无法处理的错误:\n0x{:08x}\n{}",
                    static_cast<uint32_t>(e.Code()), e.Message()
                ))
            );
        });
        engine_ctrl.RegisterPropertyChangedCallback(engine_ctrl.EngineTitleProperty(), [this](DependencyObject const& sender, auto&&) {
            auto engine_ctrl = sender.as<EngineControl>();
            auto title = engine_ctrl->EngineTitle();
            TitleTextBlock().Text(title);
            Tenkai::UI::Xaml::Window::GetCurrentMain().Title(title);
        });

        // Handle Alt key for menu
        LayoutRoot().KeyDown([this](auto&&, KeyRoutedEventArgs const& e) {
            auto key = e.Key();
            if (key == VirtualKey::Menu) {
                e.Handled(true);
            }
            else if (key == VirtualKey::Escape) {
                e.Handled(true);
                MainEngineControl().Focus(FocusState::Keyboard);
            }
        });
        // TODO: Maybe fix the Alt key issue by adding new APIs to Tenkai.UWP?
        LayoutRoot().KeyUp([this](auto&&, KeyRoutedEventArgs const& e) {
            auto key = e.Key();
            if (key == VirtualKey::Menu || key == VirtualKey::F10) {
                e.Handled(true);
                Microsoft::UI::Xaml::Controls::MenuBarItem mbi{ nullptr };
                auto menu_bar = MainMenuBar();
                bool need_unfocus{};
                for (auto&& item : menu_bar.Items()) {
                    // Try focus
                    if (!mbi) { mbi = item; }
                    if (item.FocusState() != FocusState::Unfocused) {
                        need_unfocus = true;
                        break;
                    }
                    item.IsTabStop(true);
                }
                if (!need_unfocus) {
                    mbi.Focus(FocusState::Keyboard);
                }
                else {
                    // Unfocus
                    MainEngineControl().Focus(FocusState::Keyboard);
                }
            }
        });
        auto hwnd = (HWND)Tenkai::UI::Xaml::Window::GetCurrentMain().View().Id().Value;
        // NOTE: Deliberately ignores errors
        SetWindowSubclass(hwnd, SubclassWndProc, 1, (DWORD_PTR)this);
        MainMenuBar().LostFocus([this](auto&&, auto&&) {
            for (auto&& item : MainMenuBar().Items()) {
                item.IsTabStop(false);
            }
        });

        // Automatically start the game engine
        dq.TryEnqueue(DispatcherQueuePriority::Low, [this]() {
            BootstrapEngine();
        });
    }

    void MainPage::MenuFile_ResetEngine_Click(IInspectable const&, RoutedEventArgs const&) {
        BootstrapEngine();
    }
    void MainPage::MenuFile_ReturnToTitle_Click(IInspectable const&, RoutedEventArgs const&) {
        MainEngineControl().ReturnToTitle();
    }
    void MainPage::MenuFile_Exit_Click(IInspectable const&, RoutedEventArgs const&) {
        Tenkai::AppService::Quit();
    }
    void MainPage::MenuFile_DevTools_Click(IInspectable const&, RoutedEventArgs const&) {
        auto ctrl = MainEngineControl();
        ctrl.IsDevToolsOpen(!ctrl.IsDevToolsOpen());
    }
    void MainPage::MenuHelp_About_Click(IInspectable const&, RoutedEventArgs const&) {
        ShowSimpleContentDialog(L"关于 MEraEmu", hstring(std::format(
            L"UI 版本: {}\n"
            L"引擎版本: {}\n"
            L"作者: apkipa",
            L"v0.2.0",
            to_hstring(MEraEngine::get_version())
        )));
    }
    void MainPage::BootstrapEngine() {
        hstring game_base_dir{ L".\\" };
        try {
            MainEngineControl().as<EngineControl>()->Bootstrap(game_base_dir);
        }
        catch (hresult_error const& e) {
            ShowSimpleContentDialog(L"启动引擎时出错", hstring(
                std::format(L"引擎汇报了无法处理的错误:\n0x{:08x}\n{}",
                    static_cast<uint32_t>(e.code()), e.message()
                ))
            );
        }
        catch (...) { std::abort(); }
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
    void MainPage::ShowSimpleContentDialog(hstring const& title, hstring const& content) {
        ContentDialog cd;
        cd.XamlRoot(XamlRoot());
        cd.Title(box_value(title));
        cd.Content(box_value(content));
        cd.CloseButtonText(L"OK");
        cd.ShowAsync();
    }
}
