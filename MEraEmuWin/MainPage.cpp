#include "pch.h"
#include "MainPage.h"
#include "MainPage.g.cpp"
#include "Settings/MainPage.h"

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

        {
            // Load config from file
            LoadConfig();

            // Override settings from environment variables
            auto is_truthy_env_var = [](const char* name) -> std::optional<bool> {
                auto p = std::getenv(name);
                if (!p) { return std::nullopt; }
                return p[0] == '1' || p[0] == 't' || p[0] == 'T' || p[0] == 'y' || p[0] == 'Y';
            };
            if (auto p = is_truthy_env_var("MERAEMU_ENABLE_PARALLEL_LOADING")) {
                m_app_settings.EnableParallelLoading(*p);
            }
            if (auto p = is_truthy_env_var("MERAEMU_ENABLE_JIT")) {
                m_app_settings.EnableJIT(*p);
            }

            // Apply settings
            engine_ctrl.ApplySettings(m_app_settings);
        }

        // Automatically start the game engine
        dq.TryEnqueue(DispatcherQueuePriority::Low, [weak_this = get_weak()] {
            auto self = weak_this.get();
            if (!self) { return; }
            self->BootstrapEngine();
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
    void MainPage::MenuHelp_DevTools_Click(IInspectable const&, RoutedEventArgs const&) {
        auto ctrl = MainEngineControl();
        ctrl.IsDevToolsOpen(!ctrl.IsDevToolsOpen());
    }
    void MainPage::MenuHelp_Settings_Click(IInspectable const&, RoutedEventArgs const&) {
        auto main_wnd = Tenkai::UI::Xaml::Window::GetCurrentMain();
        auto settings_wnd = Tenkai::UI::Xaml::Window();
        auto settings_main_page = Settings::MainPage();
        settings_main_page.Settings(m_app_settings);
        settings_wnd.Content(settings_main_page);
        settings_wnd.Title(L"设置 - MEraEmu");
        auto main_hwnd = (HWND)main_wnd.View().Id().Value;
        auto settings_hwnd = (HWND)settings_wnd.View().Id().Value;
        RECT rt;
        GetWindowRect(main_hwnd, &rt);
        auto xmid = rt.left + (rt.right - rt.left) / 2;
        auto ymid = rt.top + (rt.bottom - rt.top) / 2;
        auto width = (rt.right - rt.left) * 0.8;
        auto height = (rt.bottom - rt.top) * 0.8;
        SetWindowPos(settings_hwnd, 0, xmid - width / 2, ymid - height / 2, width, height, SWP_NOZORDER);
        // Make modal
        SetWindowLongPtrW(settings_hwnd, GWL_EXSTYLE, GetWindowLongPtrW(settings_hwnd, GWL_EXSTYLE) | WS_EX_DLGMODALFRAME);
        SetWindowLongPtrW(settings_hwnd, GWL_STYLE, WS_POPUPWINDOW | WS_DLGFRAME | WS_THICKFRAME);
        SetWindowLongPtrW(settings_hwnd, GWLP_HWNDPARENT, (LONG_PTR)main_hwnd);
        EnableWindow(main_hwnd, false);
        settings_wnd.Activate();
        auto old_config = std::make_shared<std::string>(m_app_settings.as<implementation::AppSettingsVM>()->to_json_string());
        auto tear_down_fn = [=, settings_wnd = weak_ref(settings_wnd)] {
            EnableWindow(main_hwnd, true);
            ShowWindow(settings_hwnd, SW_HIDE);
            DispatcherQueue::GetForCurrentThread().TryEnqueue(DispatcherQueuePriority::Low, [=] {
                if (auto wnd = settings_wnd.get()) {
                    wnd.Close();
                }
            });
            // Apply settings
            MainEngineControl().ApplySettings(m_app_settings);
            if (*old_config != m_app_settings.as<implementation::AppSettingsVM>()->to_json_string()) {
                SaveConfig();
            }
        };
        settings_wnd.View().Closing([=](auto&&, auto&&) {
            tear_down_fn();
        });
        settings_main_page.KeyDown([=](auto&&, auto&& e) {
            if (e.Key() == VirtualKey::Escape) {
                e.Handled(true);
                tear_down_fn();
            }
        });
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
            auto engine_ctrl = MainEngineControl().try_as<EngineControl>();
            if (!engine_ctrl) { return; }
            engine_ctrl->Bootstrap(game_base_dir);
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
    void MainPage::LoadConfig() {
        std::wstring file_path = L".\\MEraEmu.config.json";
        file_handle file{ CreateFileW(file_path.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr) };
        if (file) {
            DWORD size = GetFileSize(file.get(), nullptr);
            std::vector<char> buf(size);
            DWORD read;
            std::ignore = ReadFile(file.get(), buf.data(), size, &read, nullptr);
            if (read == size) {
                auto j = nlohmann::json::parse(buf);
                from_json(j, *m_app_settings.as<implementation::AppSettingsVM>());
            }
        }
    }
    void MainPage::SaveConfig() {
        std::wstring file_path = L".\\MEraEmu.config.json";
        file_handle file{ CreateFileW(file_path.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr) };
        if (file) {
            auto s = m_app_settings.as<implementation::AppSettingsVM>()->to_json_string();
            DWORD written;
            WriteFile(file.get(), s.data(), s.size(), &written, nullptr);
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
    void MainPage::ShowSimpleContentDialog(hstring const& title, hstring const& content) {
        ContentDialog cd;
        cd.XamlRoot(XamlRoot());
        cd.Title(box_value(title));
        cd.Content(box_value(content));
        cd.CloseButtonText(L"OK");
        cd.DefaultButton(ContentDialogButton::Close);
        cd.ShowAsync();
    }
}
