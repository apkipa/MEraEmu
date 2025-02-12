#include "pch.h"

#include "App.h"
#include "MainPage.h"

#include <winrt/Tenkai.h>
#include <winrt/Tenkai.UI.Xaml.h>
#include <winrt/Tenkai.UI.ViewManagement.h>

#include <Tenkai.hpp>

using namespace winrt;
using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::Foundation;
using namespace Windows::UI::Core;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Navigation;
using namespace Windows::UI::Xaml::Input;
using namespace MEraEmuWin;
using namespace MEraEmuWin::implementation;

/// <summary>
/// Creates the singleton application object.  This is the first line of authored code
/// executed, and as such is the logical equivalent of main() or WinMain().
/// </summary>
App::App() {
    Suspending({ this, &App::OnSuspending });

    UnhandledException([this](IInspectable const&, UnhandledExceptionEventArgs const& e) {
        if (IsDebuggerPresent()) {
            auto errorMessage = e.Message();
            __debugbreak();
        }
        else {
            e.Handled(true);
            HWND hwnd{};
            if (auto wnd = Tenkai::UI::Xaml::Window::GetCurrentMain()) {
                hwnd = (HWND)wnd.View().Id().Value;
            }
            else {
                hwnd = GetForegroundWindow();
            }
            auto rt = MessageBoxW(hwnd,
                std::format(L"The application encountered an unexpected error:\n\n{}\n\n"
                    "Select `Retry` to continue, or `Cancel` to exit the application.",
                    e.Message()
                ).c_str(),
                L"Unhandled Exception",
                MB_ICONERROR | MB_RETRYCANCEL
            );
            switch (rt) {
            case IDRETRY:
                break;
            case IDCANCEL:
            default:
                Tenkai::AppService::Quit();
                break;
            }
        }
    });
}

/// <summary>
/// Invoked when the application is launched normally by the end user.  Other entry points
/// will be used such as when the application is launched to open a specific file.
/// </summary>
/// <param name="e">Details about the launch request and process.</param>
void App::OnLaunched(LaunchActivatedEventArgs const& e) {
    if constexpr (true) {
        // TODO: Remove this when Tenkai.UWP is updated to v0.1.7
        // Manually register resources
        struct InvertedBoolConverter : implements<InvertedBoolConverter, Windows::UI::Xaml::Data::IValueConverter> {
            InvertedBoolConverter() {}
            IInspectable Convert(IInspectable const& value, Windows::UI::Xaml::Interop::TypeName const&, IInspectable const&, hstring const&) {
                if (!value) {
                    return nullptr;
                }
                return box_value(!unbox_value<bool>(value));
            }
            IInspectable ConvertBack(IInspectable const& value, Windows::UI::Xaml::Interop::TypeName const&, IInspectable const&, hstring const&) {
                if (!value) {
                    return nullptr;
                }
                return box_value(!unbox_value<bool>(value));
            }
        };
        Resources().Insert(box_value(L"InvertedBoolConverter"), make<InvertedBoolConverter>());
    }

    Tenkai::UI::Xaml::Window main_wnd;
    main_wnd.ExtendsContentIntoTitleBar(true);
    main_wnd.Activate();
    Frame frame;
    frame.Navigate(xaml_typename<MEraEmuWin::MainPage>());
    main_wnd.Content(frame);
    // TODO: Focus indicator cannot be hidden on Win10 on app launch,
    //       maybe workaround it?
    // Fix focus indicator on app launch
    if (false && !tenkai::win32::GetCurrentOSVersion().is_win11_or_newer()) {
        static auto get_last_input_t = []() {
            LASTINPUTINFO lii{ sizeof lii };
            check_bool(GetLastInputInfo(&lii));
            return lii.dwTime;
        };
        auto last_input_time = get_last_input_t();
        auto et = std::make_shared_for_overwrite<event_token>();
        *et = FocusManager::GettingFocus([=](auto&&, GettingFocusEventArgs const& e) {
            auto cur_input_time = get_last_input_t();
            if (cur_input_time != last_input_time) {
                // Revoke
                FocusManager::GettingFocus(*et);
                return;
            }
            if (e.FocusState() == FocusState::Pointer) {
                return;
            }
            // For simplicity, unconditionally convert to pointer focus or cancel focus
            e.TryCancel();
            auto new_elem = e.NewFocusedElement().as<Control>();
            new_elem.Dispatcher().RunAsync(CoreDispatcherPriority::Normal, [=] {
                //frame.IsTabStop(true);
                //frame.Focus(FocusState::Programmatic);
                //frame.IsTabStop(false);
                new_elem.Focus(FocusState::Pointer);
            });
            // Revoke
            //FocusManager::GettingFocus(*et);
        });
    }
}

/// <summary>
/// Invoked when application execution is being suspended.  Application state is saved
/// without knowing whether the application will be terminated or resumed with the contents
/// of memory still intact.
/// </summary>
/// <param name="sender">The source of the suspend request.</param>
/// <param name="e">Details about the suspend request.</param>
void App::OnSuspending([[maybe_unused]] IInspectable const& sender, [[maybe_unused]] SuspendingEventArgs const& e)
{
    // Save application state and stop any background activity
}

/// <summary>
/// Invoked when Navigation to a certain page fails
/// </summary>
/// <param name="sender">The Frame which failed navigation</param>
/// <param name="e">Details about the navigation failure</param>
void App::OnNavigationFailed(IInspectable const&, NavigationFailedEventArgs const& e)
{
    throw hresult_error(E_FAIL, hstring(L"Failed to load Page ") + e.SourcePageType().Name);
}