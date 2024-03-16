#include "pch.h"
#include "MainPage.h"
#include "MainPage.g.cpp"

#include <winrt/Tenkai.UI.Xaml.h>
#include <winrt/Tenkai.UI.ViewManagement.h>

#include <Tenkai.hpp>

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Core;

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

        MainEngineControl().UnhandledException([this](auto&&, MEraEmuWin::EngineUnhandledExceptionEventArgs const& e) {
            ShowErrorDialog(L"运行引擎时出错", hstring(
                std::format(L"引擎汇报了无法处理的错误:\n0x{:08x}\n{}",
                    static_cast<uint32_t>(e.Code()), e.Message()
                ))
            );
        });

        Dispatcher().RunAsync(CoreDispatcherPriority::Low, [self = get_strong()]() {
            hstring game_base_dir{ L".\\" };
            try {
                self->MainEngineControl().as<EngineControl>()->Bootstrap(game_base_dir);
            }
            catch (hresult_error const& e) {
                self->ShowErrorDialog(L"启动引擎时出错", hstring(
                    std::format(L"引擎汇报了无法处理的错误:\n0x{:08x}\n{}",
                        static_cast<uint32_t>(e.code()), e.message()
                    ))
                );
            }
            catch (...) {
                std::abort();
            }
        });
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
    void MainPage::ShowErrorDialog(hstring const& title, hstring const& content) {
        ContentDialog cd;
        cd.XamlRoot(XamlRoot());
        cd.Title(box_value(title));
        cd.Content(box_value(content));
        cd.CloseButtonText(L"OK");
        cd.ShowAsync();
    }
}
