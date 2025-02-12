#include "pch.h"
#include "Settings/MainPage.h"
#include "Settings/MainPage.g.cpp"

using namespace winrt;
using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::UI::Xaml::Controls;
namespace muxc = Microsoft::UI::Xaml::Controls;

namespace winrt::MEraEmuWin::Settings::implementation {
    MainPage::MainPage() {}
    void MainPage::InitializeComponent() {
        MainPageT::InitializeComponent();

        auto nav_view = NavView();
        auto general_nvi = GeneralNVI();
        general_nvi.Tag(box_value(xaml_typename<MEraEmuWin::Settings::GeneralPage>()));
        nav_view.SelectedItem(general_nvi);
        GameNVI().Tag(box_value(xaml_typename<MEraEmuWin::Settings::GamePage>()));
        DisplayNVI().Tag(box_value(xaml_typename<MEraEmuWin::Settings::DisplayPage>()));
    }
    void MainPage::NavView_SelectionChanged(muxc::NavigationView const&, muxc::NavigationViewSelectionChangedEventArgs const& e) {
        auto selected_item = e.SelectedItemContainer();
        if (selected_item == nullptr) {
            return;
        }
        auto tag = unbox_value<TypeName>(selected_item.Tag());
        ContentFrame().Navigate(tag, *this);
    }
}
