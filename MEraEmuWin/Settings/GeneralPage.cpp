﻿#include "pch.h"
#include "Settings/GeneralPage.h"
#include "Settings/GeneralPage.g.cpp"

using namespace winrt;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Navigation;

namespace winrt::MEraEmuWin::Settings::implementation {
    GeneralPage::GeneralPage() {}
    void GeneralPage::OnNavigatedTo(NavigationEventArgs const& e) {
        m_parent_page_noref = e.Parameter().as<Settings::implementation::MainPage>().get();
    }
}
