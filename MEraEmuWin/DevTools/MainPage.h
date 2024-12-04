#pragma once

#include "DevTools/MainPage.g.h"
#include "DevTools/SourcesFileTabItem.h"
#include "DevTools/SourcesTabCallStackTabItem.h"
#include "EngineControl.h"
#include "util.hpp"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage() = default;
        ~MainPage();

        void InitializeComponent();

        Windows::Foundation::Collections::IVector<MEraEmuWin::DevTools::SourcesFileTabItem> SourcesFileTabsItemList() { return m_sources_file_tab_items; }
        Windows::Foundation::Collections::IVector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> SourcesTabCallStackTabItemsList() { return m_sources_tab_call_stack_tab_items; }

        void SetConnectedEngineControl(MEraEmuWin::EngineControl engine);

        void SourceFilesTreeView_ItemInvoked(Windows::Foundation::IInspectable const& sender, Microsoft::UI::Xaml::Controls::TreeViewItemInvokedEventArgs const& e);
        void SourceFilesTabView_TabCloseRequested(Windows::Foundation::IInspectable const& sender, Microsoft::UI::Xaml::Controls::TabViewTabCloseRequestedEventArgs const& e);
        void SourcesTabLeftPaneOpenCloseButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void SourcesTabRightPaneOpenCloseButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecPauseResumeButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepIntoButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepOverButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepOutButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepSingleButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        fire_forget SourcesTabCallStackTabItem_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);

    private:
        fire_forget InitFromEngineControl();
        void OnEngineExecutionInterrupted(EraExecutionBreakReason reason);
        fire_forget UpdateForEnginePausedState(EraExecutionBreakReason reason);
        fire_forget UpdateForEngineRunningState(EraExecutionBreakReason reason);
        Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Controls::TabViewItem> OpenOrCreateSourcesFileTab(hstring path);
        MEraEmuWin::DevTools::CodeEditControl CodeEditorControlFromSourcesFileTabViewItem(Microsoft::UI::Xaml::Controls::TabViewItem const& tab);
        Windows::Foundation::IAsyncAction OpenOrCreateSourcesFileTabAtSrcSpan(hstring path, SrcSpan span);
        void InitializeCodeEditorControl(MEraEmuWin::DevTools::CodeEditControl const& editor_ctrl);
        void CodeEditorControl_MarginClick(WinUIEditor::Editor const& sender, WinUIEditor::MarginClickEventArgs const& e);

        winrt::MEraEmuWin::implementation::EngineControl* m_engine_ctrl{ nullptr };
        event_token m_et_EngineExecutionInterrupted{};
        Windows::Foundation::Collections::IVector<MEraEmuWin::DevTools::SourcesFileTabItem> m_sources_file_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesFileTabItem>()
        };
        Windows::Foundation::Collections::IVector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> m_sources_tab_call_stack_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>()
        };
        bool m_engine_running{};
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
