#pragma once

#include "DevTools/MainPage.g.h"
#include "DevTools/QuickActionItem.h"
#include "DevTools/SourcesFileTabItem.h"
#include "DevTools/SourcesTabCallStackTabItem.h"
#include "EngineControl.h"
#include "util.hpp"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage() = default;
        ~MainPage();

        void InitializeComponent();

        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::QuickActionItem> FilteredQuickActionItemsList() { return m_filtered_quick_action_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesFileTabItem> SourcesFileTabsItemList() { return m_sources_file_tab_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabWatchTabItem> SourcesTabWatchTabItemsList() { return m_sources_tab_watch_tab_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> SourcesTabCallStackTabItemsList() { return m_sources_tab_call_stack_tab_items; }

        void SetConnectedEngineControl(MEraEmuWin::EngineControl engine);

        void GlobalQuickActionsBaseFlyout_InputTextBox_TextChanged(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Controls::TextChangedEventArgs const& e);
        void GlobalQuickActionsBaseFlyout_InputTextBox_KeyDown(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs const& e);
        void GlobalQuickActionsBaseFlyout_QuickActionsItemListView_ItemClick(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Controls::ItemClickEventArgs const& e);
        void GlobalQuickActionsBaseFlyout_QuickActionsItemListView_ContainerContentChanging(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Controls::ContainerContentChangingEventArgs const& e);

        void PageFlyoutContainerBackground_PointerPressed(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs const& e);
        void PageFlyoutContainer_KeyDown(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs const& e);

        void TopTabViewMoreMenuRunCommandItem_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);

        void SourceFilesTreeView_ItemInvoked(Windows::Foundation::IInspectable const& sender, Microsoft::UI::Xaml::Controls::TreeViewItemInvokedEventArgs const& e);
        void SourceFilesTabView_TabCloseRequested(Windows::Foundation::IInspectable const& sender, Microsoft::UI::Xaml::Controls::TabViewTabCloseRequestedEventArgs const& e);
        void SourcesTabLeftPaneOpenCloseButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void SourcesTabRightPaneOpenCloseButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecPauseResumeButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepIntoButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepOverButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepOutButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeExecStepSingleButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void CodeSourceWatchAddButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        fire_forget CodeSourceWatchRefreshButton_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        fire_forget SourcesTabWatchTabItemsListRepeaterContainer_Tapped(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::TappedRoutedEventArgs const& e);
        void SourcesTabWatchTabItemsListRepeaterContainer_DoubleTapped(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::DoubleTappedRoutedEventArgs const& e);
        void SourcesTabWatchTabCurrentItemTextBox_LostFocus(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        void SourcesTabWatchTabCurrentItemTextBox_KeyDown(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::Input::KeyRoutedEventArgs const& e);
        fire_forget SourcesTabCallStackTabItem_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);

    private:
        fire_forget InitFromEngineControl();
        void OnEngineExecutionInterrupted(EraExecutionBreakReason reason);
        fire_forget UpdateForEnginePausedState(EraExecutionBreakReason reason);
        fire_forget UpdateForEngineRunningState(EraExecutionBreakReason reason);
        Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Controls::TabViewItem> OpenOrCreateSourcesFileTab(hstring path);
        Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Controls::TabViewItem> OpenOrCreateSourcesFuncAsmTab(hstring path, hstring func_name);
        MEraEmuWin::DevTools::CodeEditControl CodeEditorControlFromSourcesFileTabViewItem(Microsoft::UI::Xaml::Controls::TabViewItem const& tab);
        Windows::Foundation::IAsyncAction OpenOrCreateSourcesFileTabAtSrcSpan(hstring path, SrcSpan span);
        void InitializeCodeEditorControl(MEraEmuWin::DevTools::CodeEditControl const& editor_ctrl);
        void CodeEditorControl_MarginClick(WinUIEditor::Editor const& sender, WinUIEditor::MarginClickEventArgs const& e);
        fire_forget PositionSourcesTabWatchTabCurrentItemTextBox(uint32_t index);
        fire_forget CommitSourcesTabWatchTabCurrentItemTextBox();
        Windows::Foundation::IAsyncAction UpdateSourcesTabWatchTabItemFromExpressionString(MEraEmuWin::DevTools::SourcesTabWatchTabItem item, std::string_view expression);
        Windows::Foundation::IAsyncAction RefreshSourcesTabWatchTabItemValues();
        void CloseGlobalQuickActionsBaseFlyout();
        Windows::Foundation::IAsyncAction UpdateFilteredQuickActionItems();
        Windows::Foundation::IAsyncAction DumpBytecodeForFunctionAtCaret();
        Microsoft::UI::Xaml::Controls::TabViewItem GetCurrentSourcesFileTabViewItem();
        Windows::Foundation::IAsyncOperation<Windows::UI::Xaml::Controls::ContentDialogResult> ShowSimpleDialog(hstring const& title, hstring const& content);

        winrt::MEraEmuWin::implementation::EngineControl* m_engine_ctrl{ nullptr };
        event_token m_et_EngineExecutionInterrupted{};
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::QuickActionItem> m_filtered_quick_action_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::QuickActionItem>()
        };
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesFileTabItem> m_sources_file_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesFileTabItem>()
        };
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabWatchTabItem> m_sources_tab_watch_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesTabWatchTabItem>()
        };
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> m_sources_tab_call_stack_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>()
        };
        std::vector<MEraEmuWin::DevTools::QuickActionItem> m_quick_action_source_file_items;
        bool m_engine_running{};
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
