﻿#pragma once

#include "DevTools/MainPage.g.h"
#include "DevTools/QuickActionItem.h"
#include "DevTools/SourcesFileTabItem.h"
#include "DevTools/SourcesTabWatchTabItem.h"
#include "DevTools/SourcesTabBreakpointItem.h"
#include "DevTools/SourcesTabCallStackTabItem.h"
#include "EngineControl.h"
#include "util.hpp"

namespace winrt::MEraEmuWin::DevTools::implementation {
    struct MainPage : MainPageT<MainPage> {
        MainPage() = default;
        ~MainPage();

        static fire_forget final_release(std::unique_ptr<MainPage> self) noexcept;

        void InitializeComponent();

        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::QuickActionItem> FilteredQuickActionItemsList() { return m_filtered_quick_action_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesFileTabItem> SourcesFileTabsItemList() { return m_sources_file_tab_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabWatchTabItem> SourcesTabWatchTabItemsList() { return m_sources_tab_watch_tab_items; }
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabBreakpointItem> FilteredSourcesTabBreakpointItemsList() { return m_filtered_sources_tab_breakpoint_items; }
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
        fire_forget SourcesTabBreakpointsTabItem_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);
        fire_forget SourcesTabCallStackTabItem_Click(Windows::Foundation::IInspectable const& sender, Windows::UI::Xaml::RoutedEventArgs const& e);

    private:
        void CleanupEngineConnection();
        Windows::Foundation::IAsyncAction InitFromEngineControlAsync();
        void OnEngineExecutionInterrupted(EraExecutionBreakReason reason);
        fire_forget UpdateForEnginePausedState(EraExecutionBreakReason reason);
        fire_forget UpdateForEngineRunningState(EraExecutionBreakReason reason);
        Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Controls::TabViewItem> OpenOrCreateSourcesFileTabAsync(hstring path);
        Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Controls::TabViewItem> OpenOrCreateSourcesFuncAsmTabAsync(hstring path, hstring func_name);
        MEraEmuWin::DevTools::CodeEditControl CodeEditorControlFromSourcesFileTabViewItem(Microsoft::UI::Xaml::Controls::TabViewItem const& tab);
        Windows::Foundation::IAsyncAction OpenOrCreateSourcesFileTabAtSrcSpanAsync(hstring path, SrcSpan span);
        void InitializeCodeEditorControl(MEraEmuWin::DevTools::CodeEditControl const& editor_ctrl);
        void CodeEditorControl_MarginClick(WinUIEditor::Editor const& sender, WinUIEditor::MarginClickEventArgs const& e);
        fire_forget PositionSourcesTabWatchTabCurrentItemTextBox(uint32_t index);
        fire_forget CommitSourcesTabWatchTabCurrentItemTextBox();
        Windows::Foundation::IAsyncAction UpdateSourcesTabWatchTabItemFromExpressionStringAsync(MEraEmuWin::DevTools::SourcesTabWatchTabItem item, std::string_view expression);
        Windows::Foundation::IAsyncAction RefreshSourcesTabWatchTabItemValuesAsync();
        void CloseGlobalQuickActionsBaseFlyout();
        Windows::Foundation::IAsyncAction UpdateFilteredQuickActionItemsAsync();
        Windows::Foundation::IAsyncAction DumpBytecodeForFunctionAtCaretAsync();
        Microsoft::UI::Xaml::Controls::TabViewItem GetCurrentSourcesFileTabViewItem(hstring const& target_path = {});
        Windows::Foundation::IAsyncOperation<Windows::UI::Xaml::Controls::ContentDialogResult> ShowSimpleDialogAsync(hstring const& title, hstring const& content);
        void AddNewBreakpoint(hstring const& path, SrcSpan span, bool temporary);
        void AddNewTempBreakpointByIp(EraExecIp ip);
        void RemoveBreakpoint(hstring const& path, int32_t handle);
        void SyncBreakpointsWithSourceFile(hstring const& path);
        // Returns whether changes were made to the engine. If do_apply is false, undo the changes.
        // Note that temporary breakpoints will also be deleted if do_apply is false.
        Windows::Foundation::IAsyncOperation<bool> ApplyEngineBreakpointsAsync(bool do_apply);

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
        std::vector<MEraEmuWin::DevTools::SourcesTabBreakpointItem> m_sources_tab_breakpoint_items;
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabBreakpointItem> m_filtered_sources_tab_breakpoint_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesTabBreakpointItem>()
        };
        Windows::Foundation::Collections::IObservableVector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> m_sources_tab_call_stack_tab_items{
            single_threaded_observable_vector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>()
        };
        std::vector<MEraEmuWin::DevTools::QuickActionItem> m_quick_action_source_file_items;
        bool m_engine_running{};
        // TODO: Implement StepInto and StepOver
        // NOTE: StepOut is implemented by setting a temporary breakpoint at the caller's next instruction,
        //       so no need to implement it separately.
        /// Marks the current state of the DevTools operation (i.e. how the engine is being controlled).
        enum class DevToolsOperationState {
            /// No operation is being performed. No restrictions are imposed on the engine.
            None,
            /// Do not react to engine state changes, as the engine is being manipulated in a controlled way.
            Transparent,
            /// Engine is being single-stepped.
            SingleStepping,
            /// Engine is going to resume from a single-step halt. Used to apply breakpoints after stepping,
            /// to guarantee progress and prevent the engine from staying put because of a breakpoint.
            ResumingFromSingleStepHalt,
            /// Going to step into a line maybe containing function calls. Will repeatedly single-step until
            /// execution leaves the current line.
            SteppingInto,
            /// Going to step over a line maybe containing function calls. Similar to stepping into, but will
            /// not single-step into function calls, by placing temporary breakpoints at the next instruction.
            SteppingOver,
        } m_dev_tools_op_state{ DevToolsOperationState::None };
        SrcSpan m_step_info_safe_area_span{};
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct MainPage : MainPageT<MainPage, implementation::MainPage> {};
}
