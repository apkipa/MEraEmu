#include "pch.h"
#include "DevTools/MainPage.h"
#if __has_include("DevTools/MainPage.g.cpp")
#include "DevTools/MainPage.g.cpp"
#endif

using namespace winrt;
using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::UI;
using namespace Windows::UI::Text;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Controls::Primitives;
using namespace Windows::UI::Xaml::Documents;
using namespace Windows::UI::Xaml::Media;
//using namespace Microsoft::UI::Xaml::Controls;

namespace muxc = Microsoft::UI::Xaml::Controls;

using winrt::MEraEmuWin::implementation::EngineThreadTaskKind;

static constexpr int32_t BreakPointMargin = 1;
static constexpr int32_t LineNumberMargin = 2;
static constexpr int32_t BreakPointMarker = 11;
static constexpr int32_t YellowBackgroundMarker = 12;

static constexpr int32_t ARGB(int32_t a, int32_t r, int32_t g, int32_t b) {
    return (a << 24) | RGB(r, g, b);
}

static bool IsDescendantOfMenuFlyout(Windows::UI::Xaml::UIElement const& element) {
    auto parent = element.try_as<FrameworkElement>();
    while (parent) {
        if (auto parent_as_menu_flyout = parent.try_as<MenuFlyout>()) {
            return true;
        }
        parent = parent.Parent().try_as<FrameworkElement>();
    }
    return false;
}

static bool TryFocusToFirstFocusableElement(Windows::UI::Xaml::UIElement const& element) {
    if (!element) { return false; }
    /*auto class_name = get_class_name(element);
    OutputDebugStringW((L"TryFocusToFirstFocusableElement: " + class_name + L"\n").c_str());*/
    if (auto ctrl = element.try_as<Control>()) {
        if (ctrl.Focus(FocusState::Programmatic)) {
            return true;
        }
    }
    if (auto ctrl = element.try_as<ContentControl>()) {
        return TryFocusToFirstFocusableElement(ctrl.Content().try_as<UIElement>());
    }
    if (auto panel = element.try_as<Panel>()) {
        for (const auto& child : panel.Children()) {
            if (TryFocusToFirstFocusableElement(child)) {
                return true;
            }
        }
    }
    return false;
}

HRESULT BrowseToFile(LPCWSTR filename) {
    HRESULT hr = S_OK;
    LPITEMIDLIST pidl = nullptr;

    // Expand file path
    auto bufferLength = GetFullPathNameW(filename, 0, nullptr, nullptr);
    if (bufferLength == 0) {
        return HRESULT_FROM_WIN32(GetLastError());
    }
    wchar_t* fullPath = new (std::nothrow) wchar_t[bufferLength];
    if (!fullPath) {
        return E_OUTOFMEMORY;
    }
    auto newBufferLength = GetFullPathNameW(filename, bufferLength, fullPath, nullptr);
    if (newBufferLength >= bufferLength) {
        delete[] fullPath;
        return HRESULT_FROM_WIN32(GetLastError());
    }

    hr = SHParseDisplayName(fullPath, nullptr, &pidl, 0, nullptr);
    delete[] fullPath;
    if (SUCCEEDED(hr)) {
        hr = SHOpenFolderAndSelectItems(pidl, 0, nullptr, 0);
        ILFree(pidl);
    }
    return hr;
}

Windows::Foundation::IAsyncAction BrowseToFileAsync(hstring filename) {
    co_await winrt::resume_background();
    check_hresult(BrowseToFile(filename.c_str()));
}

SrcSpan PositionToLineSpan(WinUIEditor::Editor const& editor, int64_t position) {
    auto line = editor.LineFromPosition(position);
    auto line_start_pos = editor.PositionFromLine(line);
    return { (uint32_t)line_start_pos, (uint32_t)(editor.GetLineEndPosition(line) - line_start_pos) };
}

static void UpdateSourcesTabWatchTabItemFromValue(MEraEmuWin::DevTools::SourcesTabWatchTabItem item, EraEvaluateExprResult const& value) {
    auto escape_to_literal_sink = [](std::wstring& sink, std::wstring_view sv) {
        for (auto ch : sv) {
            if (ch == L'\\' || ch == L'"') {
                sink += L"\\";
            }
            sink += ch;
        }
    };

    auto sub_items = item.SubItems();
    sub_items.Clear();

    // Brief value
    if (value.value.is_int()) {
        item.BriefValue(to_hstring(*value.value.as_int()));
    }
    else if (value.value.is_str()) {
        item.BriefValue(to_hstring(*value.value.as_str()));
    }

    // Sub items
    for (auto& [key, val] : value.children) {
        auto sub_item = MEraEmuWin::DevTools::SourcesTabWatchTabItem();
        sub_item.Expression(to_hstring(key));
        if (val.is_int()) {
            sub_item.BriefValue(to_hstring(*val.as_int()));
        }
        else if (val.is_str()) {
            std::wstring result = L"\"";
            escape_to_literal_sink(result, to_hstring(*val.as_str()));
            result += L"\"";
            sub_item.BriefValue(result);
        }
        sub_items.Append(sub_item);
    }
}

namespace winrt::MEraEmuWin::DevTools::implementation {
    MainPage::~MainPage() {
    }
    fire_forget MainPage::final_release(std::unique_ptr<MainPage> self) noexcept {
        auto dq = DispatcherQueue::GetForCurrentThread();
        self->CleanupEngineConnection();
        // FIXME: Keep alive to prevent crash; should properly wait for all async tasks to finish instead.
        using namespace std::chrono_literals;
        co_await 1s;
        co_await resume_foreground(dq);
    }
    void MainPage::InitializeComponent() {
        MainPageT::InitializeComponent();

        VisualStateManager::GoToState(*this, L"EngineUnconnected", false);
    }
    void MainPage::SetConnectedEngineControl(MEraEmuWin::EngineControl engine) {
        // Close self first
        CleanupEngineConnection();

        if (!engine) { return; }

        m_engine_ctrl = engine.as<MEraEmuWin::implementation::EngineControl>().get();
        m_weak_engine_ctrl = engine;
        m_et_EngineExecutionInterrupted = m_engine_ctrl->EngineExecutionInterrupted({ this, &MainPage::OnEngineExecutionInterrupted });

        //InitFromEngineControlAsync();
        OnEngineExecutionInterrupted(m_engine_ctrl->GetLastBreakReason());
    }

    void MainPage::GlobalQuickActionsBaseFlyout_InputTextBox_TextChanged(IInspectable const& sender, TextChangedEventArgs const& e) {
        UpdateFilteredQuickActionItemsAsync();
    }
    void MainPage::GlobalQuickActionsBaseFlyout_InputTextBox_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        auto key = e.Key();
        if (key == VirtualKey::Enter) {
            // Execute command
            e.Handled(true);

            auto item = GlobalQuickActionsBaseFlyout_QuickActionsItemListView().SelectedItem()
                .try_as<MEraEmuWin::DevTools::QuickActionItem>();
            if (!item) { return; }

            CloseGlobalQuickActionsBaseFlyout();

            if (auto cmd = item.Command()) {
                if (cmd.CanExecute(*this)) {
                    cmd.Execute(*this);
                }
            }
        }
        else if (key == VirtualKey::Up || key == VirtualKey::Down) {
            // Move selection
            e.Handled(true);
            auto list_view = GlobalQuickActionsBaseFlyout_QuickActionsItemListView();
            auto items = list_view.Items();
            auto count = items.Size();
            if (count == 0) { return; }
            auto selected_index = list_view.SelectedIndex();
            if (selected_index == -1) {
                selected_index = 0;
            }
            else {
                selected_index += key == VirtualKey::Up ? -1 : 1;
                if (selected_index < 0) {
                    selected_index = count - 1;
                }
                else if (selected_index >= count) {
                    selected_index = 0;
                }
            }
            list_view.SelectedIndex(selected_index);
            list_view.ScrollIntoView(items.GetAt(selected_index));
        }
    }
    void MainPage::GlobalQuickActionsBaseFlyout_QuickActionsItemListView_ItemClick(IInspectable const& sender, ItemClickEventArgs const& e) {
        auto item = e.ClickedItem().try_as<MEraEmuWin::DevTools::QuickActionItem>();
        if (!item) { return; }

        CloseGlobalQuickActionsBaseFlyout();

        if (auto cmd = item.Command()) {
            if (cmd.CanExecute(*this)) {
                cmd.Execute(*this);
            }
        }
    }
    void MainPage::GlobalQuickActionsBaseFlyout_QuickActionsItemListView_ContainerContentChanging(IInspectable const& sender, ContainerContentChangingEventArgs const& e) {
        if (e.InRecycleQueue()) {
            return;
        }

        // Apply ranges as bold title parts
        auto root = e.ItemContainer().try_as<FrameworkElement>();
        auto item = e.Item().as<QuickActionItem>();
        auto item_title = item->Title();
        auto title_tb = root.FindName(L"QuickActionItemTitleTextBlock").try_as<TextBlock>();
        if (!title_tb) { return; }
        auto title_tb_inlines = title_tb.Inlines();
        title_tb_inlines.Clear();
        size_t last_end = 0;
        for (const auto& range : item->ranges()) {
            if (range.start > last_end) {
                auto run = Run();
                run.Text(hstring(std::wstring_view(item_title).substr(last_end, range.start - last_end)));
                title_tb_inlines.Append(run);
            }
            auto run = Run();
            run.Text(hstring(std::wstring_view(item_title).substr(range.start, range.len)));
            run.FontWeight(FontWeights::Bold());
            title_tb_inlines.Append(run);
            last_end = range.start + range.len;
        }
        if (last_end < item_title.size()) {
            auto run = Run();
            run.Text(hstring(std::wstring_view(item_title).substr(last_end)));
            title_tb_inlines.Append(run);
        }
    }

    void MainPage::PageFlyoutContainerBackground_PointerPressed(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        e.Handled(true);
        // Close flyout
        CloseGlobalQuickActionsBaseFlyout();
    }
    void MainPage::PageFlyoutContainer_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        if (e.Key() == VirtualKey::Escape) {
            // Close flyout
            CloseGlobalQuickActionsBaseFlyout();
        }
    }

    void MainPage::TopTabViewMoreMenuRunCommandItem_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        using namespace Windows::UI::Xaml::Core::Direct;

        /*GlobalQuickActionsBaseFlyout().ShowAt(LayoutRoot());
        return;*/

        // Workaround KeyboarAccelerator having too-global scope, by checking if main window is in foreground
        if (auto wnd = Tenkai::UI::Xaml::Window::GetCurrentMain()) {
            auto active_mode = wnd.View().ActivationMode();
            if (active_mode == Tenkai::UI::ViewManagement::WindowActivationMode::ActivatedInForeground) {
                return;
            }
        }

        auto flyout = GlobalQuickActionsBaseFlyout();
        auto flyout_content = flyout.Content().as<FrameworkElement>();
        auto is_open = flyout_content.IsLoaded();

        if (is_open) {
            // Close flyout instead
            CloseGlobalQuickActionsBaseFlyout();
            return;
        }

        auto presenter = FlyoutPresenter();
        presenter.Style(flyout.FlyoutPresenterStyle());
        presenter.Content(flyout_content);
        auto shadow = ThemeShadow();
        shadow.Receivers().Append(PageFlyoutContainerBackground());
        presenter.Shadow(shadow);
        presenter.Translation({ 0, 0, 32 });
        PageFlyoutContainer().Children().Append(presenter);
        PageFlyoutContainerBackground().Fill(SolidColorBrush(Colors::Transparent()));
        PageFlyoutContainer().UpdateLayout();
        /*if (!TryFocusToFirstFocusableElement(presenter)) {
            presenter.IsTabStop(true);
            presenter.Focus(FocusState::Programmatic);
            presenter.IsTabStop(false);
        }*/
        GlobalQuickActionsBaseFlyout_InputTextBox().Text(L">");
        GlobalQuickActionsBaseFlyout_InputTextBox().Select(INT32_MAX, 0);
        GlobalQuickActionsBaseFlyout_InputTextBox().Focus(FocusState::Programmatic);

        // Avoid items flickering because of TextChanged event delay
        UpdateFilteredQuickActionItemsAsync();
    }

    void MainPage::SourceFilesTreeView_ItemInvoked(IInspectable const& sender, muxc::TreeViewItemInvokedEventArgs const& e) {
        auto node = e.InvokedItem().as<muxc::TreeViewNode>();
        if (!node) { return; }
        // Ignore folders
        if (node.HasChildren()) { return; }

        std::string path_str;
        bool inverse_slash = true;
        while (node) {
            auto content = node.Content().as<IInspectable>();
            if (!content) { break; }
            auto filename = to_string(unbox_value<hstring>(content));
            if (inverse_slash && filename == "<VM>") {
                inverse_slash = false;
            }
            const auto sep = inverse_slash ? "\\" : "/";
            path_str = std::move(filename) + (path_str.empty() ? "" : sep) + std::move(path_str);
            node = node.Parent();
        }

        // Open existing file in editor or create new tab
        OpenOrCreateSourcesFileTabAsync(to_hstring(path_str));
    }
    void MainPage::SourceFilesTabView_TabCloseRequested(IInspectable const& sender, muxc::TabViewTabCloseRequestedEventArgs const& e) {
        auto tab = e.Item().as<MEraEmuWin::DevTools::SourcesFileTabItem>();
        if (!tab) { return; }
        size_t count = m_sources_file_tab_items.Size();
        for (size_t i = 0; i < count; i++) {
            if (m_sources_file_tab_items.GetAt(i) == tab) {
                m_sources_file_tab_items.RemoveAt(i);
                break;
            }
        }
    }
    void MainPage::SourcesTabLeftPaneOpenCloseButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        auto new_is_open = SourcesTabLeftPane().Visibility() == Visibility::Collapsed;
        VisualStateManager::GoToState(*this, new_is_open ? L"SourcesTabLeftPaneOpen" : L"SourcesTabLeftPaneClosed", true);
    }
    void MainPage::SourcesTabRightPaneOpenCloseButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        auto new_is_open = SourcesTabRightPane().Visibility() == Visibility::Collapsed;
        VisualStateManager::GoToState(*this, new_is_open ? L"SourcesTabRightPaneOpen" : L"SourcesTabRightPaneClosed", true);
    }
    void MainPage::CodeExecPauseResumeButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }

        if (m_engine_running) {
            // Pause execution
            m_dev_tools_op_state = DevToolsOperationState::None;
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SetHaltState);
        }
        else {
            // Resume execution
            ResumeEngineExecutionPossiblySteppingPastBreakpoint();
        }
    }
    void MainPage::CodeExecStepIntoButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }

        auto src_files_tab_view = SourceFilesTabView();
        auto item_index = src_files_tab_view.SelectedIndex();
        if (item_index < 0) { return; }
        auto tvi = src_files_tab_view.ContainerFromIndex(item_index).as<muxc::TabViewItem>();
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { return; }
        auto editor = editor_ctrl.Editor();
        if (m_sources_tab_call_stack_tab_items.Size() < 1) { return; }
        auto cb_item = m_sources_tab_call_stack_tab_items.GetAt(0);
        if (!cb_item) { return; }
        m_current_ip = EraExecIp{ cb_item.IpChunk(), cb_item.IpOffset() };
        m_step_info_safe_area_span = PositionToLineSpan(editor, editor.CurrentPos());
        m_step_info_safe_area_span.len++;
        m_dev_tools_op_state = DevToolsOperationState::SteppingInto_Initial;
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
    }
    void MainPage::CodeExecStepOverButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::CustomFuncAndClearHaltState);
    }
    void MainPage::CodeExecStepOutButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }

        if (m_sources_tab_call_stack_tab_items.Size() <= 1) {
            // No caller, so just continue
            ResumeEngineExecutionPossiblySteppingPastBreakpoint();
            return;
        }
        auto cb_item = m_sources_tab_call_stack_tab_items.GetAt(0);
        if (!cb_item) { return; }
        auto ret_ip = EraExecIp{ cb_item.RetIpChunk(), cb_item.RetIpOffset() };
        AddNewTempBreakpointByIp(ret_ip);
        ResumeEngineExecutionPossiblySteppingPastBreakpoint();
    }
    void MainPage::CodeExecStepSingleButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_dev_tools_op_state = DevToolsOperationState::SingleStepping;
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
    }
    void MainPage::CodeSourceWatchAddButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        auto index = m_sources_tab_watch_tab_items.Size();
        m_sources_tab_watch_tab_items.Append(MEraEmuWin::DevTools::SourcesTabWatchTabItem());
        PositionSourcesTabWatchTabCurrentItemTextBox(index);
    }
    fire_forget MainPage::CodeSourceWatchRefreshButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        co_await RefreshSourcesTabWatchTabItemValuesAsync();
    }
    fire_forget MainPage::SourcesTabWatchTabItemsListRepeaterContainer_Tapped(IInspectable const& sender, TappedRoutedEventArgs const& e) {
        auto item = e.OriginalSource().as<FrameworkElement>().DataContext().try_as<MEraEmuWin::DevTools::SourcesTabWatchTabItem>();
        if (!item) { co_return; }
        uint32_t index;
        if (!m_sources_tab_watch_tab_items.IndexOf(item, index)) { co_return; }

        // Switch expansion state
        item.IsExpanded(!item.IsExpanded());
    }
    void MainPage::SourcesTabWatchTabItemsListRepeaterContainer_DoubleTapped(IInspectable const& sender, DoubleTappedRoutedEventArgs const& e) {
        auto elem = e.OriginalSource().as<FrameworkElement>();
        auto repeater = SourcesTabWatchTabItemsListRepeater();
        while (auto parent = elem.Parent().try_as<FrameworkElement>()) {
            if (!parent || parent == repeater) {
                break;
            }
            elem = parent;
        }
        auto index = SourcesTabWatchTabItemsListRepeater().GetElementIndex(elem);
        if (index < 0) { return; }
        PositionSourcesTabWatchTabCurrentItemTextBox(index);
    }
    void MainPage::SourcesTabWatchTabCurrentItemTextBox_LostFocus(IInspectable const& sender, RoutedEventArgs const& e) {
        auto cur_focused = FocusManager::GetFocusedElement(XamlRoot()).as<DependencyObject>();
        auto parent = VisualTreeHelper::GetParent(cur_focused);
        if (cur_focused && parent && !IsDescendantOfMenuFlyout(cur_focused.try_as<UIElement>())) {
            CommitSourcesTabWatchTabCurrentItemTextBox();
        }
    }
    void MainPage::SourcesTabWatchTabCurrentItemTextBox_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        auto key = e.Key();
        if (key == VirtualKey::Enter) {
            CommitSourcesTabWatchTabCurrentItemTextBox();
        }
        else if (key == VirtualKey::Escape) {
            // Cancel editing
            auto tb = SourcesTabWatchTabCurrentItemTextBox();
            auto index = unbox_value<uint32_t>(tb.Tag());
            tb.Text(m_sources_tab_watch_tab_items.GetAt(index).Expression());
            CommitSourcesTabWatchTabCurrentItemTextBox();
        }
    }
    fire_forget MainPage::SourcesTabBreakpointsTabItem_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        auto item = sender.as<FrameworkElement>().DataContext().as<MEraEmuWin::DevTools::SourcesTabBreakpointItem>();
        co_await OpenOrCreateSourcesFileTabAtSrcSpanAsync(item.FileName(), { item.SrcSpanStart(), item.SrcSpanLen() });
    }
    fire_forget MainPage::SourcesTabCallStackTabItem_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        auto item = sender.as<FrameworkElement>().DataContext().as<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>();
        co_await OpenOrCreateSourcesFileTabAtSrcSpanAsync(item.FileName(), { item.SrcSpanStart(), item.SrcSpanLen() });
    }
    void MainPage::CleanupEngineConnection() {
        // WARN: Init & Uninit operations must not be async because of APIs exposed to EngineControl
        if (m_engine_ctrl) {
            m_engine_running = false;
            if (auto strong = m_weak_engine_ctrl.get()) {
                m_engine_ctrl->EngineExecutionInterrupted(m_et_EngineExecutionInterrupted);
                ApplyEngineBreakpointsAsync(false);
            }
            m_engine_ctrl = nullptr;
            InitFromEngineControlAsync();
        }
    }
    IAsyncAction MainPage::InitFromEngineControlAsync() {
        auto strong_this = get_strong();

        auto src_files_tree_view = SourceFilesTreeView();

        if (!m_engine_ctrl) {
            // Uninit instead
            src_files_tree_view.RootNodes().Clear();
            m_sources_file_tab_items.Clear();
            m_quick_action_source_file_items.clear();
            // Clear previously set breakpoints in engine
            m_sources_tab_breakpoint_items.clear();
            m_filtered_sources_tab_breakpoint_items.Clear();
            VisualStateManager::GoToState(*this, L"EngineUnconnected", false);
            co_return;
        }

        if (src_files_tree_view.RootNodes().Size() == 0) {
            auto source_files = co_await m_engine_ctrl->ExecEngineTask([](MEraEngine const& e) {
                return e.get_loaded_files_list();
            });

            // TODO: Properly fix timing issues
            if (src_files_tree_view.RootNodes().Size() > 0) {
                co_return;
            }

            std::unordered_map<std::string_view, muxc::TreeViewNode> nodes_map;
            for (std::string_view path : source_files) {
                auto add_or_make_node = [&](auto self, std::string_view path) {
                    auto path_delim_pos = path.find_last_of("/\\");
                    bool has_parent = path_delim_pos != std::string_view::npos;
                    auto parent = has_parent ? path.substr(0, path_delim_pos) : "";
                    auto filename = has_parent ? path.substr(path_delim_pos + 1) : path;

                    if (parent.empty()) {
                        // Fake parent node
                        auto& parent_node = src_files_tree_view;
                        auto node = muxc::TreeViewNode();
                        node.Content(box_value(to_hstring(filename)));
                        parent_node.RootNodes().Append(node);
                        return nodes_map.emplace(filename, node).first;
                    }
                    else {
                        auto parent_node_iter = nodes_map.find(parent);
                        if (parent_node_iter == nodes_map.end()) {
                            parent_node_iter = self(self, parent);
                        }
                        auto& parent_node = parent_node_iter->second;
                        auto node = muxc::TreeViewNode();
                        node.Content(box_value(to_hstring(filename)));
                        parent_node.Children().Append(node);
                        return nodes_map.emplace(path, node).first;
                    }
                };

                add_or_make_node(add_or_make_node, path);
            }
        }

        // Update UI
        VisualStateManager::GoToState(*this, L"EngineConnected", false);
    }
    void MainPage::OnEngineExecutionInterrupted(EraExecutionBreakReason reason) {
        InitFromEngineControlAsync();

        if (reason == ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS || is_execution_break_reason_fatal(reason)) {
            m_engine_running = false;
            if (m_dev_tools_op_state != DevToolsOperationState::Transparent) {
                VisualStateManager::GoToState(*this, L"EnginePaused", false);
                UpdateForEnginePausedState(reason);
            }
        }
        else {
            m_engine_running = true;
            if (m_dev_tools_op_state != DevToolsOperationState::Transparent) {
                VisualStateManager::GoToState(*this, L"EngineRunning", false);
                UpdateForEngineRunningState(reason);
            }
        }
    }
    fire_forget MainPage::UpdateForEnginePausedState(EraExecutionBreakReason reason) {
        auto strong_this = get_strong();

        if (m_dev_tools_op_state == DevToolsOperationState::SingleStepping) {
            m_dev_tools_op_state = DevToolsOperationState::None;
        }
        else if (m_dev_tools_op_state == DevToolsOperationState::ResumingFromSingleStepHalt) {
            m_dev_tools_op_state = DevToolsOperationState::None;
            if (reason == ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS) {
                co_await ApplyEngineBreakpointsAsync(true);
                m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::ClearHaltState);
                co_return;
            }
        }
        else if (m_dev_tools_op_state == DevToolsOperationState::SteppingInto_Initial || m_dev_tools_op_state == DevToolsOperationState::SteppingInto_Body) {
            auto se_op_state = tenkai::cpp_utils::scope_exit([&] {
                m_dev_tools_op_state = DevToolsOperationState::None;
            });
            if (reason == ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS) {
                m_dev_tools_op_state = DevToolsOperationState::Transparent;
                co_await ApplyEngineBreakpointsAsync(true);
                auto should_continue = co_await m_engine_ctrl->ExecEngineTask([prev_ip = m_current_ip, safe_span = m_step_info_safe_area_span](MEraEngine const& e) {
                    auto ip = e.get_current_ip().value();
                    if (ip.chunk != prev_ip.chunk) {
                        // Control flow moved to another file
                        return false;
                    }
                    auto src_info = e.get_src_info_from_ip(ip).value();
                    return span_contains(safe_span, src_info.span.start);
                });
                if (should_continue) {
                    // Stepping into the same line, so just continue
                    se_op_state.release();
                    m_dev_tools_op_state = DevToolsOperationState::SteppingInto_Body;
                    m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
                    co_return;
                }
            }
        }

        // Un-apply breakpoints
        co_await ApplyEngineBreakpointsAsync(false);

        // Load call stack items
        std::vector<MEraEmuWin::DevTools::SourcesTabCallStackTabItem> call_stack_items;
        co_await m_engine_ctrl->ExecEngineTask([&](MEraEngine const& e) {
            auto stack_trace = e.get_stack_trace();
            for (const auto& frame : stack_trace.frames | std::views::reverse) {
                auto ip = frame.ip;
                auto func_info = e.get_func_info_by_ip(ip).value();
                auto src_info = e.get_src_info_from_ip(ip).value();
                auto path = to_hstring(src_info.filename);
                auto resolved = e.resolve_src_span(src_info.filename, src_info.span).value();
                auto item = MEraEmuWin::DevTools::SourcesTabCallStackTabItem();
                item.FuncName(to_hstring(func_info.name));
                item.FileName(path);
                item.LineNumber(resolved.loc.line);
                item.ColumnNumber(resolved.loc.col + 1);
                item.SrcSpanStart(src_info.span.start);
                item.SrcSpanLen(src_info.span.len);
                item.IpChunk(ip.chunk);
                item.IpOffset(ip.offset);
                item.RetIpChunk(frame.ret_ip.chunk);
                item.RetIpOffset(frame.ret_ip.offset);
                if (call_stack_items.empty()) {
                    item.IsCurrent(true);
                }
                call_stack_items.push_back(std::move(item));
            }
        });
        m_sources_tab_call_stack_tab_items.ReplaceAll(call_stack_items);

        // Open current break location in editor (the first item in call stack)
        if (!call_stack_items.empty()) {
            auto& item = call_stack_items.front();
            co_await OpenOrCreateSourcesFileTabAtSrcSpanAsync(item.FileName(), { item.SrcSpanStart(), item.SrcSpanLen() });
        }

        // Reload watch items
        co_await RefreshSourcesTabWatchTabItemValuesAsync();
    }
    fire_forget MainPage::UpdateForEngineRunningState(EraExecutionBreakReason reason) {
        auto strong_this = get_strong();

        m_sources_tab_call_stack_tab_items.Clear();

        // Don't apply breakpoints if we're stepping through the first instruction
        if (m_dev_tools_op_state != DevToolsOperationState::ResumingFromSingleStepHalt &&
            m_dev_tools_op_state != DevToolsOperationState::SingleStepping &&
            m_dev_tools_op_state != DevToolsOperationState::SteppingInto_Initial)
        {
            co_await ApplyEngineBreakpointsAsync(true);
        }
    }
    // TODO: Unify tabs creation logic
    IAsyncOperation<muxc::TabViewItem> MainPage::OpenOrCreateSourcesFileTabAsync(hstring path) {
        auto strong_this = get_strong();

        std::wstring_view path_sv = path;
        auto source_tabview = SourceFilesTabView();

        for (size_t i = 0; auto && tab : m_sources_file_tab_items) {
            if (tab.DocType() != L"erb") { i++; continue; }
            if (tab.FullPath() == path) {
                source_tabview.SelectedIndex(i);
                co_return source_tabview.ContainerFromIndex(i).as<muxc::TabViewItem>();
            }
            i++;
        }

        auto tab = make<SourcesFileTabItem>();
        tab.FileName(path_sv.substr(path_sv.find_last_of(L"/\\") + 1));
        tab.FullPath(path);
        tab.DocType(L"erb");
        auto tab_index = m_sources_file_tab_items.Size();
        m_sources_file_tab_items.Append(tab);
        // TODO: Fix add tab missing animation
        source_tabview.SelectedIndex(tab_index);

        // Fetch file source
        auto source = (co_await m_engine_ctrl->ExecEngineTask([path = to_string(path)](MEraEngine const& e) {
            return e.get_file_source(path);
        })).value_or("");
        // HACK: Wait for TabViewItem to be loaded
        co_await util::winrt::resume_when_loaded(source_tabview);
        muxc::TabViewItem tvi = nullptr;
        MEraEmuWin::DevTools::CodeEditControl editor_ctrl = nullptr;
        while (source_tabview.IsLoaded()) {
            co_await resume_foreground(DispatcherQueue::GetForCurrentThread(), DispatcherQueuePriority::Low);
            if (tab_index >= source_tabview.TabItems().Size()) {
                throw hresult_changed_state();
            }
            tvi = source_tabview.ContainerFromIndex(tab_index).as<muxc::TabViewItem>();
            if (!tvi) { continue; }
            if (tvi.DataContext() != tab) {
                throw hresult_changed_state();
            }
            editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
            if (!editor_ctrl) { continue; }
            break;
        }
        auto editor = editor_ctrl.Editor();

        // TODO: If we don't wait, the editor will not be ready and invokes nullptr?
        //co_await resume_foreground(DispatcherQueue::GetForCurrentThread(), DispatcherQueuePriority::Low);
        co_await util::winrt::resume_when_loaded(editor_ctrl);

        editor.ClearAll();
        if (!tvi.Tag()) {
            // Manually prevent reinitialization thanks to TabViewItem being reused :(
            tvi.Tag(box_value(L"inited"));
            InitializeCodeEditorControl(editor_ctrl);
        }
        editor.AppendTextFromBuffer((int64_t)source.size(), util::winrt::make_buffer_wrapper(source));
        editor.EmptyUndoBuffer();

        // Load user-defined breakpoints, if any
        for (auto& bp : m_sources_tab_breakpoint_items) {
            if (bp.IsImplicit()) { continue; }
            if (bp.FileName() == path) {
                auto line = bp.LineNumber() - 1;
                auto handle = editor.MarkerAdd(line, BreakPointMarker);
                bp.MarkerHandle(handle);
            }
        }

        co_return tvi;
    }
    IAsyncOperation<muxc::TabViewItem> MainPage::OpenOrCreateSourcesFuncAsmTabAsync(hstring path, hstring func_name) {
        auto strong_this = get_strong();
        auto source_tabview = SourceFilesTabView();
        auto target_tab_name = L"Disassembly of @" + func_name;
        for (size_t i = 0; auto && tab : m_sources_file_tab_items) {
            if (tab.DocType() != L"func-asm") { i++; continue; }
            if (tab.FullPath() == path && tab.FileName() == target_tab_name) {
                source_tabview.SelectedIndex(i);
                co_return source_tabview.ContainerFromIndex(i).as<muxc::TabViewItem>();
            }
            i++;
        }

        auto tab = make<SourcesFileTabItem>();
        tab.FileName(target_tab_name);
        tab.FullPath(path);
        tab.DocType(L"func-asm");
        auto tab_index = m_sources_file_tab_items.Size();
        m_sources_file_tab_items.Append(tab);
        source_tabview.SelectedIndex(tab_index);

        // Load source
        bool need_disable_breakpoints = m_engine_running;
        auto op_state = std::exchange(m_dev_tools_op_state, DevToolsOperationState::Transparent);
        auto se_restore_state = tenkai::cpp_utils::scope_exit([&] {
            m_dev_tools_op_state = op_state;
            if (need_disable_breakpoints) {
                if (op_state == DevToolsOperationState::None) {
                    m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::ClearHaltState);
                }
                else {
                    m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
                }
            }
        });
        if (need_disable_breakpoints) {
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SetHaltState);
            co_await ApplyEngineBreakpointsAsync(false);
        }
        auto bytecodes = co_await m_engine_ctrl->ExecEngineTask([&](MEraEngine const& e) {
            return e.dump_function_bytecode(to_string(func_name));
        });
        {
            auto to_drop = std::move(se_restore_state);
        }

        // HACK: Wait for TabViewItem to be loaded
        co_await util::winrt::resume_when_loaded(source_tabview);
        source_tabview.UpdateLayout();
        auto tvi = source_tabview.ContainerFromIndex(tab_index).as<muxc::TabViewItem>();
        if (!tvi) { co_return tvi; }
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { co_return tvi; }
        auto editor = editor_ctrl.Editor();

        co_await util::winrt::resume_when_loaded(editor_ctrl);

        editor.ClearAll();
        if (!tvi.Tag()) {
            // Manually prevent reinitialization thanks to TabViewItem being reused :(
            tvi.Tag(box_value(L"inited"));
            InitializeCodeEditorControl(editor_ctrl);
        }
        for (const auto& bc : bytecodes) {
            std::string_view bc_str = bc.opcode_str;
            auto append_str = [&](std::string_view str) {
                editor.AppendTextFromBuffer((int64_t)str.size(), util::winrt::make_buffer_wrapper(str));
            };
            /*editor.AppendTextFromBuffer((int64_t)bc_str.size(), util::winrt::make_buffer_wrapper(bc_str));
            editor.AppendTextFromBuffer(1, util::winrt::make_buffer_wrapper(std::string_view{ "\n" }));*/
            auto num_str = std::to_string(bc.offset);
            while (num_str.size() < 8) {
                num_str = " " + num_str;
            }
            append_str(num_str);
            append_str("    ");
            append_str(bc_str);
            append_str("\n");
        }
        editor.EmptyUndoBuffer();

        co_return tvi;
    }
    MEraEmuWin::DevTools::CodeEditControl MainPage::CodeEditorControlFromSourcesFileTabViewItem(muxc::TabViewItem const& tab) {
        if (!tab) { return nullptr; }
        auto content = tab.Content().try_as<FrameworkElement>();
        if (!content) { return nullptr; }
        if (auto ctrl = content.try_as<MEraEmuWin::DevTools::CodeEditControl>()) { return ctrl; }
        auto ctrl = content.FindName(L"SrcCodeEditor");
        return ctrl.try_as<MEraEmuWin::DevTools::CodeEditControl>();
    }
    IAsyncAction MainPage::OpenOrCreateSourcesFileTabAtSrcSpanAsync(hstring path, SrcSpan span) {
        auto strong_this = get_strong();
        auto tvi = co_await OpenOrCreateSourcesFileTabAsync(path);
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { co_return; }
        // Move caret to the position
        auto editor = editor_ctrl.Editor();
        {
            // Center the line
            using WinUIEditor::CaretPolicy;
            auto old_policy = CaretPolicy::Slop | CaretPolicy::Strict | CaretPolicy::Even;
            editor.SetYCaretPolicy(CaretPolicy::Strict | CaretPolicy::Even, 0);
            //editor.GotoPos(span.start);
            editor.SetSel(span.start + span.len, span.start);
            editor.SetYCaretPolicy(old_policy, 1);
        }
        // Highlight the line
        auto line_to_highlight = editor.LineFromPosition(span.start);
        editor.MarkerDeleteAll(YellowBackgroundMarker);
        editor.MarkerAdd(line_to_highlight, YellowBackgroundMarker);
        // Focus the editor
        editor_ctrl.Focus(FocusState::Programmatic);
    }
    IAsyncAction MainPage::OpenOrCreateSourcesFuncAsmTabAtIpAsync(hstring path, hstring func_name, std::optional<uint32_t> ip_offset) {
        auto strong_this = get_strong();
        auto tvi = co_await OpenOrCreateSourcesFuncAsmTabAsync(path, func_name);
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { co_return; }
        auto editor = editor_ctrl.Editor();

        editor_ctrl.Focus(FocusState::Programmatic);
        if (!ip_offset) { co_return; }
        // Search through the editor for the IP offset (offset is at the beginning of the line)
        for (int64_t cur_line_num = 0;; cur_line_num++) {
            auto line = editor.GetLine(cur_line_num);
            if (line.empty()) { break; }
            std::wstring_view line_view = line;
            while (!line_view.empty() && std::isspace(line_view.front())) {
                line_view.remove_prefix(1);
            }
            if (line_view.empty()) { continue; }
            std::string digit_buf;
            size_t digit_len = 0;
            while (digit_len < line_view.size() && std::iswdigit(line_view[digit_len])) {
                digit_buf.push_back((char)line_view[digit_len]);
                digit_len++;
            }
            auto line_offset = std::stoul(digit_buf);
            if (line_offset == *ip_offset) {
                // Found the line, move caret to the line and highlight it
                editor.MarkerDeleteAll(YellowBackgroundMarker);
                editor.MarkerAdd(cur_line_num, YellowBackgroundMarker);
                editor.GotoLine(cur_line_num);
                break;
            }
        }
    }
    void MainPage::InitializeCodeEditorControl(MEraEmuWin::DevTools::CodeEditControl const& editor_ctrl) {
        if (!editor_ctrl) { return; }

        using WinUIEditor::MarginType;
        using WinUIEditor::MarkerSymbol;
        using WinUIEditor::CursorShape;
        using WinUIEditor::CaretPolicy;

        // Register event handlers
        auto editor = editor_ctrl.Editor();
        editor.SetMarginTypeN(0, MarginType::Symbol);
        editor.SetMarginTypeN(BreakPointMargin, MarginType::Symbol);
        editor.SetMarginSensitiveN(BreakPointMargin, true);
        editor.SetMarginMaskN(BreakPointMargin, (1ul << BreakPointMarker));
        editor.SetMarginCursorN(BreakPointMargin, CursorShape::ReverseArrow);
        editor.SetMarginTypeN(LineNumberMargin, MarginType::Number);
        editor.SetMarginSensitiveN(LineNumberMargin, false);
        editor.SetMarginMaskN(LineNumberMargin, 0);
        editor.MarkerDefine(BreakPointMarker, MarkerSymbol::Circle);
        editor.MarkerSetBack(BreakPointMarker, RGB(0xff, 0, 0));
        editor.MarkerSetFore(BreakPointMarker, RGB(0xdc, 0x14, 0x3c));
        editor.MarkerDefine(YellowBackgroundMarker, MarkerSymbol::Background);
        editor.MarkerSetBack(YellowBackgroundMarker, ARGB(0x80, 0xff, 0xd7, 0));
        editor.MarkerSetFore(YellowBackgroundMarker, ARGB(0x80, 0xff, 0xd7, 0));
        editor.MarginClick({ this, &MainPage::CodeEditorControl_MarginClick });
        //editor.SetYCaretPolicy(CaretPolicy::Slop | CaretPolicy::Strict | CaretPolicy::Jumps, 0);
        //editor.SetYCaretPolicy(CaretPolicy::Strict | CaretPolicy::Even, 0);
    }
    void MainPage::CodeEditorControl_MarginClick(WinUIEditor::Editor const& sender, WinUIEditor::MarginClickEventArgs const& e) {
        if (e.Margin() != BreakPointMargin) { return; }

        // Assuming the editor is in the selected tab
        auto src_files_tab_view = SourceFilesTabView();
        auto item = src_files_tab_view.SelectedItem().try_as<MEraEmuWin::DevTools::SourcesFileTabItem>();
        if (!item) { return; }

        auto path = item.FullPath();

        auto cur_pos = e.Position();
        auto line = sender.LineFromPosition(cur_pos);
        if (sender.MarkerGet(line) & (1 << BreakPointMarker)) {
            // Remove breakpoint marker by enumerating all markers on the line
            int which{};
            while (true) {
                auto handle = sender.MarkerHandleFromLine(line, which++);
                if (handle == -1) { break; }
                //sender.MarkerDelete(line, BreakPointMarker);
                RemoveBreakpoint(path, handle);
            }
        }
        else {
            //auto handle = sender.MarkerAdd(line, BreakPointMarker);
            auto span = PositionToLineSpan(sender, cur_pos);
            AddNewBreakpoint(path, span, false);
        }
    }
    fire_forget MainPage::PositionSourcesTabWatchTabCurrentItemTextBox(uint32_t index) {
        auto strong_this = get_strong();

        auto item = m_sources_tab_watch_tab_items.GetAt(index);
        if (!item) { co_return; }
        // Don't expand the item
        item.IsExpanded(false);
        auto repeater = SourcesTabWatchTabItemsListRepeater();
        auto container = repeater.GetOrCreateElement(index).as<FrameworkElement>();
        if (!container) { co_return; }

        repeater.UpdateLayout();

        auto tb = SourcesTabWatchTabCurrentItemTextBox();
        tb.Tag(box_value(index));
        tb.Text(item.Expression());
        tb.ClearUndoRedoHistory();
        tb.SelectAll();
        Point pt = { 0, 0 };
        pt = container.TransformToVisual(repeater).TransformPoint(pt);
        tb.Margin({ pt.X, pt.Y, 0, 0 });
        tb.Visibility(Visibility::Visible);
        tb.Focus(FocusState::Programmatic);
        container.StartBringIntoView();
    }
    fire_forget MainPage::CommitSourcesTabWatchTabCurrentItemTextBox() {
        auto strong_this = get_strong();

        auto tb = SourcesTabWatchTabCurrentItemTextBox();
        auto tag = tb.Tag();
        if (!tag) { co_return; }
        auto index = unbox_value<uint32_t>(tag);
        auto item = m_sources_tab_watch_tab_items.GetAt(index);
        if (!item) { co_return; }
        auto expression = tb.Text();
        item.Expression(expression);

        if (expression.empty()) {
            m_sources_tab_watch_tab_items.RemoveAt(index);
        }
        else {
            co_await UpdateSourcesTabWatchTabItemFromExpressionStringAsync(item, to_string(expression));
            m_sources_tab_watch_tab_items.SetAt(index, item);
        }

        // Hide the TextBox
        tb.Tag(nullptr);
        // Don't steal focus if there's a flyout
        if (PageFlyoutContainer().Children().Size() == 0) {
            CodeSourceWatchTabExpander().Focus(FocusState::Pointer);
        }
        tb.Visibility(Visibility::Collapsed);
    }
    IAsyncAction MainPage::UpdateSourcesTabWatchTabItemFromExpressionStringAsync(MEraEmuWin::DevTools::SourcesTabWatchTabItem item, std::string_view expression) {
        auto strong_this = get_strong();

        item.BriefValue({});
        item.SubItems().Clear();
        item.ErrorInfo({});

        try {
            auto watch_value = co_await m_engine_ctrl->ExecEngineTask([expression](MEraEngine const& e) {
                return e.evaluate_expr(expression, std::nullopt, 0, 16, 100'000);
            });
            UpdateSourcesTabWatchTabItemFromValue(item, watch_value);
        }
        catch (MEraEngineException const& e) {
            item.ErrorInfo(to_hstring(e.what()));
        }
    }
    IAsyncAction MainPage::RefreshSourcesTabWatchTabItemValuesAsync() {
        auto strong_this = get_strong();

        for (size_t i = 0; auto item : m_sources_tab_watch_tab_items) {
            auto expression = to_string(item.Expression());
            if (expression.empty()) { continue; }
            co_await UpdateSourcesTabWatchTabItemFromExpressionStringAsync(item, expression);
            m_sources_tab_watch_tab_items.SetAt(i, item);
            i++;
        }
    }
    void MainPage::CloseGlobalQuickActionsBaseFlyout() {
        GlobalQuickActionsBaseFlyout_InputTextBox().Text({});
        PageFlyoutContainerBackground().Fill(nullptr);
        PageFlyoutContainer().Children().Clear();
        // Try to focus back to the editor
        if (auto idx = SourceFilesTabView().SelectedIndex(); idx >= 0) {
            auto tvi = SourceFilesTabView().ContainerFromIndex(idx).try_as<muxc::TabViewItem>();
            if (tvi) {
                auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
                if (editor_ctrl) {
                    editor_ctrl.Focus(FocusState::Programmatic);
                    return;
                }
            }
        }
        // Fallback to the first focusable element
        if (auto elem = FocusManager::FindFirstFocusableElement(LayoutRoot())) {
            elem.as<Control>().Focus(FocusState::Pointer);
        }
    }
    IAsyncAction MainPage::UpdateFilteredQuickActionItemsAsync() {
        if (PageFlyoutContainer().Children().Size() == 0) { co_return; }

        auto strong_this = get_strong();

        auto quick_actions_item_list_view = GlobalQuickActionsBaseFlyout_QuickActionsItemListView();

        m_filtered_quick_action_items.Clear();

        auto input_str = GlobalQuickActionsBaseFlyout_InputTextBox().Text();
        std::wstring_view input = input_str;
        if (input.starts_with(L">")) {
            // Mode: Run command
            input.remove_prefix(1);

            static auto g_all_items = []() {
                std::vector<MEraEmuWin::DevTools::QuickActionItem> items;
                auto push_item = [&](hstring const& name, ICommand cmd) {
                    MEraEmuWin::DevTools::QuickActionItem item;
                    item.Title(name);
                    item.Command(cmd);
                    items.push_back(item);
                };
                push_item(L"Dump bytecode for the function at caret", RelayCommand([](auto&& sender) {
                    sender.as<MainPage>()->DumpBytecodeForFunctionAtCaretAsync();
                }));
                push_item(L"Dump current stack", RelayCommand([](auto&& sender) {
                    sender.as<MainPage>()->DumpCurrentStackAsync();
                }));
                push_item(L"Open current file in File Explorer", RelayCommand([](auto&& sender) {
                    auto that = sender.as<MainPage>();
                    auto tvi = that->GetCurrentSourcesFileTabViewItem();
                    if (!tvi) { return; }
                    auto item = tvi.DataContext().try_as<MEraEmuWin::DevTools::SourcesFileTabItem>();
                    if (!item) { return; }
                    auto path = item.FullPath();

                    [](auto that, hstring path) -> fire_forget {
                        try {
                            co_await BrowseToFileAsync(path);
                        }
                        catch (hresult_error const& e) {
                            that->ShowSimpleDialogAsync(L"Failed to open file",
                                winrt::format(L"0x{:08x}\n{}", (uint32_t)e.code(), e.message()));
                        }
                    }(that, path);
                }));
                return items;
            }();

            std::vector<MEraEmuWin::DevTools::QuickActionItem> filtered_items;
            for (const auto& item : g_all_items) {
                if (input.empty()) {
                    item.as<QuickActionItem>()->ranges().clear();
                    item.Score(0);
                    filtered_items.push_back(item);
                    continue;
                }

                auto result_opt = rust_FuzzyStringMatch::best_match(to_string(input.data()).c_str(), to_string(item.Title()).c_str());
                if (!result_opt) { continue; }
                auto& result = *result_opt;
                if (result.score() <= 0) { continue; }
                item.Score((int32_t)result.score());
                item.as<QuickActionItem>()->ranges() = result.matches() | std::ranges::to<std::vector>();
                filtered_items.push_back(item);
            }
            // Highest score first, then by title
            std::ranges::sort(filtered_items, [](const auto& a, const auto& b) {
                return std::pair(a.Score(), b.Title()) > std::pair(b.Score(), a.Title());
            });
            m_filtered_quick_action_items.ReplaceAll(filtered_items);
        }
        else {
            // Mode: Open workspace files
            if (m_quick_action_source_file_items.empty()) {
                // Fetch source files list
                auto source_files = co_await m_engine_ctrl->ExecEngineTask([](MEraEngine const& e) {
                    return e.get_loaded_files_list();
                });
                for (const auto& srcfile : source_files) {
                    auto item = MEraEmuWin::DevTools::QuickActionItem();
                    auto path = to_hstring(srcfile);
                    item.Title(path);
                    item.Command(RelayCommand([path](auto&& sender) -> fire_forget {
                        auto strong_this = sender.as<MainPage>();
                        auto tvi = co_await strong_this->OpenOrCreateSourcesFileTabAsync(path);
                        if (tvi) {
                            auto editor_ctrl = strong_this->CodeEditorControlFromSourcesFileTabViewItem(tvi);
                            if (editor_ctrl) {
                                editor_ctrl.Focus(FocusState::Programmatic);
                            }
                        }
                    }));
                    m_quick_action_source_file_items.push_back(item);
                }
            }

            std::vector<MEraEmuWin::DevTools::QuickActionItem> filtered_items;
            for (const auto& item : m_quick_action_source_file_items) {
                if (input.empty()) {
                    item.as<QuickActionItem>()->ranges().clear();
                    item.Score(0);
                    filtered_items.push_back(item);
                    continue;
                }

                auto result_opt = rust_FuzzyStringMatch::best_match(to_string(input.data()).c_str(), to_string(item.Title()).c_str());
                if (!result_opt) { continue; }
                auto& result = *result_opt;
                if (result.score() <= 0) { continue; }
                item.Score((int32_t)result.score());
                item.as<QuickActionItem>()->ranges() = result.matches() | std::ranges::to<std::vector>();
                filtered_items.push_back(item);
            }
            // Highest score first, then by title
            std::ranges::sort(filtered_items, [](const auto& a, const auto& b) {
                return std::pair(a.Score(), b.Title()) > std::pair(b.Score(), a.Title());
            });
            m_filtered_quick_action_items.ReplaceAll(filtered_items);
        }

        // Select the first item when re-filtered
        if (m_filtered_quick_action_items.Size() > 0) {
            quick_actions_item_list_view.SelectedIndex(0);
        }
    }
    IAsyncAction MainPage::DumpBytecodeForFunctionAtCaretAsync() {
        auto strong_this = get_strong();

        auto src_tabview = SourceFilesTabView();
        auto src_tvi_idx = src_tabview.SelectedIndex();
        if (src_tvi_idx < 0) { co_return; }
        auto src_tvi = src_tabview.ContainerFromIndex(src_tvi_idx).try_as<muxc::TabViewItem>();
        if (!src_tvi) { co_return; }
        auto src_item = m_sources_file_tab_items.GetAt(src_tvi_idx);
        if (!src_item || src_item.DocType() != L"erb") { co_return; }

        // Disassemble the function at caret
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(src_tvi);
        if (!editor_ctrl) { co_return; }
        auto editor = editor_ctrl.Editor();
        auto caret_pos = editor.CurrentPos();
        auto span = PositionToLineSpan(editor, caret_pos);
        EraExecIp ip;
        auto func_name = co_await m_engine_ctrl->ExecEngineTask([&](MEraEngine const& e) {
            ip = e.get_ip_from_src(to_string(src_item.FullPath()), span).value();
            auto func_name = e.get_func_info_by_ip(ip).value().name;
            return func_name;
        });

        // Create a new tab to place the disassembled bytecode
        co_await OpenOrCreateSourcesFuncAsmTabAtIpAsync(src_item.FullPath(), to_hstring(func_name), ip.offset);
    }
    IAsyncAction MainPage::DumpCurrentStackAsync() {
        auto strong_this = get_strong();
        auto stack_trace = co_await m_engine_ctrl->ExecEngineTask([](MEraEngine const& e) {
            return e.dump_stack();
        });
        auto stack_trace_str = stack_trace.dump(4);
        // Copy to clipboard
        Windows::ApplicationModel::DataTransfer::DataPackage data_package;
        data_package.RequestedOperation(Windows::ApplicationModel::DataTransfer::DataPackageOperation::Copy);
        data_package.SetText(to_hstring(stack_trace_str));
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(data_package);
        ShowSimpleDialogAsync(L"Dump current stack", L"Stack has been copied to clipboard.");
    }
    muxc::TabViewItem MainPage::GetCurrentSourcesFileTabViewItem(hstring const& target_path) {
        auto src_tabview = SourceFilesTabView();
        if (target_path.empty()) {
            // Get the selected tab
            auto src_tvi_idx = src_tabview.SelectedIndex();
            if (src_tvi_idx < 0) { return nullptr; }
            return src_tabview.ContainerFromIndex(src_tvi_idx).as<muxc::TabViewItem>();
        }

        // Get the tab with the target path
        for (size_t i = 0; auto && tab : m_sources_file_tab_items) {
            if (tab.FullPath() == target_path) {
                return src_tabview.ContainerFromIndex(i).as<muxc::TabViewItem>();
            }
            i++;
        }
        return nullptr;
    }
    IAsyncOperation<ContentDialogResult> MainPage::ShowSimpleDialogAsync(hstring const& title, hstring const& content) {
        auto dialog = ContentDialog();
        dialog.XamlRoot(XamlRoot());
        dialog.Title(box_value(title));
        dialog.Content(box_value(content));
        dialog.PrimaryButtonText(L"OK");
        dialog.DefaultButton(ContentDialogButton::Primary);
        return dialog.ShowAsync();
    }
    void MainPage::AddNewBreakpoint(hstring const& path, SrcSpan span, bool temporary) {
        auto tvi = GetCurrentSourcesFileTabViewItem(path);
        if (!tvi) { return; }
        auto src_item = tvi.DataContext().try_as<MEraEmuWin::DevTools::SourcesFileTabItem>();
        if (!src_item) { return; }
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { return; }
        auto editor = editor_ctrl.Editor();

        auto item = MEraEmuWin::DevTools::SourcesTabBreakpointItem();
        item.IsImplicit(temporary);
        item.MarkerHandle(-1);
        item.IsEnabled(true);
        item.IsApplied(false);
        item.FuncName(L"???");
        item.FileName(path);
        item.LineNumber(1);
        item.ColumnNumber(1);
        item.SrcSpanStart(span.start);
        item.SrcSpanLen(span.len);

        m_sources_tab_breakpoint_items.push_back(item);

        if (!temporary) {
            // Should also be displayed in the UI
            auto line = editor.LineFromPosition(span.start);
            auto handle = editor.MarkerAdd(line, BreakPointMarker);
            item.LineNumber(line + 1);
            item.MarkerHandle(handle);
            m_filtered_sources_tab_breakpoint_items.Append(item);
        }

        if (m_engine_running) {
            // Apply the new breakpoint
            ApplyEngineBreakpointsAsync(true);
        }
    }
    void MainPage::AddNewTempBreakpointByIp(EraExecIp ip) {
        auto item = MEraEmuWin::DevTools::SourcesTabBreakpointItem();
        item.IsImplicit(true);
        item.MarkerHandle(-1);
        item.IsEnabled(true);
        item.IsApplied(false);
        item.FuncName(L"???");
        item.FileName(L"???");
        item.LineNumber(1);
        item.ColumnNumber(1);
        item.SrcSpanStart(-1);
        item.SrcSpanLen(-1);
        item.IpChunk(ip.chunk);
        item.IpOffset(ip.offset);
        m_sources_tab_breakpoint_items.push_back(item);
    }
    void MainPage::RemoveBreakpoint(hstring const& path, int32_t handle) {
        MEraEmuWin::DevTools::SourcesTabBreakpointItem item = nullptr;
        for (auto& bp : m_sources_tab_breakpoint_items) {
            // Skip implicit breakpoints as they never get displayed in the UI
            if (bp.IsImplicit()) { continue; }
            if (bp.FileName() == path && bp.MarkerHandle() == handle) {
                item = bp;
                break;
            }
        }
        if (!item) { return; }

        if (!item.IsImplicit()) {
            auto tvi = GetCurrentSourcesFileTabViewItem(path);
            if (!tvi) { return; }
            auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
            if (!editor_ctrl) { return; }
            auto editor = editor_ctrl.Editor();
            editor.MarkerDeleteHandle(item.MarkerHandle());
            uint32_t index;
            if (m_filtered_sources_tab_breakpoint_items.IndexOf(item, index)) {
                m_filtered_sources_tab_breakpoint_items.RemoveAt(index);
            }

            // Postpone the removal to properly unpatch the engine code later
            item.IsImplicit(true);
        }

        // Purge the removed breakpoints
        auto op = ApplyEngineBreakpointsAsync(false);
        if (m_engine_running) {
            op.Completed([this, strong_this = get_strong()](auto&&, auto&&) {
                // Re-apply breakpoints
                ApplyEngineBreakpointsAsync(true);
            });
        }
    }
    void MainPage::SyncBreakpointsWithSourceFile(hstring const& path) {
        // Used to fix the breakpoint line number when the source file is modified in the editor.
        // TODO: SyncBreakpointsWithSourceFile
    }
    IAsyncOperation<bool> MainPage::ApplyEngineBreakpointsAsync(bool do_apply) {
        auto strong_this = get_strong();

        bool has_changes = false;

        if (do_apply) {
            std::vector<MEraEmuWin::DevTools::SourcesTabBreakpointItem> breakpoints;
            for (auto& bp : m_sources_tab_breakpoint_items) {
                if (bp.IsApplied()) { continue; }
                if (!bp.IsEnabled()) { continue; }
                breakpoints.push_back(bp);
            }

            for (auto& bp : breakpoints) {
                bp.IsApplied(true);
            }

            if (!breakpoints.empty()) {
                co_await m_engine_ctrl->ExecEngineTask([&](MEraEngine const& e) {
                    // TODO: What to do if an exception is thrown while applying breakpoints?
                    std::vector<std::pair<EraExecIp, std::vector<uint8_t>>> ip_bcs;
                    std::erase_if(breakpoints, [&](auto& bp) {
                        auto ip_chunk = bp.IpChunk();
                        auto ip_offset = bp.IpOffset();
                        std::optional<EraExecIp> ip;
                        if (ip_chunk != -1 && ip_offset != -1) {
                            // Use the cached ip if available
                            ip = EraExecIp{ ip_chunk, ip_offset };
                        }
                        if (!ip) {
                            // Calculate the ip from the source span
                            ip = e.get_ip_from_src(to_string(bp.FileName()), { bp.SrcSpanStart(), bp.SrcSpanLen() });
                        }
                        if (!ip) { return true; }
                        if (std::ranges::find(ip_bcs, *ip, [](const auto& v) { return v.first; }) != ip_bcs.end()) {
                            // Already applied, different breakpoint with same ip?
                            return true;
                        }
                        auto bc = e.read_bytecode(ip->chunk, ip->offset, 1);
                        if (!bc) { return true; }
                        ip_bcs.push_back({ *ip, std::move(*bc) });
                        return false;
                    });
                    for (size_t i = 0; auto & ip_bc : ip_bcs) {
                        has_changes = true;
                        OutputDebugStringW(winrt::format(L"DevTools: Applying breakpoint at {}:{}\n", ip_bc.first.chunk, ip_bc.first.offset).c_str());
                        e.patch_bytecode(ip_bc.first.chunk, ip_bc.first.offset, { { MEE_DEBUG_BREAK_BYTECODE } });
                        breakpoints[i].as<SourcesTabBreakpointItem>()->original_bytecode() = std::move(ip_bc.second);
                        i++;
                    }
                });
            }
        }
        else {
            std::vector<MEraEmuWin::DevTools::SourcesTabBreakpointItem> breakpoints;
            for (auto& bp : m_sources_tab_breakpoint_items) {
                if (!bp.IsApplied()) { continue; }
                breakpoints.push_back(bp);
            }

            for (auto& bp : breakpoints) {
                bp.IsApplied(false);
            }

            if (!breakpoints.empty()) {
                co_await m_engine_ctrl->ExecEngineTask([&](MEraEngine const& e) {
                    // TODO: What to do if an exception is thrown while un-applying breakpoints?
                    std::vector<EraExecIp> ips;
                    std::erase_if(breakpoints, [&](auto& bp) {
                        auto ip_chunk = bp.IpChunk();
                        auto ip_offset = bp.IpOffset();
                        std::optional<EraExecIp> ip;
                        if (ip_chunk != -1 && ip_offset != -1) {
                            // Use the cached ip if available
                            ip = EraExecIp{ ip_chunk, ip_offset };
                        }
                        if (!ip) {
                            // Calculate the ip from the source span
                            ip = e.get_ip_from_src(to_string(bp.FileName()), { bp.SrcSpanStart(), bp.SrcSpanLen() });
                        }
                        if (!ip) { return true; }
                        ips.push_back(*ip);
                        return false;
                    });
                    for (size_t i = 0; const auto & ip : ips) {
                        has_changes = true;
                        OutputDebugStringW(winrt::format(L"DevTools: Un-applying breakpoint at {}:{}\n", ip.chunk, ip.offset).c_str());
                        e.patch_bytecode(ip.chunk, ip.offset, breakpoints[i].as<SourcesTabBreakpointItem>()->original_bytecode());
                        i++;
                    }
                });
            }
        }

        if (!do_apply) {
            // Clear temporary breakpoints
            std::erase_if(m_sources_tab_breakpoint_items, [](const auto& bp) {
                if (!bp.IsImplicit()) { return false; }
                assert(!bp.IsApplied());
                return true;
            });
        }

        co_return has_changes;
    }
    void MainPage::ResumeEngineExecutionPossiblySteppingPastBreakpoint() {
        if (m_sources_tab_breakpoint_items.size() > 0) {
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
            m_dev_tools_op_state = DevToolsOperationState::ResumingFromSingleStepHalt;
        }
        else {
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::ClearHaltState);
        }
    }
}
