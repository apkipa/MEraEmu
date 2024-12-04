#include "pch.h"
#include "DevTools/MainPage.h"
#if __has_include("DevTools/MainPage.g.cpp")
#include "DevTools/MainPage.g.cpp"
#endif

using namespace winrt;
using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
//using namespace Microsoft::UI::Xaml::Controls;

namespace muxc = Microsoft::UI::Xaml::Controls;

using winrt::MEraEmuWin::implementation::EngineThreadTaskKind;

static constexpr int32_t BreakPointMargin = 1;
static constexpr int32_t LineNumberMargin = 2;
static constexpr int32_t BreakPointMarker = 11;

namespace winrt::MEraEmuWin::DevTools::implementation {
    MainPage::~MainPage() {
        if (m_engine_ctrl) {
            m_engine_ctrl->EngineExecutionInterrupted(m_et_EngineExecutionInterrupted);
        }
    }
    void MainPage::InitializeComponent() {
        MainPageT::InitializeComponent();

        VisualStateManager::GoToState(*this, L"EngineUnconnected", false);
    }
    void MainPage::SetConnectedEngineControl(MEraEmuWin::EngineControl engine) {
        // Close self first
        if (m_engine_ctrl) {
            m_engine_ctrl->EngineExecutionInterrupted(m_et_EngineExecutionInterrupted);
            m_engine_ctrl = nullptr;
            InitFromEngineControl();
        }

        if (!engine) { return; }

        m_engine_ctrl = engine.as<MEraEmuWin::implementation::EngineControl>().get();
        m_et_EngineExecutionInterrupted = m_engine_ctrl->EngineExecutionInterrupted({ this, &MainPage::OnEngineExecutionInterrupted });

        //InitFromEngineControl();
        OnEngineExecutionInterrupted(m_engine_ctrl->GetLastBreakReason());
    }
    void MainPage::SourceFilesTreeView_ItemInvoked(IInspectable const& sender, muxc::TreeViewItemInvokedEventArgs const& e) {
        auto node = e.InvokedItem().as<muxc::TreeViewNode>();
        if (!node) { return; }
        // Ignore folders
        if (node.HasChildren()) { return; }

        std::string path_str;
        while (node) {
            auto content = node.Content().as<IInspectable>();
            if (!content) { break; }
            auto filename = to_string(unbox_value<hstring>(content));
            path_str = std::move(filename) + (path_str.empty() ? "" : "\\") + std::move(path_str);
            node = node.Parent();
        }

        // Open existing file in editor or create new tab
        OpenOrCreateSourcesFileTab(to_hstring(path_str));
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
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SetHaltState);
        }
        else {
            // Resume execution
            m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::ClearHaltState);
        }
    }
    void MainPage::CodeExecStepIntoButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::CustomFuncAndClearHaltState);
    }
    void MainPage::CodeExecStepOverButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::CustomFuncAndClearHaltState);
    }
    void MainPage::CodeExecStepOutButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::CustomFuncAndClearHaltState);
    }
    void MainPage::CodeExecStepSingleButton_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        if (!m_engine_ctrl) { return; }
        m_engine_ctrl->QueueEngineTask(EngineThreadTaskKind::SingleStepAndHalt);
    }
    fire_forget MainPage::SourcesTabCallStackTabItem_Click(IInspectable const& sender, RoutedEventArgs const& e) {
        //auto item = sender.as<FrameworkElement>().DataContext().as<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>();
        //auto tvi = co_await OpenOrCreateSourcesFileTab(item.FileName());
        //auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        //if (!editor_ctrl) { co_return; }

        //// Move caret to the position
        //auto span_start = item.SrcSpanStart();
        //auto span_end = span_start + item.SrcSpanLen();
        //editor_ctrl.Editor().GotoPos(span_start);
        //editor_ctrl.Focus(FocusState::Programmatic);

        auto item = sender.as<FrameworkElement>().DataContext().as<MEraEmuWin::DevTools::SourcesTabCallStackTabItem>();
        co_await OpenOrCreateSourcesFileTabAtSrcSpan(item.FileName(), { item.SrcSpanStart(), item.SrcSpanLen() });
    }
    fire_forget MainPage::InitFromEngineControl() {
        auto src_files_tree_view = SourceFilesTreeView();

        if (!m_engine_ctrl) {
            // Uninit instead
            src_files_tree_view.RootNodes().Clear();
            m_sources_file_tab_items.Clear();
            VisualStateManager::GoToState(*this, L"EngineUnconnected", false);
            co_return;
        }

        auto strong_this = get_strong();

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
                    auto path_delim_pos = path.find_last_of('\\');
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
        InitFromEngineControl();

        if (is_execution_break_reason_fatal(reason)) {
            m_engine_running = false;
            VisualStateManager::GoToState(*this, L"EnginePaused", false);
            UpdateForEnginePausedState(reason);
        }
        else {
            m_engine_running = true;
            VisualStateManager::GoToState(*this, L"EngineRunning", false);
            UpdateForEngineRunningState(reason);
        }
    }
    fire_forget MainPage::UpdateForEnginePausedState(EraExecutionBreakReason reason) {
        auto strong_this = get_strong();

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
            OpenOrCreateSourcesFileTabAtSrcSpan(item.FileName(), { item.SrcSpanStart(), item.SrcSpanLen() });
        }
    }
    fire_forget MainPage::UpdateForEngineRunningState(EraExecutionBreakReason reason) {
        auto strong_this = get_strong();

        m_sources_tab_call_stack_tab_items.Clear();

        co_return;
    }
    IAsyncOperation<muxc::TabViewItem> MainPage::OpenOrCreateSourcesFileTab(hstring path) {
        auto strong_this = get_strong();

        std::wstring_view path_sv = path;
        auto source_tabview = SourceFilesTabView();

        for (size_t i = 0; auto && tab : m_sources_file_tab_items) {
            if (tab.FullPath() == path) {
                source_tabview.SelectedIndex(i);
                co_return source_tabview.ContainerFromIndex(i).as<muxc::TabViewItem>();
            }
            i++;
        }

        auto tab = make<SourcesFileTabItem>();
        tab.FileName(path_sv.substr(path_sv.rfind(L'\\') + 1));
        tab.FullPath(path);
        auto tab_index = m_sources_file_tab_items.Size();
        m_sources_file_tab_items.Append(tab);
        // TODO: Fix add tab missing animation
        source_tabview.SelectedIndex(tab_index);

        // Fetch file source
        auto source = (co_await m_engine_ctrl->ExecEngineTask([path = to_string(path)](MEraEngine const& e) {
            return e.get_file_source(path);
        })).value_or("");
        auto tvi = source_tabview.ContainerFromIndex(tab_index).as<muxc::TabViewItem>();
        // HACK: Wait for TabViewItem to be loaded
        co_await util::winrt::resume_when_loaded(source_tabview);
        while (!tvi) {
            if (!source_tabview.IsLoaded()) { break; }
            //co_await resume_foreground(DispatcherQueue::GetForCurrentThread(), DispatcherQueuePriority::Low);
            source_tabview.UpdateLayout();
            tvi = source_tabview.ContainerFromIndex(tab_index).as<muxc::TabViewItem>();
        }
        if (!tvi) { co_return tvi; }
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { co_return tvi; }
        auto editor = editor_ctrl.Editor();

        // TODO: If we don't wait, the editor will not be ready and invokes nullptr?
        //co_await resume_foreground(DispatcherQueue::GetForCurrentThread(), DispatcherQueuePriority::Low);
        co_await util::winrt::resume_when_loaded(editor_ctrl);

        editor.ClearAll();

        if (!tvi.Tag()) {
            // Manually prevent reinitialization thanks to TabViewItem being reused :(
            tvi.Tag(box_value(L"Inited"));
            InitializeCodeEditorControl(editor_ctrl);
        }

        editor.AppendTextFromBuffer((int64_t)source.size(), util::winrt::make_buffer_wrapper(source));
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
    IAsyncAction MainPage::OpenOrCreateSourcesFileTabAtSrcSpan(hstring path, SrcSpan span) {
        auto strong_this = get_strong();
        auto tvi = co_await OpenOrCreateSourcesFileTab(path);
        auto editor_ctrl = CodeEditorControlFromSourcesFileTabViewItem(tvi);
        if (!editor_ctrl) { co_return; }
        // Move caret to the position
        editor_ctrl.Editor().GotoPos(span.start);
        editor_ctrl.Focus(FocusState::Programmatic);
    }
    void MainPage::InitializeCodeEditorControl(MEraEmuWin::DevTools::CodeEditControl const& editor_ctrl) {
        if (!editor_ctrl) { return; }

        // Register event handlers
        auto editor = editor_ctrl.Editor();
        editor.SetMarginTypeN(0, WinUIEditor::MarginType::Symbol);
        editor.SetMarginTypeN(BreakPointMargin, WinUIEditor::MarginType::Symbol);
        editor.SetMarginSensitiveN(BreakPointMargin, true);
        editor.SetMarginMaskN(BreakPointMargin, (1ul << BreakPointMarker));
        editor.SetMarginCursorN(BreakPointMargin, WinUIEditor::CursorShape::ReverseArrow);
        editor.SetMarginTypeN(LineNumberMargin, WinUIEditor::MarginType::Number);
        editor.SetMarginSensitiveN(LineNumberMargin, false);
        editor.SetMarginMaskN(LineNumberMargin, 0);
        editor.MarkerDefine(BreakPointMarker, WinUIEditor::MarkerSymbol::Circle);
        editor.MarkerSetBack(BreakPointMarker, RGB(0xff, 0, 0));
        editor.MarkerSetFore(BreakPointMarker, RGB(0xdc, 0x14, 0x3c));
        editor.MarginClick({ this, &MainPage::CodeEditorControl_MarginClick });
    }
    void MainPage::CodeEditorControl_MarginClick(WinUIEditor::Editor const& sender, WinUIEditor::MarginClickEventArgs const& e) {
        if (e.Margin() != BreakPointMargin) { return; }

        auto line = sender.LineFromPosition(e.Position());
        if (sender.MarkerGet(line) & (1 << BreakPointMarker)) {
            sender.MarkerDelete(line, BreakPointMarker);
        }
        else {
            sender.MarkerAdd(line, BreakPointMarker);
        }
    }
}
