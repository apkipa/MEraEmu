#include "pch.h"
#include "DevTools/CodeEditControl.h"
#if __has_include("DevTools/CodeEditControl.g.cpp")
#include "DevTools/CodeEditControl.g.cpp"
#endif

using namespace winrt;
using namespace Windows::UI::Xaml;

// Source: https://github.com/BreeceW/WinUIEdit/blob/main/WinUIEditor/CodeEditorHandler.cpp

using Scintilla::StylesCommon;
using Scintilla::KeyMod;
using Scintilla::Message;
using Scintilla::CaretPolicy;
using Scintilla::VisiblePolicy;
using Scintilla::FoldAction;
using Scintilla::MultiPaste;
using Scintilla::Layer;
using Scintilla::IndentView;
using Scintilla::MarkerSymbol;
using Scintilla::MarkerOutline;
using Scintilla::FindOption;
using Scintilla::FontSizeMultiplier;
using Scintilla::sptr_t;
using Scintilla::Notification;
using Scintilla::FlagSet;
using Scintilla::Update;
using Scintilla::FoldDisplayTextStyle;
using Scintilla::InvalidPosition;
using Scintilla::TextRangeFull;

// Duplicated from Helpers.cpp to make portable
// Derived from https://github.com/microsoft/Windows-universal-samples/blob/main/Samples/ComplexInk/cpp/Common/DeviceResources.h
static constexpr int ConvertFromDipToPixelUnit(float val, float dpiAdjustmentRatio, bool rounded = true) {
    float pixelVal = val * dpiAdjustmentRatio;
    return static_cast<int>(rounded ? floorf(pixelVal + 0.5f) : pixelVal); // Todo: Test if this is ever necessary
}

static constexpr int IntRGBA(unsigned int red, unsigned int green, unsigned int blue, unsigned int alpha = 0xff) noexcept {
    return red | (green << 8) | (blue << 16) | (alpha << 24);
}

static constexpr int BlendRGBA(uint32_t fg, uint32_t bg) noexcept {
    bg = bg | 0xff000000;
    const uint32_t fgAlpha = (fg >> 24) & 0xff;
    const uint32_t bgAlpha = (bg >> 24) & 0xff;
    const uint32_t alpha = fgAlpha + bgAlpha - fgAlpha * bgAlpha / 255;
    const uint32_t red = ((fg & 0xff) * fgAlpha + (bg & 0xff) * bgAlpha * (255 - fgAlpha) / 255) / alpha;
    const uint32_t green = (((fg >> 8) & 0xff) * fgAlpha + ((bg >> 8) & 0xff) * bgAlpha * (255 - fgAlpha) / 255) / alpha;
    const uint32_t blue = (((fg >> 16) & 0xff) * fgAlpha + ((bg >> 16) & 0xff) * bgAlpha * (255 - fgAlpha) / 255) / alpha;
    return red | (green << 8) | (blue << 16) | (alpha << 24);
}

namespace winrt::MEraEmuWin::DevTools::implementation {
    void CodeEditControl::InitializeComponent() {
        CodeEditControlT::InitializeComponent();

        ApplyTemplate();

        auto editor_ctrl = EditorControl();
        auto editor = Editor();
        editor_ctrl.DpiChanged({ this, &CodeEditControl::Editor_DpiChanged });
        editor_ctrl.NotifyMessageReceived({ this, &CodeEditControl::Editor_NotifyMessageReceived });

        GettingFocus({ this, &CodeEditControl::OnGettingFocus });
    }
    WinUIEditor::Editor CodeEditControl::Editor() {
        return EditorControl().Editor();
    }
    WinUIEditor::EditorBaseControl CodeEditControl::EditorControl() {
        return GetTemplateChild(L"BaseEditor").try_as<WinUIEditor::EditorBaseControl>();
    }
    void CodeEditControl::OnApplyTemplate() {
        CodeEditControlT::OnApplyTemplate();

        auto editor = Editor();
        {
            // Initialize Scintilla
            auto direct_status_fn = editor.DirectStatusFunction();
            auto direct_ptr = editor.DirectPointer();
            m_call.SetFnPtr(Scintilla::FunctionDirect(direct_status_fn), direct_ptr);
        }

        AddKeyboardShortcuts();
        ChangeDefaults();
        ChangeDocumentDefaults();

        m_theme = static_cast<CodeEditorTheme>(-1);
        UpdateColors(ActualTheme());
        ActualThemeChanged([this](auto&&, auto&&) {
            UpdateColors(ActualTheme());
        });
    }
    void CodeEditControl::OnGettingFocus(UIElement const& sender, Windows::UI::Xaml::Input::GettingFocusEventArgs const& e) {
        if (e.NewFocusedElement() != *this) { return; }

        e.Handled(true);

        auto editor_ctrl = EditorControl();
        if (!e.TrySetNewFocusedElement(editor_ctrl)) {
            // Fallback with DispatcherQueue
            Windows::System::DispatcherQueue::GetForCurrentThread().TryEnqueue([editor_ctrl] {
                editor_ctrl.Focus(Windows::UI::Xaml::FocusState::Programmatic);
            });
        }
    }
    void CodeEditControl::Editor_DpiChanged(IInspectable const& sender, double value) {
        UpdateDpi(value);
    }
    void CodeEditControl::Editor_NotifyMessageReceived(IInspectable const& sender, int64_t value) {
        ProcessNotification(reinterpret_cast<Scintilla::NotificationData*>(value));
    }

    void CodeEditControl::ChangeDocumentDefaults() {
        m_call.SetUseTabs(false);
        m_call.SetTabWidth(4);
        m_call.SetIndent(4); // Brace matching and autoindent relies on this
    }

    void CodeEditControl::UpdateColors(CodeEditorTheme theme) {
        // Todo: Support high contrast mode

        constexpr auto folderForeDark{ IntRGBA(0, 0, 0, 0) };
        constexpr auto folderBackDark{ IntRGBA(0xFF, 0xFF, 0xFF, 148) };
        constexpr auto folderBackHighlightDark{ folderBackDark };
        constexpr auto folderForeLight{ IntRGBA(0xFF, 0xFF, 0xFF, 0) };
        constexpr auto folderBackLight{ IntRGBA(0, 0, 0, 108) };
        constexpr auto folderBackHighlightLight{ folderBackLight };

        if (m_theme != theme) {
            m_theme = theme;

            switch (theme) {
            case CodeEditorTheme::Dark:
                m_call.SetElementColour(Scintilla::Element::Caret, IntRGBA(0xAE, 0xAF, 0xAD));
                m_call.SetElementColour(Scintilla::Element::SelectionBack, IntRGBA(0x26, 0x4F, 0x78));
                m_call.SetElementColour(Scintilla::Element::SelectionAdditionalBack, IntRGBA(0x26, 0x4F, 0x78));
                m_call.SetElementColour(Scintilla::Element::SelectionInactiveBack, IntRGBA(0x3A, 0x3D, 0x41));
                m_call.SetElementColour(Scintilla::Element::HiddenLine, IntRGBA(0xFF, 0xFF, 0xFF, 48));

                MarkerSetColors(Scintilla::MarkerOutline::FolderOpen, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::Folder, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::FolderSub, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::FolderTail, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::FolderEnd, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::FolderOpenMid, folderForeDark, folderBackDark, folderBackHighlightDark);
                MarkerSetColors(Scintilla::MarkerOutline::FolderMidTail, folderForeDark, folderBackDark, folderBackHighlightDark);

                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToOrigin), IntRGBA(0x35, 0x95, 0xDE));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToOrigin), IntRGBA(0x35, 0x95, 0xDE, 0x00));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistorySaved), IntRGBA(0x55, 0xB1, 0x55));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistorySaved), IntRGBA(0x55, 0xB1, 0x55));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryModified), IntRGBA(0xD0, 0xB1, 0x32));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryModified), IntRGBA(0x27, 0x27, 0x27, 0x00));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToModified), IntRGBA(0x93, 0xB1, 0x44));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToModified), IntRGBA(0x93, 0xB1, 0x44, 0x00));
                break;

            case CodeEditorTheme::Light:
                m_call.SetElementColour(Scintilla::Element::Caret, IntRGBA(0x00, 0x00, 0x00));
                m_call.SetElementColour(Scintilla::Element::SelectionBack, IntRGBA(0xAD, 0xD6, 0xFF));
                m_call.SetElementColour(Scintilla::Element::SelectionAdditionalBack, IntRGBA(0xAD, 0xD6, 0xFF));
                m_call.SetElementColour(Scintilla::Element::SelectionInactiveBack, IntRGBA(0xE5, 0xEB, 0xF1));
                m_call.SetElementColour(Scintilla::Element::HiddenLine, IntRGBA(0x00, 0x00, 0x00, 64));

                MarkerSetColors(Scintilla::MarkerOutline::FolderOpen, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::Folder, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::FolderSub, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::FolderTail, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::FolderEnd, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::FolderOpenMid, folderForeLight, folderBackLight, folderBackHighlightLight);
                MarkerSetColors(Scintilla::MarkerOutline::FolderMidTail, folderForeLight, folderBackLight, folderBackHighlightLight);

                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToOrigin), IntRGBA(0x00, 0x78, 0xD4));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToOrigin), IntRGBA(0x00, 0x78, 0xD4, 0x00));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistorySaved), IntRGBA(0x10, 0x7C, 0x10));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistorySaved), IntRGBA(0x10, 0x7C, 0x10));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryModified), IntRGBA(0xAE, 0x8C, 0x00));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryModified), IntRGBA(0xF5, 0xF5, 0xF5, 0x00));
                m_call.MarkerSetForeTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToModified), IntRGBA(0x5F, 0x84, 0x08));
                m_call.MarkerSetBackTranslucent(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToModified), IntRGBA(0x5F, 0x84, 0x08, 0x00));
                break;
            }

            UpdateCaretLineBackColors(true);

            UpdateStyles();
        }
    }

    void CodeEditControl::UpdateDpi(double value) {
        m_dpi_scale = value;
        m_call.SetXCaretPolicy(Scintilla::CaretPolicy::Slop | Scintilla::CaretPolicy::Strict | Scintilla::CaretPolicy::Even, ConvertFromDipToPixelUnit(24, value));
        UpdateZoom();
    }

    void CodeEditControl::StyleSetFore(int style, Scintilla::ColourAlpha color) {
        // Overridable to allow users to internally set transparent color
        m_call.StyleSetFore(style, color);
        // TODO: Fork WinUIEdit to expose the transparent APIs later
        if (auto brush = Application::Current().Resources().Lookup(box_value(L"SolidBackgroundFillColorTertiaryBrush")).try_as<Windows::UI::Xaml::Media::SolidColorBrush>()) {
            auto bg_clr = brush.Color();
            m_call.StyleSetFore(style, BlendRGBA(color, IntRGBA(bg_clr.R, bg_clr.G, bg_clr.B)));
        }
    }

    void CodeEditControl::StyleSetBack(int style, Scintilla::ColourAlpha color) {
        // Overridable to allow users to internally set transparent color
        m_call.StyleSetBack(style, color);
        // TODO: Fork WinUIEdit to expose the transparent APIs later
        if (auto brush = Application::Current().Resources().Lookup(box_value(L"SolidBackgroundFillColorTertiaryBrush")).try_as<Windows::UI::Xaml::Media::SolidColorBrush>()) {
            auto bg_clr = brush.Color();
            m_call.StyleSetBack(style, BlendRGBA(color, IntRGBA(bg_clr.R, bg_clr.G, bg_clr.B)));
        }
    }

    void CodeEditControl::InvalidateStyleRedraw() {
        // Overridable to allow users to invalidate after setting all colors
        // Not needed if using default StyleSetFore/Back
    }

    void CodeEditControl::StyleClearCustom() {
        // Less performant way to copy styles
        // Overridable when there is access to Scintilla internals
        for (size_t i = 0; i < 256; i++) {
            if (i < static_cast<int>(StylesCommon::Default) || i > static_cast<int>(StylesCommon::LastPredefined)) {
                m_call.StyleSetFont(i, m_call.StyleGetFont(static_cast<int>(StylesCommon::Default)).c_str());
                m_call.StyleSetSizeFractional(i, m_call.StyleGetSizeFractional(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetBold(i, m_call.StyleGetBold(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetWeight(i, m_call.StyleGetWeight(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetItalic(i, m_call.StyleGetItalic(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetUnderline(i, m_call.StyleGetUnderline(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetFore(i, m_call.StyleGetFore(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetBack(i, m_call.StyleGetBack(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetEOLFilled(i, m_call.StyleGetEOLFilled(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetCharacterSet(i, m_call.StyleGetCharacterSet(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetCase(i, m_call.StyleGetCase(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetVisible(i, m_call.StyleGetVisible(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetChangeable(i, m_call.StyleGetChangeable(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetHotSpot(i, m_call.StyleGetHotSpot(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetCheckMonospaced(i, m_call.StyleGetCheckMonospaced(static_cast<int>(StylesCommon::Default)));
                m_call.StyleSetInvisibleRepresentation(i, m_call.StyleGetInvisibleRepresentation(static_cast<int>(StylesCommon::Default)).c_str());
            }
        }
    }

    void CodeEditControl::SetFoldMarginColor(bool useSetting, Scintilla::ColourAlpha back) {
        // Implement for transparent folding margin. Not implemented so default is preserved
        m_call.SetFoldMarginColour(useSetting, back);
    }

    void CodeEditControl::SetFoldMarginHiColor(bool useSetting, Scintilla::ColourAlpha fore) {
        // Implement for transparent folding margin. Not implemented so default is preserved
        m_call.SetFoldMarginHiColour(useSetting, fore);
    }

    void CodeEditControl::DefaultColorsChanged(CodeEditorTheme theme) {
        // Default event handler
    }

    void CodeEditControl::SyntaxHighlightingApplied(CodeEditorTheme theme) {
        // Default event handler
    }

    void CodeEditControl::UpdateStyles() {
        constexpr auto bgDark{ IntRGBA(0x27, 0x27, 0x27, 0x00) };
        constexpr auto bgLight{ IntRGBA(0xF5, 0xF5, 0xF5, 0x00) };

        switch (m_theme) {
        case CodeEditorTheme::Dark:
            StyleSetFore(static_cast<int>(StylesCommon::LineNumber), IntRGBA(0x85, 0x85, 0x85));
            StyleSetBack(static_cast<int>(StylesCommon::LineNumber), bgDark);

            StyleSetFore(static_cast<int>(StylesCommon::Default), IntRGBA(0xFF, 0xFF, 0xFF));
            StyleSetBack(static_cast<int>(StylesCommon::Default), bgDark);

            StyleSetFore(static_cast<int>(StylesCommon::BraceLight), IntRGBA(0xFF, 0xFF, 0xFF));
            StyleSetBack(static_cast<int>(StylesCommon::BraceLight), IntRGBA(0x11, 0x3D, 0x6F));
            StyleSetFore(static_cast<int>(StylesCommon::BraceBad), IntRGBA(0xcd, 0x31, 0x31));
            StyleSetBack(static_cast<int>(StylesCommon::BraceBad), bgDark);

            StyleSetFore(static_cast<int>(StylesCommon::IndentGuide), IntRGBA(0xFF, 0xFF, 0xFF, 48));
            StyleSetBack(static_cast<int>(StylesCommon::IndentGuide), bgDark);

            StyleSetFore(static_cast<int>(StylesCommon::ControlChar), IntRGBA(0xFF, 0xFF, 0xFF));
            StyleSetBack(static_cast<int>(StylesCommon::ControlChar), IntRGBA(0x96, 0x00, 0x00));

            StyleSetFore(static_cast<int>(StylesCommon::FoldDisplayText), IntRGBA(0xB8, 0xC2, 0xCC));
            StyleSetBack(static_cast<int>(StylesCommon::FoldDisplayText), IntRGBA(0x26, 0x33, 0x3F));
            break;

        case CodeEditorTheme::Light:
            StyleSetFore(static_cast<int>(StylesCommon::LineNumber), IntRGBA(0x23, 0x78, 0x93));
            StyleSetBack(static_cast<int>(StylesCommon::LineNumber), bgLight);

            StyleSetFore(static_cast<int>(StylesCommon::Default), IntRGBA(0x00, 0x00, 0x00));
            StyleSetBack(static_cast<int>(StylesCommon::Default), bgLight);

            StyleSetFore(static_cast<int>(StylesCommon::BraceLight), IntRGBA(0x00, 0x00, 0x00));
            StyleSetBack(static_cast<int>(StylesCommon::BraceLight), IntRGBA(0xE2, 0xE6, 0xD6));
            StyleSetFore(static_cast<int>(StylesCommon::BraceBad), IntRGBA(0xcd, 0x31, 0x31));
            StyleSetBack(static_cast<int>(StylesCommon::BraceBad), bgLight);

            StyleSetFore(static_cast<int>(StylesCommon::IndentGuide), IntRGBA(0x00, 0x00, 0x00, 64));
            StyleSetBack(static_cast<int>(StylesCommon::IndentGuide), bgLight);

            StyleSetFore(static_cast<int>(StylesCommon::ControlChar), IntRGBA(0xFF, 0xFF, 0xFF));
            StyleSetBack(static_cast<int>(StylesCommon::ControlChar), IntRGBA(0x96, 0x00, 0x00));

            StyleSetFore(static_cast<int>(StylesCommon::FoldDisplayText), IntRGBA(0x73, 0x79, 0x80));
            StyleSetBack(static_cast<int>(StylesCommon::FoldDisplayText), IntRGBA(0xDC, 0xEA, 0xF5));
            break;
        }

        DefaultColorsChanged(m_theme);

        UpdateLanguageStyles();

        SyntaxHighlightingApplied(m_theme);

        InvalidateStyleRedraw();
    }

    void CodeEditControl::MarkerSetColors(Scintilla::MarkerOutline marker, Scintilla::ColourAlpha fore, Scintilla::ColourAlpha back, Scintilla::ColourAlpha backHighlight) {
        const auto markerNumber{ static_cast<int>(marker) };
        m_call.MarkerSetForeTranslucent(markerNumber, fore);
        m_call.MarkerSetBackTranslucent(markerNumber, back);
        m_call.MarkerSetBackSelectedTranslucent(markerNumber, backHighlight);
    }

    void CodeEditControl::UpdateCaretLineBackColors(bool colorsUpdated) {
        const auto hasEmptySelection = m_call.SelectionEmpty();

        if ((m_has_empty_selection != hasEmptySelection) || (hasEmptySelection && colorsUpdated)) {
            m_has_empty_selection = hasEmptySelection;

            if (hasEmptySelection) {
                switch (m_theme) {
                case CodeEditorTheme::Dark:
                    m_call.SetElementColour(Scintilla::Element::CaretLineBack, IntRGBA(0xFF, 0xFF, 0xFF, 16));
                    break;

                case CodeEditorTheme::Light:
                    m_call.SetElementColour(Scintilla::Element::CaretLineBack, IntRGBA(0x00, 0x00, 0x00, 12));
                    break;
                }
            }
            else {
                m_call.ResetElementColour(Scintilla::Element::CaretLineBack);
            }
        }
    }

    void CodeEditControl::UpdateBraceMatch() {
        SciBraceMatch();
    }

    void CodeEditControl::AutoIndent(char ch) {
        SciAutomaticIndentation(ch);
    }

    void CodeEditControl::SetLanguageIndentMode(
        int indentKeywordStyle, const std::set<std::string>& indentKeywords,
        int lineEndStyle, const std::set<std::string>& lineEndWords,
        int blockStartStyle, const std::set<std::string>& blockStartWords,
        int blockEndStyle, const std::set<std::string>& blockEndWords) {
        _sciStatementIndent = { indentKeywordStyle, { indentKeywords, } }; // Todo: customize. also note using WORD2 for these
        _sciStatementEnd = { lineEndStyle, { lineEndWords, } };
        _sciBlockStart = { blockStartStyle, { blockStartWords, } };
        _sciBlockEnd = { blockEndStyle, { blockEndWords, } };
    }

    void CodeEditControl::UpdateZoom() {
        const auto size{ m_call.StyleGetSizeFractional(static_cast<int>(Scintilla::StylesCommon::Default)) };
        const auto zoom{ static_cast<int>(m_call.Zoom()) };
        const auto factor{ static_cast<float>((size + zoom * 100)) / size };
        // Todo: Width 11 hard coded for a digit. Use below for real value:
        // m_call.TextWidth, static_cast<uptr_t>(StylesCommon::LineNumber), reinterpret_cast<sptr_t>("9"))
        const auto line{ m_call.LineCount() };
        //const auto width{ 12 + 11 * std::max(3, static_cast<int>(std::floor(std::log10(line) + 1))) };
        const auto width{ 0 + 11 * std::max(3, static_cast<int>(std::floor(std::log10(line) + 1))) };
        m_call.SetMarginWidthN(0, ConvertFromDipToPixelUnit(std::floorf(factor * 4 + 0.5f), m_dpi_scale));
        m_call.SetMarginWidthN(1, ConvertFromDipToPixelUnit(std::floorf(factor * 16 + 0.5f), m_dpi_scale));
        m_call.SetMarginWidthN(2, ConvertFromDipToPixelUnit(std::floorf(factor * width + 0.5f), m_dpi_scale));
        m_call.SetMarginWidthN(3, ConvertFromDipToPixelUnit(std::floorf(factor * 10 + 0.5f), m_dpi_scale));
        m_call.SetMarginWidthN(4, ConvertFromDipToPixelUnit(std::floorf(factor * 0 + 0.5f), m_dpi_scale));
        const auto foldMarkerStroke{ ConvertFromDipToPixelUnit(factor * 100.0f, m_dpi_scale, false) };
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderOpen), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::Folder), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderSub), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderTail), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderEnd), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderOpenMid), foldMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::FolderMidTail), foldMarkerStroke);
        const auto historyMarkerStroke{ foldMarkerStroke };
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToOrigin), historyMarkerStroke);
        // HistorySaved is left at default width (1) because rendering artifacts occur with the solid color + box outline
        //m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::HistorySaved), historyMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::HistoryModified), historyMarkerStroke);
        m_call.MarkerSetStrokeWidth(static_cast<int>(Scintilla::MarkerOutline::HistoryRevertedToModified), historyMarkerStroke);
        m_call.SetMarginLeft(ConvertFromDipToPixelUnit(std::floorf(factor * 1 + 0.5f), m_dpi_scale));
        // Todo: Set caret width to be at least the UISettings system caret width
        const auto caretWidth{ std::max(1.0f, std::floorf(factor * 2 * m_dpi_scale)) };
        m_call.SetCaretWidth(caretWidth); // Todo: Needs to stop blinking after timeout and respect blink rate
        m_call.SetCaretLineFrame(caretWidth);
        m_call.SetExtraDescent(std::floorf(factor * 1.8 * m_dpi_scale));
    }

    void CodeEditControl::AddKeyboardShortcuts() {
        m_call.AssignCmdKey(187 + (static_cast<int>(KeyMod::Ctrl) << 16), static_cast<int>(Message::ZoomIn)); // Ctrl+Plus
        m_call.AssignCmdKey(189 + (static_cast<int>(KeyMod::Ctrl) << 16), static_cast<int>(Message::ZoomOut)); // Ctrl+Minus
        m_call.AssignCmdKey(48 + (static_cast<int>(KeyMod::Ctrl) << 16), static_cast<int>(Message::SetZoom)); // Ctrl+0
    }

    void CodeEditControl::ChangeDefaults() {
        m_call.SetMultipleSelection(true);
        m_call.SetScrollWidth(2000);
        m_call.SetScrollWidthTracking(true);
        m_call.SetYCaretPolicy(CaretPolicy::Slop | CaretPolicy::Strict | CaretPolicy::Even, 1);
        m_call.SetVisiblePolicy(VisiblePolicy::Slop, 0);
        m_call.SetHScrollBar(true);
        m_call.SetEndAtLastLine(false);
        m_call.StyleSetFont(static_cast<int>(StylesCommon::Default), "Cascadia Code"); // Todo: Use font available on Windows 10
        m_call.StyleSetSizeFractional(static_cast<int>(StylesCommon::Default), 11 * FontSizeMultiplier);
        m_call.SetAdditionalSelectionTyping(true);
        m_call.SetMultiPaste(MultiPaste::Each);
        m_call.SetLayoutThreads(16); // Todo: Determine performance impact
        m_call.SetCaretLineVisibleAlways(true);
        m_call.SetCaretLineLayer(Layer::UnderText);
        m_call.SetCaretLineHighlightSubLine(true);
        m_call.SetIndentationGuides(IndentView::LookBoth);
        m_call.SetMarginMaskN(2, Scintilla::MaskFolders);
        m_call.SetMarginSensitiveN(2, true);
        SetFoldMarginColor(true, IntRGBA(0, 0, 0, 0));
        SetFoldMarginHiColor(true, IntRGBA(0, 0, 0, 0));

        constexpr auto useCustomChevron{ true }; // Todo: make overridable
        constexpr auto chevronMinus{ static_cast<MarkerSymbol>(1989) };
        constexpr auto chevronPlus{ static_cast<MarkerSymbol>(1990) };
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderOpen), useCustomChevron ? chevronMinus : MarkerSymbol::BoxMinus);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::Folder), useCustomChevron ? chevronPlus : MarkerSymbol::BoxPlus);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderSub), MarkerSymbol::VLine);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderTail), MarkerSymbol::LCorner);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderEnd), useCustomChevron ? chevronPlus : MarkerSymbol::BoxPlusConnected);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderOpenMid), useCustomChevron ? chevronMinus : MarkerSymbol::BoxMinusConnected);
        m_call.MarkerDefine(static_cast<int>(MarkerOutline::FolderMidTail), MarkerSymbol::TCorner);
        m_call.MarkerEnableHighlight(false);

        m_call.SetDefaultFoldDisplayText("\u00a0\u22ef\u00a0"); // ... nbsp + centered vertically + nbsp
        m_call.FoldDisplayTextSetStyle(Scintilla::FoldDisplayTextStyle::Standard);

        m_call.SetAutomaticFold(static_cast<Scintilla::AutomaticFold>(static_cast<int>(Scintilla::AutomaticFold::Show) | static_cast<int>(Scintilla::AutomaticFold::Click) | static_cast<int>(Scintilla::AutomaticFold::Change)));
    }

    // Todo: This code needs to integrate with find and replace
    // Todo: This code seems to struggle with rectangular selections and/or multiple carets already existing and/or multiple selections already existing
    void CodeEditControl::ChangeAllOccurrences() {
        sptr_t start;
        sptr_t end;
        const auto s{ GetMainSelectedText(true, start, end) };
        if (s.length() == 0) {
            return;
        }
        const auto length{ end - start };

        // Drop additional selections and ensure caret is at end of selection
        m_call.SetSelection(end, start);

        m_call.TargetWholeDocument();
        m_call.SetSearchFlags(FindOption::MatchCase);

        const auto mainSelection{ m_call.MainSelection() };
        const auto bodyLength{ m_call.Length() };

        while (true) {
            const auto match{ m_call.SearchInTarget(length, &s[0]) };

            if (match == -1) {
                break;
            }

            const auto targetEnd{ m_call.TargetEnd() };

            if (match != start) {
                // Todo: Add maximum number of carets and notify user if exceeded (VS Code allows 10,000)
                // Todo: This method calls a lot of redraws in a loop. You might need to use the lower level API to avoid that
                m_call.AddSelection(match + length, match);
            }

            m_call.SetTargetStart(targetEnd);
            m_call.SetTargetEnd(bodyLength);
        }

        m_call.SetMainSelection(mainSelection);
    }

    std::wstring_view CodeEditControl::HighlightingLanguage() {
        return m_highlighting_language;
    }

    void CodeEditControl::HighlightingLanguage(std::wstring_view value) {
        if (m_highlighting_language == value) {
            return;
        }

        m_highlighting_language = value;

        m_call.ClearDocumentStyle();

        //SetLexer();

        UpdateStyles();

        m_call.ColouriseAll();
    }

    void CodeEditControl::ProcessNotification(Scintilla::NotificationData* data) {
        if (data->nmhdr.code == Notification::Zoom
            || (data->nmhdr.code == Notification::Modified && data->linesAdded)) {
            UpdateZoom();
        }

        if (data->nmhdr.code == Notification::UpdateUI && FlagSet(data->updated, Update::Content | Update::Selection)) {
            UpdateCaretLineBackColors();
            if (m_call.Focus()) {
                UpdateBraceMatch();
            }
        }

        if (data->nmhdr.code == Notification::FocusOut) {
            m_call.BraceBadLight(-1);
        }
        else if (data->nmhdr.code == Notification::FocusIn) {
            UpdateBraceMatch();
        }

        if (data->nmhdr.code == Notification::CharAdded && data->ch <= 0xFF) {
            const auto sel{ m_call.SelectionSpan() };

            if (sel.start == sel.end && sel.start > 0
                && !m_call.CallTipActive()
                && !m_call.AutoCActive()) {
                AutoIndent(data->ch);
            }
        }

        if (data->nmhdr.code == Notification::DoubleClick && m_call.FoldDisplayTextGetStyle() != FoldDisplayTextStyle::Hidden) {
            const auto lineEndPosition{ m_call.LineEndPosition(data->line) };
            if ((data->position == lineEndPosition || data->position == InvalidPosition)
                && !m_call.FoldExpanded(data->line)) {
                const auto ctrl{ FlagSet(data->modifiers, KeyMod::Ctrl) };
                const auto shift{ FlagSet(data->modifiers, KeyMod::Shift) };
                const auto levelClick{ m_call.FoldLevel(data->line) };
                if (LevelIsHeader(levelClick)) {
                    if (shift) {
                        // Ensure all children visible
                        m_call.ExpandChildren(data->line, levelClick);
                    }
                    else if (ctrl) {
                        m_call.FoldChildren(data->line, FoldAction::Toggle);
                    }
                    else {
                        // Toggle this line
                        m_call.FoldLine(data->line, FoldAction::Toggle);
                    }

                    // Remove selection from double click
                    m_call.SetEmptySelection(lineEndPosition);
                }
            }
        }
    }

    std::string CodeEditControl::GetMainSelectedText(bool expandCaretToWord, sptr_t& start, sptr_t& end) {
        // Todo: This code may be problematic for rectangular selections
        start = m_call.SelectionStart();
        end = m_call.SelectionEnd();
        if (expandCaretToWord && start == end) {
            start = m_call.WordStartPosition(start, true);
            end = m_call.WordEndPosition(start, true);

            if (start == end) {
                return "";
            }
        }

        const auto length{ end - start };
        std::string s(length, '\0');
        TextRangeFull range{ { start, end, },
            &s[0],
        };
        m_call.GetTextRangeFull(&range);

        return s;
    }

    void CodeEditControl::UpdateLanguageStyles() {
        StyleClearCustom();
    }
}
