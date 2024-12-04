#pragma once

#include "winrt/Windows.UI.Xaml.h"
#include "winrt/Windows.UI.Xaml.Markup.h"
#include "winrt/Windows.UI.Xaml.Interop.h"
#include "winrt/Windows.UI.Xaml.Controls.Primitives.h"
#include "DevTools/CodeEditControl.g.h"

#include "ScintillaModule.hpp"

namespace winrt::MEraEmuWin::DevTools::implementation {
	enum class IndentationStatus {
		none, // no effect on indentation
		blockStart,	// indentation block begin such as "{" or VB "function"
		blockEnd, // indentation end indicator such as "}" or VB "end"
		keyWordStart, // Keywords that cause indentation
	};

	class StyleAndWords {
		int styleNumber = 0;
		std::set<std::string> words;
	public:
		StyleAndWords() noexcept;
		explicit StyleAndWords(const std::string& definition);
		StyleAndWords(int style, const std::set<std::string>& words);
		[[nodiscard]] bool IsEmpty() const noexcept;
		[[nodiscard]] bool IsSingleChar() const noexcept;
		[[nodiscard]] bool IsCharacter(char ch) const noexcept;
		[[nodiscard]] int Style() const noexcept;
		[[nodiscard]] bool Includes(const std::string& value) const;
	};

	// Read only access to a document, its styles and other data
	class TextReader {
	protected:
		static constexpr Scintilla::Position extremePosition = INTPTR_MAX;
		/** @a bufferSize is a trade off between time taken to copy the characters
		 * and retrieval overhead.
		 * @a slopSize positions the buffer before the desired position
		 * in case there is some backtracking. */
		static constexpr Scintilla::Position bufferSize = 4000;
		static constexpr Scintilla::Position slopSize = bufferSize / 8;
		char buf[bufferSize + 1];
		Scintilla::Position startPos;
		Scintilla::Position endPos;
		int codePage;

		Scintilla::ScintillaCall& sc;
		Scintilla::Position lenDoc;

		void Fill(Scintilla::Position position);
	public:
		explicit TextReader(Scintilla::ScintillaCall& sc_) noexcept;
		// Deleted so TextReader objects can not be copied.
		TextReader(const TextReader& source) = delete;
		TextReader& operator=(const TextReader&) = delete;
		char operator[](Scintilla::Position position)
		{
			if (position < startPos || position >= endPos)
			{
				Fill(position);
			}
			return buf[position - startPos];
		}
		/** Safe version of operator[], returning a defined value for invalid position. */
		char SafeGetCharAt(Scintilla::Position position, char chDefault = ' ')
		{
			if (position < startPos || position >= endPos)
			{
				Fill(position);
				if (position < startPos || position >= endPos)
				{
					// Position is outside range of document
					return chDefault;
				}
			}
			return buf[position - startPos];
		}
		void SetCodePage(int codePage_) noexcept
		{
			codePage = codePage_;
		}
		bool Match(Scintilla::Position pos, const char* s);
		int StyleAt(Scintilla::Position position);
		Scintilla::Line GetLine(Scintilla::Position position);
		Scintilla::Position LineStart(Scintilla::Line line);
		Scintilla::FoldLevel LevelAt(Scintilla::Line line);
		Scintilla::Position Length();
		int GetLineState(Scintilla::Line line);
	};
}

namespace winrt::MEraEmuWin::DevTools::implementation {
    using CodeEditorTheme = Windows::UI::Xaml::ElementTheme;

    struct CodeEditControl : CodeEditControlT<CodeEditControl> {
        CodeEditControl() = default;

        void InitializeComponent();

        WinUIEditor::Editor Editor();
        WinUIEditor::EditorBaseControl EditorControl();

        void OnApplyTemplate();

    private:
        void OnGettingFocus(Windows::UI::Xaml::UIElement const& sender, Windows::UI::Xaml::Input::GettingFocusEventArgs const& e);
		void Editor_DpiChanged(Windows::Foundation::IInspectable const& sender, double value);
		void Editor_NotifyMessageReceived(Windows::Foundation::IInspectable const& sender, int64_t value);
        void ChangeDocumentDefaults();
        void UpdateColors(CodeEditorTheme theme);
        void UpdateDpi(double value);
        void StyleSetFore(int style, Scintilla::ColourAlpha color);
        void StyleSetBack(int style, Scintilla::ColourAlpha color);
        void InvalidateStyleRedraw();
        void StyleClearCustom();
        void SetFoldMarginColor(bool useSetting, Scintilla::ColourAlpha back);
        void SetFoldMarginHiColor(bool useSetting, Scintilla::ColourAlpha fore);
        void DefaultColorsChanged(CodeEditorTheme theme);
        void SyntaxHighlightingApplied(CodeEditorTheme theme);
        void UpdateStyles();
        void MarkerSetColors(Scintilla::MarkerOutline marker, Scintilla::ColourAlpha fore, Scintilla::ColourAlpha back, Scintilla::ColourAlpha backHighlight);
        void UpdateCaretLineBackColors(bool colorsUpdated = false);
        void UpdateBraceMatch();
        void AutoIndent(char ch);
        void SetLanguageIndentMode(
            int indentKeywordStyle, const std::set<std::string>& indentKeywords,
            int lineEndStyle, const std::set<std::string>& lineEndWords,
            int blockStartStyle, const std::set<std::string>& blockStartWords,
            int blockEndStyle, const std::set<std::string>& blockEndWords);
        void UpdateZoom();
        void AddKeyboardShortcuts();
        void ChangeDefaults();
        void ChangeAllOccurrences();
        std::wstring_view HighlightingLanguage();
        void HighlightingLanguage(std::wstring_view value);
        void ProcessNotification(Scintilla::NotificationData* data);
        std::string GetMainSelectedText(bool expandCaretToWord, Scintilla::sptr_t& start, Scintilla::sptr_t& end);

        Scintilla::ScintillaCall m_call{};
        CodeEditorTheme m_theme{ CodeEditorTheme::Light };

		static constexpr bool _sciIndentOpening{ false };
		static constexpr bool _sciIndentClosing{ false };
		static constexpr int _sciStatementLookback{ 20 };
		double m_dpi_scale;
        bool m_has_empty_selection{ true };
		hstring m_highlighting_language;
		StyleAndWords _sciStatementIndent;
		StyleAndWords _sciStatementEnd;
		StyleAndWords _sciBlockStart;
		StyleAndWords _sciBlockEnd;
		bool SciFindMatchingBracePosition(Scintilla::Position& braceAtCaret, Scintilla::Position& braceOpposite, bool sloppy);
		void SciBraceMatch();
		Scintilla::Line SciGetCurrentLineNumber();
		int SciIndentOfBlock(Scintilla::Line line);
		int SciGetLineIndentation(Scintilla::Line line);
		Scintilla::Position SciGetLineIndentPosition(Scintilla::Line line);
		void SciSetLineIndentation(Scintilla::Line line, int indent);
		Scintilla::Span SciGetSelection();
		std::vector<std::string> SciGetLinePartsInStyle(Scintilla::Line line, const StyleAndWords& saw);
		void SciSetSelection(Scintilla::Position anchor, Scintilla::Position currentPos);
		bool SciRangeIsAllWhitespace(Scintilla::Position start, Scintilla::Position end);
		IndentationStatus SciGetIndentState(Scintilla::Line line);
		void SciAutomaticIndentation(char ch);
		void UpdateLanguageStyles();
    };
}

namespace winrt::MEraEmuWin::DevTools::factory_implementation {
    struct CodeEditControl : CodeEditControlT<CodeEditControl, implementation::CodeEditControl> {};
}
