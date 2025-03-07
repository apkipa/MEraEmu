#include "pch.h"
#include "EngineControl.h"
#if __has_include("EngineControl.g.cpp")
#include "EngineControl.g.cpp"
#endif

#include "EngineUnhandledExceptionEventArgs.g.cpp"
#include "DevTools/MainPage.h"

#define DP_NAMESPACE ::winrt::MEraEmuWin
#define DP_CLASS EngineControl

using namespace winrt;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Media::Imaging;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Core;
using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::System::Threading;

// GETCONFIG
#define ERA_CONFIG_NAME_LINE_HEIGHT "一行の高さ"
#define ERA_CONFIG_NAME_FONT_SIZE "フォントサイズ"
#define ERA_CONFIG_NAME_PRINTC_COUNT_PER_LINE "PRINTCを並べる数"
#define ERA_CONFIG_NAME_PRINTC_CHAR_LENGTH "PRINTCの文字数"
#define ERA_CONFIG_NAME_WINDOW_WIDTH "ウィンドウ幅"
#define ERA_CONFIG_NAME_SAVE_DATA_COUNT "表示するセーブデータ数"
#define ERA_CONFIG_NAME_AUTO_SAVE "オートセーブを行なう"

// GETCONFIGS
#define ERA_CONFIG_NAME_GRAPHICS_INTERFACE "描画インターフェース"

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

static inline std::string to_string(DiagnosticLevel value) {
    switch (value) {
    case DIAGNOSTIC_LEVEL_ERROR: return "错误";
    case DIAGNOSTIC_LEVEL_WARNING: return "警告";
    case DIAGNOSTIC_LEVEL_NOTE: return "提示";
    case DIAGNOSTIC_LEVEL_HELP: return "帮助";
    case DIAGNOSTIC_LEVEL_SUGGESTION: return "建议";
    default: return "<未知>";
    }
}

static inline hstring to_hstring(DiagnosticLevel value) {
    return to_hstring(to_string(value));
}

static inline winrt::Windows::UI::Color color_from_diagnostic_level(DiagnosticLevel level) noexcept {
    switch (level) {
    case DIAGNOSTIC_LEVEL_ERROR: return Colors::Red();
    case DIAGNOSTIC_LEVEL_WARNING: return Colors::Orange();
    case DIAGNOSTIC_LEVEL_NOTE: return Colors::Blue();
    case DIAGNOSTIC_LEVEL_HELP: return Colors::Green();
    case DIAGNOSTIC_LEVEL_SUGGESTION: return Colors::Purple();
    default: return Colors::Black();
    }
}
static inline auto recur_dir_iter(hstring const& base, hstring const& dir) {
    return std::filesystem::recursive_directory_iterator((base + dir).c_str());
}
template<typename T>
static inline bool checked_add(T a, T b, T& out) noexcept {
    constexpr auto MAX = std::numeric_limits<T>::max();
    constexpr auto MIN = std::numeric_limits<T>::min();
    if (a > 0 && b > 0) {
        if (a > MAX - b) { return false; }
    }
    if (a < 0 && b < 0) {
        if (a < MIN - b) { return false; }
    }
    out = a + b;
    return true;
}
template<typename T>
static inline bool checked_mul(T a, T b, T& out) noexcept {
    constexpr auto MAX = std::numeric_limits<T>::max();
    constexpr auto MIN = std::numeric_limits<T>::min();
    if (a == 0 || b == 0) {
        out = 0;
        return true;
    }
    if ((a > 0 && b > 0) || (a < 0 && b < 0)) {
        if (a > MAX / b) { return false; }
    }
    else {
        if (a > b) { std::swap(a, b); }
        if (a < MIN / b) { return false; }
    }
    out = a * b;
    return true;
}
template<typename T>
static inline std::optional<T> checked_add(T a, T b) noexcept {
    T out;
    if (!checked_add(a, b, out)) { return std::nullopt; }
    return out;
}
template<typename T>
static inline std::optional<T> checked_mul(T a, T b) noexcept {
    T out;
    if (!checked_mul(a, b, out)) { return std::nullopt; }
    return out;
}
template<typename T>
static inline std::optional<T> parse(std::wstring_view str) noexcept {
    if (str.empty()) { return std::nullopt; }
    std::wstring_view::size_type n = str.size();
    std::wstring_view::size_type i;
    T r{};
    bool is_negative{};
    for (i = 0; i < n; i++) {
        auto ch = str[i];
        if (i == 0 && ch == L'-') {
            is_negative = true;
            if (str.size() == 1) { return std::nullopt; }
            continue;
        }
        if (!(L'0' <= ch && ch <= L'9')) {
            return std::nullopt;
        }
        if (!checked_mul<T>(r, 10, r)) { return std::nullopt; }
        if (!checked_add<T>(r, ch - L'0', r)) { return std::nullopt; }
    }
    if (i != n) { return std::nullopt; }
    if (is_negative) {
        return checked_mul<T>(r, -1);
    }
    else {
        return r;
    }
}

static inline constexpr uint32_t saturate_to_u32(int32_t v) noexcept {
    return v < 0 ? 0 : v;
}

template <typename T>
struct ValueCache {
    ValueCache() : m_value{ std::nullopt } {}

    void set(T value) {
        m_value = value;
    }
    template <typename F>
    T get_or_update(F&& f) {
        if (!m_value.has_value()) {
            m_value = f();
        }
        return *m_value;
    }
    std::optional<T> get_opt() const {
        return m_value;
    }
    void invalidate() {
        m_value = std::nullopt;
    }
    operator bool() const noexcept {
        return m_value.has_value();
    }

    std::optional<T> m_value;
};

static inline bool key_is_value(auto&& map, auto&& key, auto&& value) {
    auto it = map.find(std::forward<decltype(key)>(key));
    if (it == map.end()) { return false; }
    return it->second == value;
}

static inline std::string_view trim_ascii(std::string_view str) {
    while (!str.empty() && std::isspace(static_cast<unsigned char>(str.front()))) {
        str.remove_prefix(1);
    }
    while (!str.empty() && std::isspace(static_cast<unsigned char>(str.back()))) {
        str.remove_suffix(1);
    }
    return str;
}

static inline std::wstring_view trim_ascii(std::wstring_view str) {
    while (!str.empty() && std::iswspace(str.front())) {
        str.remove_prefix(1);
    }
    while (!str.empty() && std::iswspace(str.back())) {
        str.remove_suffix(1);
    }
    return str;
}

// WARN: ASCII only!
template <typename CharT>
static inline bool sv_equals(std::basic_string_view<CharT> str, std::string_view expected) {
    if (str.size() != expected.size()) { return false; }
    return std::equal(str.begin(), str.end(), expected.begin(), expected.end());
}

template <typename T, typename CharT>
static inline auto parse_comma_separated_length_sv(std::basic_string_view<CharT> str, size_t min_count = 0, size_t max_count = SIZE_MAX) {
    using string_view_type = std::basic_string_view<CharT>;
    std::vector<std::pair<T, string_view_type>> result;
    while (!str.empty()) {
        auto comma_pos = str.find(',');
        if (comma_pos == string_view_type::npos) {
            comma_pos = str.size();
        }
        auto whole_value_str = trim_ascii(str.substr(0, comma_pos));
        //size_t unit_pos = whole_value_str.find_first_not_of("-0123456789");
        size_t unit_pos{};
        for (unit_pos = 0; unit_pos < whole_value_str.size(); unit_pos++) {
            auto ch = whole_value_str[unit_pos];
            if (ch == '-' || ('0' <= ch && ch <= '9')) {
                continue;
            }
            break;
        }
        auto value_str = trim_ascii(whole_value_str.substr(0, unit_pos));
        auto unit_str = trim_ascii(whole_value_str.substr(unit_pos));
        result.push_back({ parse<T>(value_str).value(), unit_str });
        if (comma_pos == str.size()) { break; }
        str.remove_prefix(comma_pos + 1);
    }
    if (result.size() < min_count || result.size() > max_count) {
        char buf[256];
        snprintf(buf, sizeof buf, "Invalid count of comma-separated values: %zu not in [%zu, %zu]",
            result.size(), min_count, max_count);
        throw std::runtime_error(buf);
    }
    return result;
}

template <typename T>
static inline auto parse_comma_separated_length(auto&& in_str, size_t min_count = 0, size_t max_count = SIZE_MAX) {
    std::basic_string_view str(in_str);
    using string_view_type = decltype(str);
    return parse_comma_separated_length_sv<T>(str, min_count, max_count);
}

template <typename CharT>
struct SimpleHtmlParser {
    using string_type = std::basic_string<CharT>;
    using string_view_type = std::basic_string_view<CharT>;

    struct Tag {
        string_type name;
        std::map<string_type, string_type, std::less<>> attrs;
        // NOTE: -1 means not exists or not yet calculated
        std::pair<size_t, size_t> start_tag_pos_range{ SIZE_MAX, SIZE_MAX }, end_tag_pos_range{ SIZE_MAX, SIZE_MAX };

        bool is_start_set() const noexcept {
            return start_tag_pos_range.first != SIZE_MAX;
        }
        bool is_end_set() const noexcept {
            return end_tag_pos_range.first != SIZE_MAX;
        }
        bool is_self_closing() const noexcept {
            if (!is_start_set() || !is_end_set()) { return false; }
            return start_tag_pos_range == end_tag_pos_range;
        }
        bool is_void_tag() const noexcept {
            auto test = [&](std::string_view expected) {
                if (name.size() != expected.size()) { return false; }
                return sv_starts_with(name, expected);
            };
            return test("br") || test("img") || test("shape");
        }
    };
    enum class VisitEventKind {
        EnterTag,
        LeaveTag,
        OnText,
        ImplicitlyLeaveTag,   // For self-closing tags or tags allowing omitting the end tag
    };

    SimpleHtmlParser(string_view_type str) : m_orig_str(str), m_str(str) {}

    auto const& current_tags() const noexcept {
        return m_active_tags;
    }

    void reset() noexcept {
        m_str = m_orig_str;
        m_active_tags.clear();
    }

    void check_syntax() {
        parse([](auto&&...) {});
    }

    // F: fn(VistEventKind, &Tag, &Text)
    template <typename F>
    void parse(F&& visitor) {
        while (true) {
            auto token = next_token(false);
            switch (token) {
            case Token::Eof:
                goto done;
            case Token::TagStart: {
                m_active_tags.push_back(parse_tag(false));
                auto& tag = m_active_tags.back();
                visitor(VisitEventKind::EnterTag, tag, string_type{});
                if (tag.is_self_closing() || tag.is_void_tag()) {
                    tag.end_tag_pos_range = tag.start_tag_pos_range;
                    auto last_tag = std::move(m_active_tags.back());
                    m_active_tags.pop_back();
                    visitor(VisitEventKind::ImplicitlyLeaveTag, last_tag, string_type{});
                }
                break;
            }
            case Token::TagCloseStart: {
                auto tag = parse_tag(true);
                bool handled{};
                while (!m_active_tags.empty()) {
                    auto last_tag = std::move(m_active_tags.back());
                    m_active_tags.pop_back();
                    if (last_tag.name == tag.name) {
                        visitor(VisitEventKind::LeaveTag, last_tag, string_type{});
                        handled = true;
                        break;
                    }
                    visitor(VisitEventKind::ImplicitlyLeaveTag, last_tag, string_type{});
                }
                if (!handled) {
                    if (!tolerate_errors) {
                        throw std::runtime_error("Invalid tag");
                    }
                }
                break;
            }
            case Token::Text: {
                auto tag = Tag{};
                auto span = current_token_pos_range();
                auto str = unescape_string(m_orig_str.substr(span.first, span.second - span.first));
                visitor(VisitEventKind::OnText, tag, std::move(str));
                break;
            }
            default:
                throw std::runtime_error("Invalid tag");
            }
        }
    done:
        // Close all tags
        while (!m_active_tags.empty()) {
            auto tag = std::move(m_active_tags.back());
            m_active_tags.pop_back();
            visitor(VisitEventKind::ImplicitlyLeaveTag, tag, string_type{});
        }
    }

private:
    enum class Token {
        Invalid,
        Eof,
        TagStart,
        TagEnd,
        TagSelfCloseEnd,
        TagCloseStart,
        Equal,
        StringLiteral,
        Text,
    };

    /// Reads the next token from the input. `inside_tag` is true if the parser is currently inside a tag.
    /// For example, with input "<p ^align='left'>content</p>", where ^ is the current position, `inside_tag`
    /// is true; with input "<p align='left'>^content</p>", `inside_tag` is false.
    Token next_token(bool inside_tag) {
        if (inside_tag) {
            // Skip whitespace; it's not significant inside a tag.
            trim_start();
        }

        m_token_start_pos = current_pos();
        if (m_str.empty()) { return Token::Eof; }

        if (inside_tag) {
            // Inside either start tag or end tag. Only parse `attr='value'` or `>` or `/>`.
            if (starts_with("/>")) {
                m_str.remove_prefix(2);
                return Token::TagSelfCloseEnd;
            }
            if (starts_with(">")) {
                m_str.remove_prefix(1);
                return Token::TagEnd;
            }
            if (starts_with("=")) {
                m_str.remove_prefix(1);
                return Token::Equal;
            }
            if (starts_with("\"") || starts_with("'")) {
                auto quote = m_str[0];
                m_str.remove_prefix(1);
                auto end = m_str.find(quote);
                if (end == string_view_type::npos) {
                    m_str = {};
                    return Token::Invalid;
                }
                m_str.remove_prefix(end + 1);
                return Token::StringLiteral;
            }
            if (!is_ident_char(m_str[0])) {
                m_str.remove_prefix(1);
                return Token::Invalid;
            }
            while (!m_str.empty() && is_ident_char(m_str[0])) {
                m_str.remove_prefix(1);
            }
            return Token::Text;
        }
        else {
            // Outside tag. Parse `<`, `</`, or text.
            if (starts_with("</")) {
                m_str.remove_prefix(2);
                return Token::TagCloseStart;
            }
            if (starts_with("<")) {
                m_str.remove_prefix(1);
                return Token::TagStart;
            }
            auto end = m_str.find('<');
            if (end == string_view_type::npos) {
                m_str = {};
                return Token::Text;
            }
            m_str.remove_prefix(end);
            return Token::Text;
        }
    }

    Token peek_token(bool inside_tag) {
        auto orig_str = m_str;
        auto orig_token_start_pos = m_token_start_pos;
        auto token = next_token(inside_tag);
        m_str = orig_str;
        m_token_start_pos = orig_token_start_pos;
        return token;
    }

    // Parses the residual of `<tag ...>` or `<tag .../>` or `</tag>`.
    Tag parse_tag(bool is_closing_start) {
        Tag tag;

        // NOTE: We assign to start_tag_pos_range regardless of whether it is a start tag or end tag.
        tag.start_tag_pos_range.first = m_token_start_pos;

        // Parse tag name
        if (next_token(true) != Token::Text) {
            throw std::runtime_error("Invalid tag");
        }
        {
            auto name_span = current_token_pos_range();
            tag.name = m_orig_str.substr(name_span.first, name_span.second - name_span.first);
        }

        bool is_self_closing = false;

        if (is_closing_start) {
            // End tag
            if (next_token(true) != Token::TagEnd) {
                throw std::runtime_error("Invalid tag");
            }
        }
        else {
            // Parse attributes
            // TODO: Support parsing valueless / duplicate attributes
            while (true) {
                auto token = next_token(true);
                if (token == Token::TagSelfCloseEnd) {
                    is_self_closing = true;
                    break;
                }
                if (token == Token::TagEnd) {
                    break;
                }
                if (token != Token::Text) {
                    throw std::runtime_error("Invalid tag");
                }
                auto name_span = current_token_pos_range();
                string_type name{ m_orig_str.substr(name_span.first, name_span.second - name_span.first) };
                if (next_token(true) != Token::Equal) {
                    throw std::runtime_error("Invalid tag");
                }
                if (next_token(true) != Token::StringLiteral) {
                    throw std::runtime_error("Invalid tag");
                }
                auto value_span = current_token_pos_range();
                value_span.first++;
                value_span.second--;
                string_type value{ unescape_string(m_orig_str.substr(value_span.first, value_span.second - value_span.first)) };
                if (!tag.attrs.emplace(std::move(name), std::move(value)).second) {
                    throw std::runtime_error("Duplicate attribute");
                }
            }
        }

        tag.start_tag_pos_range.second = current_pos();
        if (is_self_closing) {
            tag.end_tag_pos_range = tag.start_tag_pos_range;
        }

        return tag;
    }

    static string_type unescape_string(string_view_type str) {
        string_type result;
        result.reserve(str.size());
        auto push_unicode_to_result = [&result](uint32_t code) {
            if constexpr (sizeof(CharT) == 1) {
                // Assume UTF-8
                if (code <= 0x7F) {
                    result.push_back((CharT)code);
                }
                else if (code <= 0x7FF) {
                    result.push_back((CharT)(0xC0 | (code >> 6)));
                    result.push_back((CharT)(0x80 | (code & 0x3F)));
                }
                else if (code <= 0xFFFF) {
                    result.push_back((CharT)(0xE0 | (code >> 12)));
                    result.push_back((CharT)(0x80 | ((code >> 6) & 0x3F)));
                    result.push_back((CharT)(0x80 | (code & 0x3F)));
                }
                else if (code <= 0x10FFFF) {
                    result.push_back((CharT)(0xF0 | (code >> 18)));
                    result.push_back((CharT)(0x80 | ((code >> 12) & 0x3F)));
                    result.push_back((CharT)(0x80 | ((code >> 6) & 0x3F)));
                    result.push_back((CharT)(0x80 | (code & 0x3F)));
                }
                else {
                    return false;
                }
            }
            else if constexpr (sizeof(CharT) == 2) {
                // Assume UTF-16
                if (code <= 0xFFFF) {
                    result.push_back((CharT)code);
                }
                else if (code <= 0x10FFFF) {
                    code -= 0x10000;
                    result.push_back((CharT)(0xD800 | (code >> 10)));
                    result.push_back((CharT)(0xDC00 | (code & 0x3FF)));
                }
                else {
                    return false;
                }
            }
            else {
                // Assume UTF-32
                result.push_back((CharT)code);
            }
            return true;
        };
        for (size_t i = 0; i < str.size(); i++) {
            if (str[i] == '&') {
                if (sv_starts_with(str.substr(i), "&lt;")) {
                    result.push_back('<');
                    i += 3;
                }
                else if (sv_starts_with(str.substr(i), "&gt;")) {
                    result.push_back('>');
                    i += 3;
                }
                else if (sv_starts_with(str.substr(i), "&amp;")) {
                    result.push_back('&');
                    i += 4;
                }
                else if (sv_starts_with(str.substr(i), "&quot;")) {
                    result.push_back('"');
                    i += 5;
                }
                else if (sv_starts_with(str.substr(i), "&apos;")) {
                    result.push_back('\'');
                    i += 5;
                }
                else if (sv_starts_with(str.substr(i), "&#x")) {
                    auto end = str.find(';', i + 3);
                    if (end != string_view_type::npos) {
                        auto hex_str = str.substr(i + 3, end - i - 3);
                        try {
                            auto code = std::stoul(string_type(hex_str), nullptr, 16);
                            if (push_unicode_to_result((uint32_t)code)) {
                                i = end;
                            }
                            else {
                                result.push_back(str[i]);
                            }
                        }
                        catch (...) {
                            result.push_back(str[i]);
                        }
                    }
                    else {
                        result.push_back(str[i]);
                    }
                }
                else if (sv_starts_with(str.substr(i), "&#")) {
                    auto end = str.find(';', i + 2);
                    if (end != string_view_type::npos) {
                        auto dec_str = str.substr(i + 2, end - i - 2);
                        try {
                            auto code = std::stoul(string_type(dec_str), nullptr, 10);
                            if (push_unicode_to_result((uint32_t)code)) {
                                i = end;
                            }
                            else {
                                result.push_back(str[i]);
                            }
                        }
                        catch (...) {
                            result.push_back(str[i]);
                        }
                    }
                    else {
                        result.push_back(str[i]);
                    }
                }
                else {
                    result.push_back(str[i]);
                }
            }
            else {
                result.push_back(str[i]);
            }
        }
        return result;
    }

    size_t current_pos() const noexcept {
        return m_orig_str.size() - m_str.size();
    }

    std::pair<size_t, size_t> current_token_pos_range() const noexcept {
        return { m_token_start_pos, current_pos() };
    }

    // WARN: ASCII only!
    bool starts_with(std::string_view prefix) const noexcept {
        return sv_starts_with(m_str, prefix);
    }

    // WARN: ASCII only!
    static bool sv_starts_with(string_view_type sv, std::string_view prefix) noexcept {
        if (sv.size() < prefix.size()) { return false; }
        return std::equal(prefix.begin(), prefix.end(), sv.begin());
    }

    void trim_start() {
        while (!m_str.empty() && (uint32_t)m_str[0] <= 127 && std::isspace(m_str[0])) {
            m_str.remove_prefix(1);
        }
    }

    static bool is_ident_char(CharT ch) {
        return (uint32_t)ch > 127 || std::isalnum(ch) || ch == '_';
    }

    const string_view_type m_orig_str;
    bool tolerate_errors{ true };
    string_view_type m_str;
    size_t m_token_start_pos{};
    std::vector<Tag> m_active_tags; // Tags stack
};

namespace winrt::MEraEmuWin::implementation {
    static size_t width(std::wstring_view str) noexcept {
        return rust_get_wstring_view_width({ (const uint16_t*)str.data(), str.size() });
    }
    static size_t width(hstring const& str) noexcept {
        return rust_get_wstring_width((const uint16_t*)str.c_str());
    }
    static size_t width(std::string const& str) noexcept {
        return rust_get_string_width(str.c_str());
    }
    static size_t width(const char* str) noexcept {
        return rust_get_string_width(str);
    }

    template <typename CharT>
    static inline auto parse_comma_separated_ui_length_sv(std::basic_string_view<CharT> str, size_t min_count = 0, size_t max_count = SIZE_MAX) {
        using Type = EngineUILength::Type;
        std::vector<EngineUILength> result;
        for (auto&& [value, unit] : parse_comma_separated_length_sv<int32_t>(str, min_count, max_count)) {
            if (sv_equals(unit, "px")) {
                result.push_back({ value, Type::Pixel });
            }
            else if (sv_equals(unit, "")) {
                result.push_back({ value, Type::FontPercent });
            }
            else {
                throw std::runtime_error("Invalid unit");
            }
        }
        return result;
    }

    static inline auto parse_comma_separated_ui_length(auto&& in_str, size_t min_count = 0, size_t max_count = SIZE_MAX) {
        std::basic_string_view str(in_str);
        using string_view_type = decltype(str);
        return parse_comma_separated_ui_length_sv(str, min_count, max_count);
    }

    enum class FileBomKind {
        None,
        Utf8,
        Utf16LE,
        Utf16BE,
    };
    struct FileBomDetectResult {
        FileBomKind kind;
        uint32_t read_size;
        uint32_t bom_size;
    };
    FileBomDetectResult consume_detect_file_bom(HANDLE hFile, uint8_t buf[4]) {
        DWORD read_size;
        check_bool(ReadFile(hFile, buf, 3, &read_size, nullptr));
        if (read_size >= 3) {
            if (buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf) {
                return { FileBomKind::Utf8, read_size, 3 };
            }
        }
        if (read_size >= 2) {
            if (buf[0] == 0xff && buf[1] == 0xfe) {
                return { FileBomKind::Utf16LE, read_size, 2 };
            }
            if (buf[0] == 0xfe && buf[1] == 0xff) {
                return { FileBomKind::Utf16BE, read_size, 2 };
            }
        }
        return { FileBomKind::None, read_size, 0 };
    }
    std::pair<std::unique_ptr<uint8_t[]>, size_t> read_utf8_file(std::filesystem::path const& path) {
        winrt::file_handle file{ CreateFileW(path.c_str(),
            GENERIC_READ,
            FILE_SHARE_READ,
            nullptr,
            OPEN_EXISTING,
            0,
            nullptr
        ) };
        try { check_bool(static_cast<bool>(file)); }
        catch (hresult_error const& e) {
            throw hresult_error(e.code(), format(L"Failed to open `{}`: {}", path.c_str(), e.message()));
        }
        DWORD high_size{};
        auto size = GetFileSize(file.get(), &high_size);
        if (high_size != 0) {
            throw hresult_error(E_FAIL, L"file is too large");
        }
        auto buf = new uint8_t[size + 1];
        tenkai::cpp_utils::ScopeExit se_buf([&] {
            delete[] buf;
        });
        DWORD read_size{};
        size_t buf_offset{};
        FileBomKind bom_kind;
        {
            auto [temp_bom_kind, read_size, bom_size] = consume_detect_file_bom(file.get(), buf);
            auto temp_payload_size = read_size - bom_size;
            memcpy(buf, buf + bom_size, temp_payload_size);
            buf_offset += temp_payload_size;
            size -= read_size;
            bom_kind = temp_bom_kind;
        }
        auto read_rest_fn = [&] {
            check_bool(ReadFile(file.get(), buf + buf_offset, size, &read_size, nullptr));
            if (read_size != size) {
                throw hresult_error(E_FAIL, L"Failed to read the whole file");
            }
            buf_offset += size;
            size = 0;
        };
        if (bom_kind == FileBomKind::Utf8 || bom_kind == FileBomKind::None) {
            // Read directly
            read_rest_fn();
        }
        else if (bom_kind == FileBomKind::Utf16LE) {
            // Convert to UTF-8
            //OutputDebugStringW(winrt::format(L"Converting UTF-16 LE file `{}` to UTF-8...\n", path.c_str()).c_str());
            read_rest_fn();
            auto utf16_src = std::wstring_view((wchar_t*)buf, buf_offset / sizeof(wchar_t));
            auto utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_src.data(), static_cast<int32_t>(utf16_src.size()), nullptr, 0, nullptr, nullptr);
            if (utf8_size == 0) {
                throw hresult_error(E_FAIL, L"Failed to convert UTF-16 LE to UTF-8");
            }
            auto utf8_buf = std::unique_ptr<uint8_t[]>(new uint8_t[utf8_size + 1]);
            auto final_utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_src.data(), static_cast<int32_t>(utf16_src.size()), (char*)utf8_buf.get(), utf8_size, nullptr, nullptr);
            if (final_utf8_size != utf8_size) {
                throw hresult_error(E_FAIL, L"Failed to convert UTF-16 LE to UTF-8");
            }
            delete[] std::exchange(buf, utf8_buf.release());
            buf_offset = final_utf8_size;
        }
        else if (bom_kind == FileBomKind::Utf16BE) {
            throw hresult_error(E_FAIL, L"UTF-16 BE is not supported");
        }
        else {
            throw hresult_error(E_FAIL, L"Unknown BOM kind");
        }

        se_buf.release();
        return { std::unique_ptr<uint8_t[]>{ buf }, buf_offset };
    }
    constexpr uint32_t to_u32(winrt::Windows::UI::Color value) noexcept {
        uint32_t a = value.A, r = value.R, g = value.G, b = value.B;
        return (a << 24) + (r << 16) + (g << 8) + (b << 0);
    }
    constexpr winrt::Windows::UI::Color to_winrt_color(uint32_t value) noexcept {
        return {
            .A = static_cast<uint8_t>(value >> 24),
            .R = static_cast<uint8_t>(value >> 16),
            .G = static_cast<uint8_t>(value >> 8),
            .B = static_cast<uint8_t>(value >> 0),
        };
    }
    constexpr wchar_t ascii_tolower(wchar_t ch) noexcept {
        if (L'A' <= ch && ch <= L'Z') {
            return ch - 'A' + 'a';
        }
        return ch;
    }
    constexpr bool istarts_with(std::wstring_view str, std::wstring_view pat) noexcept {
        if (size(str) < size(pat)) { return false; }
        std::wstring_view::size_type j{};
        for (std::wstring_view::size_type i{}; i != size(pat); i++) {
            if (ascii_tolower(str[i]) != ascii_tolower(pat[i])) {
                return false;
            }
        }
        return true;
    }
    constexpr bool iends_with(std::wstring_view str, std::wstring_view pat) noexcept {
        if (size(str) < size(pat)) { return false; }
        std::wstring_view::size_type j{};
        for (auto i = size(str) - size(pat); i != size(str); i++, j++) {
            if (ascii_tolower(str[i]) != ascii_tolower(pat[j])) {
                return false;
            }
        }
        return true;
    }
    constexpr bool ieq(std::wstring_view a, std::wstring_view b) noexcept {
        if (size(a) != size(b)) { return false; }
        for (std::wstring_view::size_type i{}; i != size(a); i++) {
            if (ascii_tolower(a[i]) != ascii_tolower(b[i])) {
                return false;
            }
        }
        return true;
    }
}

namespace winrt::MEraEmuWin::implementation {
    com_ptr<IDWriteFactory2> g_dwrite_factory;
    com_ptr<IWICImagingFactory> g_wic_factory;
    void ensure_global_factory() {
        if (g_dwrite_factory) { return; }
        {
            decltype(g_dwrite_factory) factory;
            check_hresult(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED,
                guid_of<decltype(*factory)>(),
                reinterpret_cast<::IUnknown**>(factory.put())
            ));
            g_dwrite_factory = std::move(factory);
        }
        {
            g_wic_factory = create_instance<IWICImagingFactory>(CLSID_WICImagingFactory);
        }
    }

    // NOTE: If InputRequest is dropped before fulfilled, it will be treated as interrupted.
    //       To convey the skip operation, call `try_fulfill_void` instead.
    struct InputRequest {
        //InputRequest(int64_t time_limit, bool show_time_prompt, hstring expiry_msg, bool is_one, bool can_skip) : {}
        InputRequest(int64_t& time_limit) : time_limit(time_limit) {}
        virtual ~InputRequest() {}

        // NOTE: For derived requests, hold a std::promise so that engine
        //       can receive the result or teardown notification.

        // Shared parameters
        // NOTE: This is also used by UI to indicate skipped ticks
        int64_t& time_limit; // Negatives stand for no limit
        bool show_time_prompt{ false };
        hstring expiry_msg{};
        bool show_expiry_msg{ false };
        bool is_one{ false };
        bool can_skip{ false };
        bool break_user_skip{ false };
        bool can_click{ false };

        //virtual void time_tick() = 0;
        virtual bool try_fulfill(hstring const& input) = 0;
        // NOTE: Recommended to override this for proper semantics (don't handle skip as empty input!)
        virtual bool try_fulfill_void() {
            return try_fulfill({});
        }
    };
    struct InputRequestI : InputRequest {
        std::optional<int64_t> default_value;
        std::promise<int64_t> promise;

        using InputRequest::InputRequest;

        ~InputRequestI() {
            /*if (default_value) {
                promise.set_value(*default_value);
            }*/
        }
        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                default_value = std::nullopt;
                return true;
            }
            if (auto r = parse<int64_t>(input)) {
                promise.set_value(*r);
                default_value = std::nullopt;
                return true;
            }
            return false;
        }
    };
    struct InputRequestS : InputRequest {
        std::optional<hstring> default_value;
        std::promise<hstring> promise;

        using InputRequest::InputRequest;

        ~InputRequestS() {
            /*if (default_value) {
                promise.set_value(*default_value);
            }*/
        }
        bool try_fulfill(hstring const& input) override {
            if (input.empty() && default_value) {
                promise.set_value(*default_value);
                default_value = std::nullopt;
                return true;
            }
            promise.set_value(input);
            default_value = std::nullopt;
            return true;
        }
    };
    // Input request which swallows any input; can be used for merely waiting
    struct InputRequestVoid : InputRequest {
        std::promise<void> promise{};

        using InputRequest::InputRequest;

        ~InputRequestVoid() {
            // TODO: Don't fulfill if interrupted, use ControlFlow::Break(())
            promise.set_value();
        }
        bool try_fulfill(hstring const& input) override {
            return true;
        }
    };

    struct EngineThreadTask {
        EngineThreadTask(EngineThreadTaskKind kind) : kind(kind) {}
        EngineThreadTask(EngineThreadTaskKind kind, std::move_only_function<void()> f) : kind(kind), f(std::move(f)) {}
        EngineThreadTask(std::move_only_function<void()> f, bool clear_halt = false) :
            kind(clear_halt ? EngineThreadTaskKind::CustomFuncAndClearHaltState : EngineThreadTaskKind::CustomFunc),
            f(std::move(f)) {
        }
        //virtual ~EngineThreadTask() {}

        EngineThreadTaskKind kind;
        std::move_only_function<void()> f;
    };

    struct EngineSharedData : std::enable_shared_from_this<EngineSharedData> {
        EngineSharedData() {}
        ~EngineSharedData() {}

        hstring game_base_dir;
        //CoreDispatcher ui_dispatcher{ nullptr };
        DispatcherQueue ui_dispatcher_queue{ nullptr };
        //weak_ref<EngineControl> ui_ctrl{ nullptr };
        EngineControl* ui_ctrl{};
        IAsyncAction thread_task_op{ nullptr };
        std::atomic_bool ui_is_alive{ false };
        std::atomic_bool ui_redraw_block_engine{ false };
        std::atomic_bool ui_queue_work_debounce{ false };
        std::atomic_bool thread_started{ false };
        std::atomic_bool thread_is_alive{ false };
        std::exception_ptr thread_exception{ nullptr };
        bool has_execution_error{ false };
        MEraEngine engine{ nullptr };
        com_ptr<implementation::AppSettingsVM> app_settings{};
    };

    struct MEraEmuWinEngineSysCallback : ::MEraEngineSysCallback {
        // SAFETY: Engine is destructed before EngineSharedData
        MEraEmuWinEngineSysCallback(EngineSharedData* sd, util::sync::spsc::Sender<std::move_only_function<void()>> const* ui_task_tx) :
            m_sd(sd), m_ui_task_tx(ui_task_tx) {
        }

        void on_error(EraDiagnosticProvider const& provider) override {
            auto count = provider.get_entry_count();
            for (uint64_t i = 0; i < count; i++) {
                auto entry = std::move(provider.get_entry(i).value());
                auto level = entry.level;
                auto filename = to_hstring(entry.filename);
                auto span = entry.span;
                auto message = to_hstring(entry.message);
                auto msg_clr = color_from_diagnostic_level(level);
                if (level == DIAGNOSTIC_LEVEL_ERROR) {
                    m_sd->has_execution_error = true;
                }
                if (auto resolved_opt = provider.resolve_src_span(entry.filename, span)) {
                    auto& resolved = *resolved_opt;
                    // Convert 0-based to 1-based
                    resolved.loc.col += 1;

                    auto final_msg = format(L"{}({},{}): {}: {}\nSnippet: {}",
                        filename,
                        resolved.loc.line, resolved.loc.col,
                        to_hstring(level),
                        message,
                        to_hstring(resolved.snippet)
                    );
                    queue_ui_work([=, sd = m_sd] {
                        auto old_clr = sd->ui_ctrl->EngineForeColor();
                        sd->ui_ctrl->EngineForeColor(msg_clr);
                        sd->ui_ctrl->RoutinePrintSourceButton(
                            final_msg, filename,
                            resolved.loc.line, resolved.loc.col,
                            ERA_PEF_IS_LINE
                        );
                        sd->ui_ctrl->EngineForeColor(old_clr);
                    });
                }
                else {
                    auto final_msg = format(L"{}(<failed to resolve SrcSpan({}, {})>): {}: {}",
                        filename,
                        span.start, span.len,
                        to_hstring(level),
                        message
                    );
                    queue_ui_work([=, sd = m_sd] {
                        auto old_clr = sd->ui_ctrl->EngineForeColor();
                        sd->ui_ctrl->EngineForeColor(msg_clr);
                        sd->ui_ctrl->RoutinePrintSourceButton(
                            final_msg, filename,
                            1, 1,
                            ERA_PEF_IS_LINE
                        );
                        sd->ui_ctrl->EngineForeColor(old_clr);
                    });
                }
            }
        }
        uint64_t on_get_rand() override {
            return m_rand_gen();
        }
        void on_print(std::string_view content, PrintExtendedFlags flags) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content), flags] {
                sd->ui_ctrl->RoutinePrint(content, flags);
            });
            if (flags & ERA_PEF_IS_WAIT) {
                on_wait(true, false);
            }
        }
        void on_html_print(std::string_view content, int64_t no_single) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content), no_single] {
                sd->ui_ctrl->RoutineHtmlPrint(content, no_single);
            });
        }
        const char* on_html_popprintingstr() override {
            m_str_cache = exec_ui_work_or([sd = m_sd] {
                return to_string(sd->ui_ctrl->RoutineHtmlPopPrintingStr());
            });
            return m_str_cache.c_str();
        }
        const char* on_html_getprintedstr(int64_t line_no) override {
            m_str_cache = exec_ui_work_or([sd = m_sd, line_no] {
                return to_string(sd->ui_ctrl->RoutineHtmlGetPrintedStr(line_no));
            });
            return m_str_cache.c_str();
        }
        int64_t on_html_stringlen(std::string_view content, bool return_pixel) override {
            return exec_ui_work_or([sd = m_sd, content = to_hstring(content), return_pixel] {
                return sd->ui_ctrl->RoutineHtmlStringLen(content, return_pixel);
            });
        }
        void on_wait(bool any_key, bool is_force) override {
            int64_t time_limit{ -1 };
            auto input_req = std::make_unique<InputRequestVoid>(time_limit);
            input_req->can_skip = true;
            input_req->break_user_skip = is_force;
            input_req->can_click = true;
            auto future = input_req->promise.get_future();
            queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                sd->ui_ctrl->RoutineInput(std::move(input_req));
            });
            return future.get();
        }
        void on_twait(int64_t duration, bool is_force) override {
            if (duration > 0) {
                if (duration <= m_tick_compensation) {
                    m_tick_compensation -= duration;
                    duration = 0;
                }
                else {
                    duration -= m_tick_compensation;
                    m_tick_compensation = 0;
                }
            }
            auto input_req = std::make_unique<InputRequestVoid>(duration);
            tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                if (duration < 0) {
                    m_tick_compensation = -duration;
                }
            });
            input_req->can_skip = !is_force;
            input_req->break_user_skip = true;
            auto future = input_req->promise.get_future();
            queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                sd->ui_ctrl->RoutineInput(std::move(input_req));
            });
            return future.get();
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_input_int(std::optional<int64_t> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestI>(time_limit);
                input_req->default_value = default_value;
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return { continue_tag, future.get() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, const char*> on_input_str(std::optional<std::string_view> default_value, bool can_click, bool allow_skip) override {
            m_tick_compensation = 0;
            int64_t time_limit{ -1 };
            try {
                auto input_req = std::make_unique<InputRequestS>(time_limit);
                input_req->default_value = default_value.transform([](auto x) { return to_hstring(x); });
                input_req->can_skip = allow_skip;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return { continue_tag, m_str_cache.c_str() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_tinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            try {
                if (time_limit > 0) {
                    if (time_limit <= m_tick_compensation) {
                        m_tick_compensation -= time_limit;
                        time_limit = 0;
                    }
                    else {
                        time_limit -= m_tick_compensation;
                        m_tick_compensation = 0;
                    }
                }
                auto input_req = std::make_unique<InputRequestI>(time_limit);
                tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                    if (time_limit < 0) {
                        m_tick_compensation = -time_limit;
                    }
                });
                input_req->default_value = default_value;
                input_req->show_time_prompt = show_prompt;
                input_req->expiry_msg = expiry_msg.empty() ?
                    to_hstring(default_value) :
                    winrt::format(L"{}\n{}", to_hstring(expiry_msg), to_hstring(default_value));
                input_req->show_expiry_msg = true;
                input_req->can_skip = can_click;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                return { continue_tag, future.get() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, const char*> on_tinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            try {
                if (time_limit > 0) {
                    if (time_limit <= m_tick_compensation) {
                        m_tick_compensation -= time_limit;
                        time_limit = 0;
                    }
                    else {
                        time_limit -= m_tick_compensation;
                        m_tick_compensation = 0;
                    }
                }
                auto input_req = std::make_unique<InputRequestS>(time_limit);
                tenkai::cpp_utils::ScopeExit se_time_limit([&] {
                    if (time_limit < 0) {
                        m_tick_compensation = -time_limit;
                    }
                });
                input_req->default_value = to_hstring(default_value);
                input_req->show_time_prompt = show_prompt;
                input_req->expiry_msg = expiry_msg.empty() ?
                    to_hstring(default_value) :
                    winrt::format(L"{}\n{}", to_hstring(expiry_msg), to_hstring(default_value));
                input_req->show_expiry_msg = true;
                input_req->can_skip = can_click;
                input_req->break_user_skip = true;
                input_req->can_click = can_click;
                auto future = input_req->promise.get_future();
                queue_ui_work([sd = m_sd, input_req = std::move(input_req)]() mutable {
                    sd->ui_ctrl->RoutineInput(std::move(input_req));
                });
                m_str_cache = to_string(future.get());
                return { continue_tag, m_str_cache.c_str() };
            }
            catch (std::future_error const& e) { return { break_tag, std::monostate{} }; }
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_oneinput_int(std::optional<int64_t> default_value) override {
            // TODO: on_oneinput_int
            return { continue_tag, std::optional<int64_t>() };
        }
        ControlFlow<std::monostate, const char*> on_oneinput_str(std::optional<std::string_view> default_value) override {
            // TODO: on_oneinput_str
            return { continue_tag, nullptr };
        }
        ControlFlow<std::monostate, std::optional<int64_t>> on_toneinput_int(int64_t time_limit, int64_t default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            // TODO: on_toneinput_int
            return { continue_tag, std::optional<int64_t>() };
        }
        ControlFlow<std::monostate, const char*> on_toneinput_str(int64_t time_limit, std::string_view default_value, bool show_prompt, std::string_view expiry_msg, bool can_click) override {
            // TODO: on_toneinput_str
            return { continue_tag, nullptr };
        }
        void on_reuselastline(std::string_view content) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content)] {
                sd->ui_ctrl->RoutineReuseLastLine(content);
            });
        }
        void on_clearline(int64_t count) override {
            if (count <= 0) { return; }
            queue_ui_work([sd = m_sd, count = (uint64_t)count] {
                sd->ui_ctrl->RoutineClearLine(count);
            });
        }
        int64_t on_var_get_int(std::string_view name, size_t idx) override {
            if (name == "@COLOR") {
                return m_ui_cache.fore_color.get_or_update([&] {
                    return exec_ui_work_or([sd = m_sd] {
                        return to_u32(sd->ui_ctrl->EngineForeColor()) & ~0xff000000;
                    });
                });
            }
            if (name == "@DEFCOLOR") {
                return to_u32(m_sd->app_settings->GameForegroundColor());
            }
            if (name == "@BGCOLOR") {
                return m_ui_cache.back_color.get_or_update([&] {
                    return exec_ui_work_or([sd = m_sd] {
                        return to_u32(sd->ui_ctrl->EngineBackColor()) & ~0xff000000;
                    });
                });
            }
            if (name == "@DEFBGCOLOR") {
                return to_u32(m_sd->app_settings->GameBackgroundColor());
            }
            if (name == "@FOCUSCOLOR") {
                return to_u32(m_sd->app_settings->GameHighlightColor());
            }
            if (name == "@STYLE") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->GetCurrentFontStyle();
                });
            }
            if (name == "@REDRAW") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->GetRedrawState();
                });
            }
            if (name == "@ALIGN") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->GetCurrentLineAlignment();
                });
            }
            if (name == "@TOOLTIP_DELAY") {
                // TODO: @TOOLTIP_DELAY
                return 0;
            }
            if (name == "@TOOLTIP_DURATION") {
                // TODO: @TOOLTIP_DURATION
                return 0;
            }
            if (name == "@SKIPDISP") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->GetSkipDisplay();
                });
            }
            if (name == "@MESSKIP") {
                // TODO: @MESSKIP
                return 0;
            }
            if (name == "@ANIMETIMER") {
                // TODO: @ANIMETIMER
                return 0;
            }
            if (name == "@PRINTCPERLINE") {
                return m_sd->app_settings->GamePrintCCountPerLine();
            }
            if (name == "@PRINTCLENGTH") {
                return m_sd->app_settings->GamePrintCCharCount();
            }
            if (name == "@LINEISEMPTY") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->m_cur_composing_line.parts.empty();
                });
            }
            if (name == "@CLIENTCHARWIDTH") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->m_ui_param_cache.line_char_capacity;
                });
            }
            if (name == "@CLIENTWIDTH") {
                // NOTE: Returns width in logical pixels
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->m_ui_param_cache.canvas_width_px / sd->ui_ctrl->m_ui_param_cache.ui_scale;
                });
            }
            if (name == "LINECOUNT") {
                return exec_ui_work_or([sd = m_sd] {
                    return sd->ui_ctrl->GetCurrentUILinesCount();
                });
            }
            std::string msg = "no such variable: ";
            msg += name;
            throw std::runtime_error(msg);
        }
        const char* on_var_get_str(std::string_view name, size_t idx) override {
            if (name == "@FONT") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->GetCurrentFontName());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "WINDOW_TITLE") {
                std::promise<hstring> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    promise.set_value(sd->ui_ctrl->EngineTitle());
                });
                m_str_cache = to_string(future.get());
                return m_str_cache.c_str();
            }
            if (name == "DRAWLINESTR") {
                std::promise<std::string> promise;
                auto future = promise.get_future();
                queue_ui_work([sd = m_sd, promise = std::move(promise)]() mutable {
                    auto screen_width = sd->ui_ctrl->m_ui_param_cache.line_char_capacity;
                    promise.set_value(std::string(screen_width, '-'));
                });
                m_str_cache = future.get();
                return m_str_cache.c_str();
            }
            std::string msg = "no such variable: ";
            msg += name;
            throw std::runtime_error(msg);
        }
        void on_var_set_int(std::string_view name, size_t idx, int64_t val) override {
            if (name == "@COLOR") {
                if (m_ui_cache.fore_color.get_opt() == val) { return; }
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineForeColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                m_ui_cache.fore_color.set(val);
                return;
            }
            if (name == "@BGCOLOR") {
                if (m_ui_cache.back_color.get_opt() == val) { return; }
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->EngineBackColor(to_winrt_color((uint32_t)val | 0xff000000));
                });
                m_ui_cache.back_color.set(val);
                return;
            }
            if (name == "@STYLE") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetCurrentFontStyle(val);
                });
                return;
            }
            if (name == "@REDRAW") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetRedrawState(val);
                });
                return;
            }
            if (name == "@ALIGN") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetCurrentLineAlignment(val);
                });
                return;
            }
            if (name == "@TOOLTIP_DELAY") {
                // TODO: @TOOLTIP_DELAY
                return;
            }
            if (name == "@TOOLTIP_DURATION") {
                // TODO: @TOOLTIP_DURATION
                return;
            }
            if (name == "@SKIPDISP") {
                queue_ui_work([sd = m_sd, val]() {
                    sd->ui_ctrl->SetSkipDisplay(val);
                });
                return;
            }
            // TODO...
            if (name == "@ANIMETIMER") {
                // TODO: @ANIMETIMER
                return;
            }
            // TODO: Prohibit setting variables @DEF*COLOR?
            // TODO...
            std::string msg = "no such variable: ";
            msg += name;
            throw std::runtime_error(msg);
        }
        void on_var_set_str(std::string_view name, size_t idx, std::string_view val) override {
            if (name == "@FONT") {
                queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->SetCurrentFontName(val);
                });
                return;
            }
            if (name == "WINDOW_TITLE") {
                queue_ui_work([sd = m_sd, val = to_hstring(val)]() {
                    sd->ui_ctrl->EngineTitle(val);
                });
                return;
            }
            std::string msg = "no such variable: ";
            msg += name;
            throw std::runtime_error(msg);
        }
        void on_print_button(std::string_view content, std::string_view value, PrintExtendedFlags flags) override {
            queue_ui_work([sd = m_sd, content = to_hstring(content), value = to_hstring(value), flags] {
                sd->ui_ctrl->RoutinePrintButton(content, value, flags);
            });
        }
        int64_t on_gcreate(int64_t gid, int64_t width, int64_t height) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGCreate(gid, width, height);
            });
        }
        int64_t on_gcreatefromfile(int64_t gid, std::string_view path) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGCreateFromFile(gid, to_hstring(path));
            });
        }
        int64_t on_gdispose(int64_t gid) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGDispose(gid);
            });
        }
        int64_t on_gcreated(int64_t gid) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGCreated(gid);
            });
        }
        int64_t on_gdrawsprite(int64_t gid, std::string_view sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const* color_matrix) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGDrawSprite(gid, to_hstring(sprite_name), dest_x, dest_y, dest_width, dest_height, color_matrix);
            });
        }
        int64_t on_gclear(int64_t gid, int64_t color) override {
            // TODO: Use `queue_ui_work` for inexpensive graphics operations?
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineGClear(gid, (uint32_t)color | 0xff000000);
            });
        }
        int64_t on_spritecreate(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineSpriteCreate(to_hstring(name), gid, x, y, width, height);
            });
        }
        int64_t on_spritedispose(std::string_view name) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineSpriteDispose(to_hstring(name));
            });
        }
        int64_t on_spritecreated(std::string_view name) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineSpriteCreated(to_hstring(name));
            });
        }
        int64_t on_spriteanimecreate(std::string_view name, int64_t width, int64_t height) override {
            // TODO: on_spriteanimecreate
            return 0;
        }
        int64_t on_spriteanimeaddframe(std::string_view name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height, int64_t offset_x, int64_t offset_y, int64_t delay) override {
            // TODO: on_spriteanimeaddframe
            return 0;
        }
        int64_t on_spritewidth(std::string_view name) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineSpriteWidth(to_hstring(name));
            });
        }
        int64_t on_spriteheight(std::string_view name) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineSpriteHeight(to_hstring(name));
            });
        }
        std::unique_ptr<MEraEngineHostFile> on_open_host_file(std::string_view path, bool can_write) override {
            hstring actual_path;
            if (path.ends_with(".sav")) {
                // Try to isolate save file by adding `MEraEmu` to the path
                auto temp = path.substr(0, path.size() - 4);
                actual_path = to_hstring(temp) + L".MEraEmu.sav";
                if (!can_write && !util::fs::file_exists(actual_path.c_str())) {
                    actual_path = to_hstring(path);
                }
            }
            else {
                actual_path = to_hstring(path);
            }

            auto create_file_fn = [&] {
                file_handle fh(CreateFileW(
                    actual_path.c_str(),
                    GENERIC_READ | (can_write ? GENERIC_WRITE : 0),
                    FILE_SHARE_READ,
                    nullptr,
                    can_write ? OPEN_ALWAYS : OPEN_EXISTING,
                    0,
                    nullptr
                ));
                return fh;
            };
            auto fh = create_file_fn();
            if (!fh && can_write) {
                // File creation failed, try to create the parent directory
                auto parent_dir = std::filesystem::path(actual_path.c_str()).parent_path();
                std::filesystem::create_directories(parent_dir);
                // Then try again
                fh = create_file_fn();
            }
            check_bool((bool)fh);

            struct Win32File : MEraEngineHostFile {
                Win32File(file_handle fh) : m_fh(std::move(fh)) {}
                ~Win32File() {}

                uint64_t read(std::span<uint8_t> buf) override {
                    DWORD io_bytes;
                    check_bool(ReadFile(m_fh.get(), buf.data(), buf.size(), &io_bytes, nullptr));
                    return io_bytes;
                }
                void write(std::span<const uint8_t> buf) override {
                    DWORD io_bytes;
                    check_bool(WriteFile(m_fh.get(), buf.data(), buf.size(), &io_bytes, nullptr));
                    assert(buf.size() == io_bytes);
                }
                void flush() override {
                    check_bool(FlushFileBuffers(m_fh.get()));
                }
                void truncate() override {
                    SetFilePointer(m_fh.get(), 0, nullptr, FILE_BEGIN);
                    //check_bool(SetFilePointer(m_fh.get(), 0, nullptr, FILE_BEGIN) != INVALID_SET_FILE_POINTER);
                    check_bool(SetEndOfFile(m_fh.get()));
                }
                uint64_t seek(int64_t pos, EraCompilerFileSeekMode mode) override {
                    int native_mode = 0;
                    switch (mode) {
                    case ERA_COMPILER_FILE_SEEK_MODE_START:
                        native_mode = FILE_BEGIN;
                        break;
                    case ERA_COMPILER_FILE_SEEK_MODE_END:
                        native_mode = FILE_END;
                        break;
                    case ERA_COMPILER_FILE_SEEK_MODE_CURRENT:
                        native_mode = FILE_CURRENT;
                        break;
                    default:
                        std::terminate();
                    }
                    LONG high = (LONG)(pos >> 32);
                    LONG low = (LONG)pos;
                    auto r = SetFilePointer(m_fh.get(), low, &high, native_mode);
                    check_bool(r != INVALID_SET_FILE_POINTER || GetLastError() == NO_ERROR);
                    return (uint32_t)r + (((uint64_t)(uint32_t)high) << 32);
                }

            private:
                file_handle m_fh;
            };

            return std::make_unique<Win32File>(std::move(fh));
        }
        bool on_check_host_file_exists(std::string_view path) override {
            hstring actual_path;
            if (path.ends_with(".sav")) {
                // Try to isolate save file by adding `MEraEmu` to the path
                auto temp = path.substr(0, path.size() - 4);
                actual_path = to_hstring(temp) + L".MEraEmu.sav";
                if (!util::fs::file_exists(actual_path.c_str())) {
                    actual_path = to_hstring(path);
                }
            }
            else {
                actual_path = to_hstring(path);
            }

            return util::fs::file_exists(actual_path.c_str());
        }
        int64_t on_play_sound(std::string_view path, int64_t loop_count, bool is_bgm) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutinePlaySound(to_hstring(path), loop_count, is_bgm);
            });
        }
        int64_t on_stop_sound(int64_t sound_id) override {
            return exec_ui_work_or([&] {
                return m_sd->ui_ctrl->RoutineStopSound(sound_id);
            });
        }
        int64_t on_check_font(std::string_view font_name) override {
            static bool first_time = true;
            com_ptr<IDWriteFontCollection> font_collection;
            // NOTE: GetSystemFontCollection is *very* slow (~22ms), so cache the result.
            check_hresult(g_dwrite_factory->GetSystemFontCollection(font_collection.put(), first_time));
            first_time = false;
            uint32_t index;
            BOOL exists;
            check_hresult(font_collection->FindFamilyName(to_hstring(font_name).c_str(), &index, &exists));
            return exists ? 1 : 0;
        }
        uint64_t on_get_host_time() override {
            auto t = std::chrono::system_clock::now().time_since_epoch();
            return (uint64_t)std::chrono::duration_cast<std::chrono::milliseconds>(t).count();
        }
        int64_t on_get_config_int(std::string_view name) override {
            if (name == ERA_CONFIG_NAME_LINE_HEIGHT) {
                // Line height
                return m_sd->app_settings->GameLineHeight();
            }
            if (name == ERA_CONFIG_NAME_FONT_SIZE) {
                // Font size
                return m_sd->app_settings->GameFontSize();
            }
            if (name == ERA_CONFIG_NAME_WINDOW_WIDTH) {
                // Window width
                return on_var_get_int("@CLIENTWIDTH", 0);
            }
            if (name == ERA_CONFIG_NAME_SAVE_DATA_COUNT) {
                // Save data count
                return m_sd->app_settings->SaveDataCount();
            }
            if (name == ERA_CONFIG_NAME_AUTO_SAVE) {
                // Auto-save
                return m_sd->app_settings->EnableAutoSave() ? 1 : 0;
            }
            throw std::runtime_error(to_string(std::format(L"no such int config: {}", to_hstring(name))));
        }
        const char* on_get_config_str(std::string_view name) override {
            // TODO: on_get_config_str
            if (name == ERA_CONFIG_NAME_GRAPHICS_INTERFACE) {
                return "Direct2D";
            }
            throw std::runtime_error(to_string(std::format(L"no such str config: {}", to_hstring(name))));
        }
        int64_t on_get_key_state(int64_t key_code) override {
            // TODO: on_get_key_state
            return 0;
        }
        void on_await(int64_t milliseconds) override {
            if (milliseconds < 0) {
                milliseconds = 0;
            }
            if (milliseconds > 10000) {
                milliseconds = 10000;
            }
            // Sleep `milliseconds` milliseconds or until UI is dead
            bool expected = true;
            WaitOnAddress(&m_sd->ui_is_alive, &expected, sizeof expected, (DWORD)milliseconds);
        }

        void reset_ui_cache() {
            m_ui_cache = {};
        }

    private:
        void queue_ui_work(std::move_only_function<void()> work) {
            if (m_ui_task_tx->send(work) && !m_sd->ui_queue_work_debounce.load(std::memory_order_relaxed)) {
                // Wake up UI thread
                m_sd->ui_redraw_block_engine.wait(true, std::memory_order_relaxed);
                // Debounce to avoid too many UI update calls
                m_sd->ui_queue_work_debounce.store(true, std::memory_order_relaxed);
                // NOTE: CoreDispatcher.RunAsync will cause memory leak, so use
                //       DispatcherQueue.TryEnqueue instead
                m_sd->ui_dispatcher_queue.TryEnqueue(DispatcherQueuePriority::Low, [sd = m_sd->shared_from_this()] {
                    sd->ui_queue_work_debounce.store(false, std::memory_order_relaxed);
                    if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                    // SAFETY: We are in the UI thread; no one could be destructing
                    //         EngineControl.
                    sd->ui_ctrl->UpdateEngineUI();
                });
            }
        }
        template <typename F>
        auto exec_ui_work(F&& f) {
            using ReturnType = std::invoke_result_t<F>;
            std::promise<ReturnType> promise;
            auto future = promise.get_future();
            queue_ui_work([f = std::forward<F>(f), promise = std::move(promise)]() mutable {
                promise.set_value(f());
            });
            return future.get();
        }
        template <typename F>
        auto exec_ui_work_or(F&& f, std::invoke_result_t<F> def_val = {}) {
            try { return exec_ui_work(std::forward<F>(f)); }
            catch (std::future_error const& e) {
                if (e.code() == std::future_errc::broken_promise) {
                    // UI thread is dead
                    return def_val;
                }
                else {
                    throw;
                }
            }
        }

        EngineSharedData* const m_sd;
        util::sync::spsc::Sender<std::move_only_function<void()>> const* const m_ui_task_tx;
        std::string m_str_cache;
        std::mt19937_64 m_rand_gen{ std::random_device{}() };

        // UI value caches
        struct {
            ValueCache<uint32_t> fore_color;
            ValueCache<uint32_t> back_color;
        } m_ui_cache;

    public:
        int64_t m_tick_compensation{};
    };

    struct EngineUIPrintLineDataStyle {
        struct Color {
            uint32_t color;
        };
        struct Style {
            // 0x1: Bold, 0x2: Italic, 0x4: Strikeout, 0x8: Underline
            uint32_t style;
        };
        struct Font {
            hstring name;
        };

        uint32_t starti, len;
        std::variant<Color, Style, Font> data;
    };
    struct EngineUIPrintLineDataInlineObject::InlineObject : implements<InlineObject, IDWriteInlineObject> {
        InlineObject(EngineControl* ctrl, InlineObjectData data) : data(std::move(data)), ctrl(ctrl) {}

        // IDWriteInlineObject
        STDMETHOD(Draw)(void* clientDrawingContext, IDWriteTextRenderer* renderer, FLOAT originX, FLOAT originY, BOOL isSideways, BOOL isRightToLeft, IUnknown* clientDrawingEffect) override {
            // HACK: Exploits the UNDOCUMENTED `renderer`'s memory layout
            struct TextRenderer : ::IDWriteTextRenderer {
                ID2D1DeviceContext3* d2d_ctx;
                ID2D1Brush* brush;
            };
            auto get_d2d_ctx = [&] {
                return ((TextRenderer*)renderer)->d2d_ctx;
            };
            auto get_brush = [&] {
                return ((TextRenderer*)renderer)->brush;
            };

            auto shape_rect = [&](EngineUIPrintLineDataInlineObject::ShapeRect const& v) -> HRESULT {
                D2D1_RECT_F rect;
                rect.left = originX + ctrl->ConvLengthToPixels(v.x);
                rect.top = originY;
                rect.right = rect.left + ctrl->ConvLengthToPixels(v.width);
                rect.bottom = rect.top + ctrl->ConvLengthToPixels(v.height);
                auto d2d_ctx = get_d2d_ctx();
                if (clientDrawingEffect) {
                    com_ptr<ID2D1Brush> brush;
                    auto hr = clientDrawingEffect->QueryInterface(guid_of<ID2D1Brush>(), brush.put_void());
                    if (FAILED(hr)) { return hr; }
                    d2d_ctx->FillRectangle(rect, brush.get());
                }
                else {
                    d2d_ctx->FillRectangle(rect, get_brush());
                }
                return S_OK;
            };
            auto shape_space = [&](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) -> HRESULT {
                // Do nothing
                return S_OK;
            };
            auto image = [&](EngineUIPrintLineDataInlineObject::Image const& v) -> HRESULT {
                auto it = ctrl->m_sprite_objects.find(v.sprite);
                if (it == ctrl->m_sprite_objects.end()) {
                    // Sprite not found
                    return S_OK;
                }
                auto& sprite = it->second;
                D2D1_RECT_F rect;
                if (ctrl->m_app_settings->EnablePixelSnapping()) {
                    originX = std::round(originX);
                    originY = std::round(originY);
                }
                rect.left = originX;
                rect.top = originY;
                rect.right = rect.left + ctrl->ConvLengthToPixels(v.width);
                auto height_in_pixels = ctrl->ConvLengthToPixels(v.height);
                rect.bottom = rect.top + height_in_pixels;
                auto d2d_ctx = get_d2d_ctx();
                auto img_it = ctrl->m_graphics_objects.find(sprite.gid);
                if (img_it == ctrl->m_graphics_objects.end() || !img_it->second.try_ensure_loaded(ctrl)) {
                    // Graphics object not found or bad, draw crossed box
                    d2d_ctx->DrawRectangle(rect, get_brush(), 1.0f);
                    d2d_ctx->DrawLine({ rect.left, rect.top }, { rect.right, rect.bottom }, get_brush(), 1.0f);
                    d2d_ctx->DrawLine({ rect.right, rect.top }, { rect.left, rect.bottom }, get_brush(), 1.0f);
                    return S_OK;
                }
                else {
                    // Draw sprite
                    auto& img = img_it->second;
                    /*auto src_rect = D2D1::RectF(sprite.x, sprite.y, sprite.x + sprite.width, sprite.y + sprite.height);
                    com_ptr<ID2D1Effect> crop_effect;
                    check_hresult(d2d_ctx->CreateEffect(CLSID_D2D1Crop, crop_effect.put()));
                    check_hresult(crop_effect->SetValue(D2D1_CROP_PROP_RECT, src_rect));
                    crop_effect->SetInput(0, img.bitmap.get());
                    com_ptr<ID2D1Effect> scale_effect;
                    check_hresult(d2d_ctx->CreateEffect(CLSID_D2D1Scale, scale_effect.put()));
                    check_hresult(scale_effect->SetValue(D2D1_SCALE_PROP_SCALE, D2D1::Vector2F(
                        (float)(rect.right - rect.left) / sprite.width,
                        (float)(rect.bottom - rect.top) / sprite.height
                    )));
                    check_hresult(scale_effect->SetValue(D2D1_SCALE_PROP_INTERPOLATION_MODE,
                        D2D1_SCALE_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC));
                    scale_effect->SetInputEffect(0, crop_effect.get());
                    d2d_ctx->DrawImage(scale_effect.get(), D2D1::Point2F(rect.left, rect.top),
                        D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR);*/
                    ;
                    /*auto src_rect = D2D1::RectF(sprite.x, sprite.y, sprite.x + sprite.width, sprite.y + sprite.height);
                    d2d_ctx->DrawBitmap(img.bitmap.get(), rect, 1.0f,
                        D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC, src_rect);*/
                    ;
                    /*D2D1_RECT_U src_rect;
                    src_rect.left = saturate_to_u32(sprite.x);
                    src_rect.top = saturate_to_u32(sprite.y);
                    src_rect.right = saturate_to_u32(src_rect.left + sprite.width);
                    src_rect.bottom = saturate_to_u32(src_rect.top + sprite.height);
                    auto const& sb = ctrl->m_d2d_sprite_batch;
                    sb->Clear();
                    sb->AddSprites(1, &rect, &src_rect);
                    d2d_ctx->DrawSpriteBatch(sb.get(), img.bitmap.get(),
                        D2D1_BITMAP_INTERPOLATION_MODE_LINEAR, D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE);*/
                    ;

                    // HACK: A terrible workaround trying to fix D2D sampling outside the source region,
                    //       while avoiding degrading the quality under certain circumstances.
                    if (std::fabs(height_in_pixels - ctrl->m_ui_param_cache.font_size_px_f) < 1.0f) {
                        auto src_rect = D2D1::RectF(sprite.x, sprite.y, sprite.x + sprite.width, sprite.y + sprite.height);
                        d2d_ctx->DrawBitmap(img.bitmap.get(), rect, 1.0f,
                            D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC, src_rect);
                    }
                    else {
                        D2D1_RECT_U src_rect;
                        src_rect.left = saturate_to_u32(sprite.x);
                        src_rect.top = saturate_to_u32(sprite.y);
                        src_rect.right = saturate_to_u32(src_rect.left + sprite.width);
                        src_rect.bottom = saturate_to_u32(src_rect.top + sprite.height);
                        auto const& sb = ctrl->m_d2d_sprite_batch;
                        sb->Clear();
                        sb->AddSprites(1, &rect, &src_rect);
                        d2d_ctx->DrawSpriteBatch(sb.get(), img.bitmap.get(),
                            D2D1_BITMAP_INTERPOLATION_MODE_LINEAR, D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE);
                    }
                }
                if (ctrl->m_app_settings->DebugShowLayoutBounds()) {
                    d2d_ctx->DrawRectangle(rect, get_brush(), 1.0f);
                }
                return S_OK;
            };

            HRESULT hr = S_OK;
            auto hr2 = tenkai::winrt::ExceptionBoundary([&] {
                hr = std::visit(overloaded(
                    shape_rect,
                    shape_space,
                    image
                ), data);
            });
            if (FAILED(hr2)) { return hr2; }
            return hr;
        }
        STDMETHOD(GetMetrics)(DWRITE_INLINE_OBJECT_METRICS* metrics) override {
            DWRITE_INLINE_OBJECT_METRICS inlineMetrics{};
            inlineMetrics.width = get_object_width();
            //inlineMetrics.height = ctrl->ConvFontUnitToPixels(100);
            auto vrange = get_object_vertical_range();
            inlineMetrics.height = vrange.second - vrange.first;
            inlineMetrics.baseline = ctrl->m_ui_param_cache.font_size_px_f * 0.8f;

            float baselineOffset = 0.0f;
            auto shape_rect = [&](EngineUIPrintLineDataInlineObject::ShapeRect const& v) {
                baselineOffset = vrange.first;
            };
            auto shape_space = [&](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) {
                // HACK: Full font height for hit testing
                inlineMetrics.height = ctrl->ConvFontUnitToPixels(100);
            };
            auto image = [&](EngineUIPrintLineDataInlineObject::Image const& v) {
                baselineOffset = vrange.first;
            };
            std::visit(overloaded(
                shape_rect,
                shape_space,
                image
            ), data);

            inlineMetrics.baseline -= baselineOffset;
            *metrics = inlineMetrics;
            return S_OK;
        }
        STDMETHOD(GetOverhangMetrics)(DWRITE_OVERHANG_METRICS* overhangs) override {
            *overhangs = {};
            auto shape_rect = [&](EngineUIPrintLineDataInlineObject::ShapeRect const& v) {
                /*overhangs->top = ctrl->ConvFontUnitToPixels(-v.y);
                overhangs->bottom = ctrl->ConvFontUnitToPixels(v.y + v.height - 100);*/
            };
            auto shape_space = [&](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) {
                return;
            };
            auto image = [&](EngineUIPrintLineDataInlineObject::Image const& v) {
                /*overhangs->top = ctrl->ConvFontUnitToPixels(-v.ypos);
                overhangs->bottom = ctrl->ConvFontUnitToPixels(v.ypos + v.height - 100);*/
            };
            std::visit(overloaded(
                shape_rect,
                shape_space,
                image
            ), data);
            return S_OK;
        }
        STDMETHOD(GetBreakConditions)(DWRITE_BREAK_CONDITION* breakConditionBefore, DWRITE_BREAK_CONDITION* breakConditionAfter) override {
            *breakConditionBefore = DWRITE_BREAK_CONDITION_NEUTRAL;
            *breakConditionAfter = DWRITE_BREAK_CONDITION_NEUTRAL;
            return S_OK;
        }

        float get_object_width() {
            auto shape_rect = [this](EngineUIPrintLineDataInlineObject::ShapeRect const& v) {
                return std::max(ctrl->ConvLengthToPixels(v.x) + ctrl->ConvLengthToPixels(v.width), 0.0f);
            };
            auto shape_space = [this](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) {
                return std::max(ctrl->ConvLengthToPixels(v.size), 0.0f);
            };
            auto image = [this](EngineUIPrintLineDataInlineObject::Image const& v) {
                return std::max(ctrl->ConvLengthToPixels(v.width), 0.0f);
            };
            return std::visit(overloaded(
                shape_rect,
                shape_space,
                image
            ), data);
        }
        std::pair<float, float> get_object_vertical_range() {
            auto shape_rect = [this](EngineUIPrintLineDataInlineObject::ShapeRect const& v) {
                return std::make_pair(
                    ctrl->ConvLengthToPixels(v.y),
                    ctrl->ConvLengthToPixels(v.y) + ctrl->ConvLengthToPixels(v.height)
                );
            };
            auto shape_space = [this](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) {
                return std::pair{ 0.0f, 0.0f };
            };
            auto image = [this](EngineUIPrintLineDataInlineObject::Image const& v) {
                return std::make_pair(
                    ctrl->ConvLengthToPixels(v.ypos),
                    ctrl->ConvLengthToPixels(v.ypos) + ctrl->ConvLengthToPixels(v.height)
                );
            };
            return std::visit(overloaded(
                shape_rect,
                shape_space,
                image
            ), data);
        }

        EngineControl* ctrl;
        InlineObjectData data;
    };
    // NOTE: Styles & buttons are applied to all elements in the range, including inline objects.
    //       Inline objects replace the (placeholder) text in the range and may have own properties.
    struct EngineUIPrintLineData {
        hstring txt;
        com_ptr<IDWriteTextLayout4> txt_layout;
        uint32_t height{}; // Unit: line
        uint32_t acc_height{}; // Including the current line
        /* NOTE:
         * A line may render outside the line height, thus we need to keep track of the
         * amount of lines to render before and after the current line. `render_forward_height`
         * and `render_backward_height` are the amount of lines to render before and after
         * the current line, and they correspond to `lookback_render_pos` and
         * `lookforward_render_pos`, the accumulated line indices for actual rendering.
         * In other words, `render*` values describe the drawing area produced by the
         * current line, while `look*` values describe the lines that draw inside the
         * current line's area.
         */
        uint32_t render_forward_height{}; // Unit: line; usually >= height
        uint32_t render_backward_height{}; // Unit: line; usually >= height
        uint32_t lookback_render_pos{}; // Unit: line
        uint32_t lookforward_render_pos{}; // Unit: line
        std::vector<EngineUIPrintLineDataStyle> styles;
        std::vector<EngineUIPrintLineDataButton> buttons;
        std::vector<EngineUIPrintLineDataInlineObject> inline_objs; // Usually images
        HorizontalAlignment alignment{ HorizontalAlignment::Left };

        EngineUIPrintLineData(hstring const& txt) : txt(txt) {}

        bool is_empty() const noexcept {
            return txt.empty();
        }

        void put_empty(EngineControl* ctrl) {
            if (is_empty()) { return; }
            txt = {};
            txt_layout = ctrl->m_empty_text_layout;
            styles.clear();
            buttons.clear();
            inline_objs.clear();
            // No need to flush metrics; it is OK to keep stale metrics until resized
        }

        void update_width(EngineControl* ctrl, uint32_t width) {
            if (!txt_layout) { return ensure_layout(ctrl, width); }
            check_hresult(txt_layout->SetMaxWidth(static_cast<float>(width)));
        }

        void ensure_layout(EngineControl* ctrl, uint32_t width) {
            if (txt_layout) { return; }
            if (is_empty() && ctrl->m_empty_text_layout) {
                // Reuse empty text layout
                txt_layout = ctrl->m_empty_text_layout;
                return;
            }

            com_ptr<IDWriteTextLayout> tmp_layout;
            if (ctrl->m_app_settings->EnableGdiCompatRender()) {
                check_hresult(g_dwrite_factory->CreateGdiCompatibleTextLayout(
                    txt.c_str(), static_cast<UINT32>(txt.size()),
                    ctrl->GetDefaultTextFormat(),
                    static_cast<float>(width),
                    0,
                    1,
                    nullptr,
                    true,
                    tmp_layout.put()
                ));
            }
            else {
                check_hresult(g_dwrite_factory->CreateTextLayout(
                    txt.c_str(), static_cast<UINT32>(txt.size()),
                    ctrl->GetDefaultTextFormat(),
                    static_cast<float>(width),
                    0,
                    tmp_layout.put()
                ));
            }
            tmp_layout.as(txt_layout);
            auto line_height = ctrl->m_ui_param_cache.line_height_px_f;
            check_hresult(txt_layout->SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM,
                (float)line_height,
                (float)(line_height * 0.8)
            ));
            txt_layout->SetWordWrapping(DWRITE_WORD_WRAPPING_CHARACTER);
            switch (alignment) {
            case HorizontalAlignment::Left:
                txt_layout->SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
                break;
            case HorizontalAlignment::Center:
                txt_layout->SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
                break;
            case HorizontalAlignment::Right:
                txt_layout->SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
                break;
            }
            for (auto const& inline_obj : inline_objs) {
                txt_layout->SetInlineObject(inline_obj.obj.get(), { inline_obj.starti, inline_obj.len });
            }

            if (is_empty()) [[unlikely]] {
                // Cache empty text layout
                // NOTE: This should not result in data race since we always add the first empty line
                //       before any other lines are added.
                ctrl->m_empty_text_layout = txt_layout;
            }
        }

        // NOTE: This function may influence the layout, don't forget to call `flush_metrics` after this.
        void flush_effects(EngineControl* ctrl) {
            txt_layout->SetDrawingEffect(nullptr, { 0, txt.size() });
            for (auto const& style : styles) {
                DWRITE_TEXT_RANGE range{ style.starti, style.len };
                auto set_color = [&](EngineUIPrintLineDataStyle::Color const& v) {
                    txt_layout->SetDrawingEffect(ctrl->GetOrCreateSolidColorBrush(v.color), range);
                };
                auto set_style = [&](EngineUIPrintLineDataStyle::Style const& v) {
                    if (v.style & ERA_FONT_STYLE_BOLD) {
                        // Bold
                        txt_layout->SetFontWeight(DWRITE_FONT_WEIGHT_BOLD, range);
                    }
                    if (v.style & ERA_FONT_STYLE_ITALIC) {
                        // Italic
                        txt_layout->SetFontStyle(DWRITE_FONT_STYLE_ITALIC, range);
                    }
                    if (v.style & ERA_FONT_STYLE_STRIKEOUT) {
                        // Strikeout
                        txt_layout->SetStrikethrough(true, range);
                    }
                    if (v.style & ERA_FONT_STYLE_UNDERLINE) {
                        // Underline
                        txt_layout->SetUnderline(true, range);
                    }
                };
                auto set_font = [&](EngineUIPrintLineDataStyle::Font const& v) {
                    txt_layout->SetFontFamilyName(v.name.c_str(), range);
                };
                std::visit(overloaded(
                    set_color,
                    set_style,
                    set_font
                ), style.data);
            }
        }

        float get_actual_width() const {
            if (!txt_layout) { return 0.0f; }
            DWRITE_TEXT_METRICS1 metrics;
            check_hresult(txt_layout->GetMetrics(&metrics));
            return metrics.width;
        }

        D2D1_RECT_F get_visual_bounding_box() const {
            if (!txt_layout) { return {}; }
            DWRITE_TEXT_METRICS1 metrics;
            DWRITE_OVERHANG_METRICS overhangs;
            check_hresult(txt_layout->GetMetrics(&metrics));
            check_hresult(txt_layout->GetOverhangMetrics(&overhangs));
            metrics.left = metrics.top = 0;
            return D2D1::RectF(
                metrics.left - overhangs.left,
                metrics.top - overhangs.top,
                metrics.left + metrics.layoutWidth + overhangs.right,
                metrics.top + overhangs.bottom
            );
        }

        hstring to_html_string() const {
            if (txt.empty()) { return {}; }

            // TODO: Use map<pos, vec of tags> for memory efficiency?
            std::vector<std::vector<hstring>> tag_strs(txt.size() + 1);
            {
                auto align = [&] {
                    switch (alignment) {
                    case HorizontalAlignment::Left: return L"left";
                    case HorizontalAlignment::Center: return L"center";
                    case HorizontalAlignment::Right: return L"right";
                    default: return L"left";
                    }
                }();
                tag_strs[0].push_back(winrt::format(L"<p align=\"{}\">", align));
                tag_strs[txt.size()].push_back(L"</p>");
            }
            for (auto const& style : styles) {
                auto set_color = [&](EngineUIPrintLineDataStyle::Color const& v) {
                    tag_strs[style.starti].push_back(winrt::format(L"<font color=\"#{:06X}\">", v.color));
                    tag_strs[style.starti + style.len].push_back(L"</font>");
                };
                auto set_style = [&](EngineUIPrintLineDataStyle::Style const& v) {
                    if (v.style & ERA_FONT_STYLE_BOLD) {
                        tag_strs[style.starti].push_back(L"<b>");
                        tag_strs[style.starti + style.len].push_back(L"</b>");
                    }
                    if (v.style & ERA_FONT_STYLE_ITALIC) {
                        tag_strs[style.starti].push_back(L"<i>");
                        tag_strs[style.starti + style.len].push_back(L"</i>");
                    }
                    if (v.style & ERA_FONT_STYLE_STRIKEOUT) {
                        tag_strs[style.starti].push_back(L"<s>");
                        tag_strs[style.starti + style.len].push_back(L"</s>");
                    }
                    if (v.style & ERA_FONT_STYLE_UNDERLINE) {
                        tag_strs[style.starti].push_back(L"<u>");
                        tag_strs[style.starti + style.len].push_back(L"</u>");
                    }
                };
                auto set_font = [&](EngineUIPrintLineDataStyle::Font const& v) {
                    tag_strs[style.starti].push_back(winrt::format(L"<font face=\"{}\">", v.name));
                    tag_strs[style.starti + style.len].push_back(L"</font>");
                };
                std::visit(overloaded(
                    set_color,
                    set_style,
                    set_font
                ), style.data);
            }
            for (auto const& button : buttons) {
                if (auto data = std::get_if<EngineUIPrintLineDataButton::InputButton>(&button.data)) {
                    tag_strs[button.starti].push_back(winrt::format(L"<button value=\"{}\">", data->input));
                    tag_strs[button.starti + button.len].push_back(L"</button>");
                }
            }
            for (auto const& inline_obj : inline_objs) {
                auto set_shape_rect = [&](EngineUIPrintLineDataInlineObject::ShapeRect const& v) {
                    tag_strs[inline_obj.starti].push_back(winrt::format(
                        LR"(<shape type="rect" param="{},{},{},{}">)",
                        to_hstring(v.x), to_hstring(v.y), to_hstring(v.width), to_hstring(v.height)
                    ));
                };
                auto set_shape_space = [&](EngineUIPrintLineDataInlineObject::ShapeSpace const& v) {
                    tag_strs[inline_obj.starti].push_back(winrt::format(
                        LR"(<shape type="space" param="{}">)",
                        to_hstring(v.size)
                    ));
                };
                auto set_image = [&](EngineUIPrintLineDataInlineObject::Image const& v) {
                    tag_strs[inline_obj.starti].push_back(winrt::format(
                        LR"("<img src="{}" width="{}" height="{}" ypos="{}">)",
                        v.sprite, to_hstring(v.width), to_hstring(v.height), to_hstring(v.ypos)
                    ));
                };
                std::visit(overloaded(
                    set_image,
                    [](auto&&) {}
                ), inline_obj.obj->data);
            }

            // Combine tags
            std::wstring result;
            for (size_t i = 0; i < txt.size(); ++i) {
                for (auto const& tag : tag_strs[i]) {
                    result += tag;
                }
                result += txt[i];
            }
            for (auto const& tag : tag_strs[txt.size()]) {
                result += tag;
            }
            return hstring(result);
        }

        void flush_metrics(EngineControl* ctrl) {
            if (!txt_layout) { return; }

            //acc_height -= line_height;
            DWRITE_TEXT_METRICS1 metrics;
            check_hresult(txt_layout->GetMetrics(&metrics));
            height = metrics.lineCount;
            DWRITE_OVERHANG_METRICS overhangs;
            check_hresult(txt_layout->GetOverhangMetrics(&overhangs));
            render_backward_height = (uint32_t)std::ceil(std::max(overhangs.top, 0.0f) / ctrl->m_ui_param_cache.line_height_px_f);
            render_forward_height = (uint32_t)std::ceil(overhangs.bottom / ctrl->m_ui_param_cache.line_height_px_f);
            //acc_height += line_height;
        }
    };

    auto EngineControl::MakeUILinesSnapshotGuard() {
        return tenkai::cpp_utils::ScopeExit([this, old_line_count = m_ui_lines.size(), old_composing_line = m_cur_composing_line]() mutable {
            while (size(m_ui_lines) > old_line_count) {
                m_ui_lines.pop_back();
            }
            m_cur_composing_line = std::move(old_composing_line);
        });
    }

    EngineControl::EngineControl() {
        ensure_global_factory();
    }
    EngineControl::~EngineControl() {
        if (m_devtools_wnd) {
            m_devtools_wnd.Close();
            m_devtools_wnd = nullptr;
        }
        UISideDisconnect();
    }
    void EngineControl::InitializeComponent() {
        EngineControlT::InitializeComponent();

        // Initialize default settings
        ApplySettings(nullptr);

        // Register for scale notification...
        auto bkg_swapchain_panel = BackgroundSwapchainPanel();
        bkg_swapchain_panel.CompositionScaleChanged([this](SwapChainPanel const&, auto&&) {
            UpdateEngineImageOutputLayout(true);
        });
        // And width notification.
        SizeChanged([this](auto&&, auto&&) {
            UpdateEngineImageOutputLayout(true);
            // Always bring view to bottom
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        });
        UpdateEngineImageOutputLayout(true);
        EngineOutputImage().SizeChanged([this](auto&&, auto&&) {
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        });
        RootScrollViewer().ViewChanged([this](auto&&, auto&&) {
            auto pt = m_cur_pointer.pt;
            auto root_sv = RootScrollViewer();
            if (root_sv.ScrollableHeight() <= 0) {
                // Slow path
                pt = root_sv.TransformToVisual(EngineOutputImage()).TransformPoint(pt);
            }
            else {
                // Fast path
                pt.Y = float(pt.Y + root_sv.VerticalOffset());
            }
            UpdateAndInvalidateActiveButton(pt);
        });

        // HACK: Prevent automatic bring into view
        UserInputTextBox().BringIntoViewRequested([](auto&&, BringIntoViewRequestedEventArgs const& e) {
            e.Handled(true);
        });

        // Initialize input countdown timer
        m_input_countdown_timer.Interval(std::chrono::milliseconds{ 1 });
        m_input_countdown_timer.Tick({ get_weak(), &EngineControl::OnInputCountDownTick });
    }
    void EngineControl::ReturnToTitle() {
        m_outstanding_input_req = nullptr;
        QueueEngineTask(std::make_unique<EngineThreadTask>(EngineThreadTaskKind::ReturnToTitle));

        m_cur_printc_count = 0;
        m_user_skipping = false;
        m_ui_lines.clear();
        m_cur_composing_line = {};
        m_reused_last_line = false;
        m_empty_text_layout = nullptr;
        FlushCurrentPrintLine(true);    // Add an empty line at the top of the screen
        m_soft_deleted_ui_lines_count = m_soft_deleted_ui_lines_pos = 0;
        m_cur_font_name = m_app_settings->GameDefaultFontName();
        m_cur_font_style = 0;
        check_hresult(m_vsis_noref->Resize(0, 0));

        if (m_sound.hub) {
            m_sound.hub.stop_all();
        }
        EngineForeColor(m_app_settings->GameForegroundColor());
        EngineBackColor(m_app_settings->GameBackgroundColor());
    }
    void EngineControl::ApplySettings(MEraEmuWin::AppSettingsVM settings) {
        com_ptr<implementation::AppSettingsVM> new_settings;
        if (!settings) {
            new_settings = make_self<implementation::AppSettingsVM>();
        }
        else {
            new_settings = settings.DeepClone().as<implementation::AppSettingsVM>();
        }
        auto old_settings = std::exchange(m_app_settings, new_settings);
        // Synchronize settings to engine
        QueueEngineTask(std::make_unique<EngineThreadTask>(EngineThreadTaskKind::SyncSettingsWithFunc, [this] {
            m_sd->app_settings = m_app_settings;
        }));

        bool font_changed{};
        bool layout_changed{};
        if (old_settings) {
            if (old_settings->GameDefaultFontName() != new_settings->GameDefaultFontName()) {
                font_changed = true;
                if (m_cur_font_name.empty() || m_cur_font_name == old_settings->GameDefaultFontName()) {
                    m_cur_font_name = new_settings->GameDefaultFontName();
                }
            }
            if (old_settings->EnableGdiCompatRender() != new_settings->EnableGdiCompatRender()) {
                layout_changed = true;
            }
        }

        bool recreate_ui_lines = font_changed || layout_changed;
        UpdateEngineImageOutputLayout(true, recreate_ui_lines);
    }
    bool EngineControl::IsStarted() {
        return m_sd && m_sd->thread_is_alive.load(std::memory_order_relaxed);
    }
    void EngineControl::IsDevToolsOpen(bool value) {
        if (value == IsDevToolsOpen()) { return; }

        if (value) {
            m_devtools_wnd = Tenkai::UI::Xaml::Window();
            auto page = make<DevTools::implementation::MainPage>();
            m_devtools_wnd.Content(page);
            UpdateDevToolsWindow();
            m_devtools_wnd.Activate();
            page.SetConnectedEngineControl(*this);
            m_devtools_wnd.View().Closing([this](auto&&, auto&&) {
                m_devtools_wnd = nullptr;
            });
        }
        else {
            m_devtools_wnd.Close();
            m_devtools_wnd = nullptr;
        }
    }
    bool EngineControl::IsDevToolsOpen() {
        return m_devtools_wnd != nullptr;
    }
    void EngineControl::AudioVolume(double value) {
        // Do not handle sound if sound functions are disabled
        if (!m_app_settings->ReadSoundDir()) { return; }

        if (!m_sound.hub) {
            m_sound.initial_volume = value;
            return;
        }
        m_sound.hub.set_output_volume(value);
    }
    double EngineControl::AudioVolume() {
        if (!m_sound.hub) { return m_sound.initial_volume; }
        return m_sound.hub.get_output_volume();
    }
    void EngineControl::EngineOutputImage_PointerMoved(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        e.Handled(true);
        auto root_sv = RootScrollViewer();
        auto ptr_pt = e.GetCurrentPoint(root_sv);
        m_cur_pointer.pt = ptr_pt.Position();
        auto pt = m_cur_pointer.pt;
        if (root_sv.ScrollableHeight() <= 0) {
            // Slow path
            pt = root_sv.TransformToVisual(EngineOutputImage()).TransformPoint(pt);
        }
        else {
            // Fast path
            pt.Y += root_sv.VerticalOffset();
        }
        UpdateAndInvalidateActiveButton(pt);

#if 1
        // Support left-click while holding right-click.
        // TODO: This behavior is not consistent with Tapped event, which is fired on LeftButtonReleased.
        //       Should we change it to all fire on PointerReleased / PointerPressed?
        using PointerUpdateKind = Windows::UI::Input::PointerUpdateKind;
        if (ptr_pt.Properties().PointerUpdateKind() == PointerUpdateKind::LeftButtonPressed) {
            m_user_skipping = true;
            HandleEngineLeftClick(m_cur_pointer.pt, pt);
        }
#endif
    }
    void EngineControl::EngineOutputImage_PointerPressed(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        auto props = e.GetCurrentPoint(nullptr).Properties();
        m_cur_pointer.left_button_down = props.IsLeftButtonPressed();
        m_cur_pointer.right_button_down = props.IsRightButtonPressed();
    }
    void EngineControl::EngineOutputImage_PointerReleased(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        auto props = e.GetCurrentPoint(nullptr).Properties();
        m_cur_pointer.left_button_down = props.IsLeftButtonPressed();
        m_cur_pointer.right_button_down = props.IsRightButtonPressed();
    }
    void EngineControl::EngineOutputImage_PointerExited(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        m_cur_pointer.pt = { -1, -1 };
        //m_cur_pointer.left_button_down = m_cur_pointer.right_button_down = false;
        UpdateAndInvalidateActiveButton(m_cur_pointer.pt);
    }
    void EngineControl::EngineOutputImage_PointerCanceled(IInspectable const& sender, PointerRoutedEventArgs const& e) {
        m_cur_pointer.pt = { -1, -1 };
        m_cur_pointer.left_button_down = m_cur_pointer.right_button_down = false;
        UpdateAndInvalidateActiveButton(m_cur_pointer.pt);
    }
    void EngineControl::EngineOutputImage_Tapped(IInspectable const& sender, TappedRoutedEventArgs const& e) {
        e.Handled(true);
        HandleEngineLeftClick(
            e.GetPosition(RootScrollViewer()),
            e.GetPosition(EngineOutputImage())
        );
    }
    void EngineControl::EngineOutputImage_RightTapped(IInspectable const& sender, RightTappedRoutedEventArgs const& e) {
        e.Handled(true);

        // Bring to bottom
        RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
        // Try to skip input requests
        if (auto& input_req = m_outstanding_input_req) {
            if (input_req->can_click || input_req->can_skip) {
                input_req->try_fulfill_void();
                input_req = nullptr;
                m_user_skipping = true;
            }
        }
    }
    void EngineControl::UserInputTextBox_KeyDown(IInspectable const& sender, KeyRoutedEventArgs const& e) {
        if (e.Key() == VirtualKey::Enter) {
            e.Handled(true);

            // Try to fulfill input requests
            TryFulfillInputRequest(true);
        }
    }
    void EngineControl::Bootstrap(hstring const& game_base_dir) try {
        UISideDisconnect();
        m_sd = std::make_shared<EngineSharedData>();
        m_sd->game_base_dir = game_base_dir;
        m_sd->ui_dispatcher_queue = DispatcherQueue::GetForCurrentThread();
        //m_sd->ui_ctrl = get_weak();
        m_sd->ui_ctrl = this;
        m_sd->app_settings = m_app_settings;
        // Create channels
        auto [ui_task_tx, ui_task_rx] = util::sync::spsc::sync_channel<std::move_only_function<void()>>(64);
        auto [engine_task_tx, engine_task_rx] = util::sync::spsc::sync_channel<std::unique_ptr<EngineThreadTask>>(64);
        m_ui_task_rx = std::move(ui_task_rx);
        m_engine_task_tx = std::move(engine_task_tx);
        // Start a dedicated background thread
#if _DEBUG
        auto start_thread_fn = []<typename F>(F && f) {
            using OrigF = std::remove_cvref_t<F>;
            auto ptr = std::make_unique<OrigF>(std::forward<F>(f));
            // HACK: Extend stack size to 4MB
            CloseHandle(CreateThread(nullptr, 4ull * 1024 * 1024, [](void* p) -> DWORD {
                auto ptr = std::unique_ptr<OrigF>(static_cast<OrigF*>(p));
                (*ptr)();
                return 0;
            }, ptr.release(), 0, nullptr));
        };
        start_thread_fn([sd = m_sd, ui_task_tx = std::move(ui_task_tx), engine_task_rx = std::move(engine_task_rx)] {
#else
        m_sd->thread_task_op = ThreadPool::RunAsync([sd = m_sd, ui_task_tx = std::move(ui_task_tx), engine_task_rx = std::move(engine_task_rx)](IAsyncAction const&) {
#endif
            SetThreadDescription(GetCurrentThread(), L"MEraEmu Engine Thread");

            sd->thread_is_alive.store(true, std::memory_order_relaxed);

            auto& engine = sd->engine;
            auto queue_ui_work = [&](std::move_only_function<void()> work) {
                auto is_vacant = ui_task_tx.is_vacant();
                if (ui_task_tx.send(work) && is_vacant) {
                    // Wake up UI thread
                    sd->ui_redraw_block_engine.wait(true, std::memory_order_relaxed);
                    // NOTE: CoreDispatcher.RunAsync will cause memory leak, so use
                    //       DispatcherQueue.TryEnqueue instead
                    sd->ui_dispatcher_queue.TryEnqueue(DispatcherQueuePriority::Low, [sd] {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        // SAFETY: We are in the UI thread; no one could be destructing
                        //         EngineControl.
                        sd->ui_ctrl->UpdateEngineUI();
                    });
                }
            };

            auto mark_thread_started = [&] {
                sd->thread_started.store(true, std::memory_order_release);
                sd->thread_started.notify_one();
            };

            auto task_disconnect = [&] {
                sd->thread_is_alive.store(false, std::memory_order_relaxed);
                sd->thread_is_alive.notify_one();
                // Triggers UI update, which in turn responds to task disconnection
                queue_ui_work([] {});
            };

            tenkai::cpp_utils::ScopeExit se_thread([&] {
                task_disconnect();
                // In case of exception, don't hang the UI thread
                mark_thread_started();
                engine = nullptr;
            });

            try {
                sd->ui_is_alive.wait(false, std::memory_order_acquire);

                // TODO: Enable threaded loading when we upgrade mimalloc to v2.1.9
                //       (see https://github.com/microsoft/mimalloc/issues/944)
                auto& appcfg = sd->app_settings;

                MEraEmuWinEngineSysCallback* callback{};
                auto builder = [&] {
                    auto callback_box = std::make_unique<MEraEmuWinEngineSysCallback>(sd.get(), &ui_task_tx);
                    callback = callback_box.get();
                    return MEraEngineBuilder(std::move(callback_box));
                }();

                auto update_config_fn = [&](MEraEngineConfig& cfg) {
                    cfg.threads_cnt = appcfg->EnableParallelLoading() ? 0 : 1;
                    cfg.vm_cache_strategy = appcfg->EnableJIT() ?
                        M_ERA_ENGINE_VM_CACHE_STRATEGY_FAST_JIT :
                        M_ERA_ENGINE_VM_CACHE_STRATEGY_DISABLED;
                };

                {
                    auto cfg = builder.get_config();
                    update_config_fn(cfg);
                    builder.set_config(cfg);
                }

                // Register global variables
                // Engine -- register int
                auto eri = [&](const char* name, bool watch = true) {
                    builder.register_variable(name, false, 1, watch);
                };
                // Engine -- register str
                auto ers = [&](const char* name, bool watch = true) {
                    builder.register_variable(name, true, 1, watch);
                };
                eri("@COLOR");
                eri("@DEFCOLOR");
                eri("@BGCOLOR");
                eri("@DEFBGCOLOR");
                eri("@FOCUSCOLOR");
                eri("@STYLE");
                ers("@FONT");
                eri("@REDRAW");
                eri("@ALIGN");
                eri("@TOOLTIP_DELAY");
                eri("@TOOLTIP_DURATION");
                eri("@SKIPDISP");
                eri("@MESSKIP");
                eri("@ANIMETIMER");
                eri("@PRINTCPERLINE");
                eri("@PRINTCLENGTH");
                eri("@LINEISEMPTY");
                ers("WINDOW_TITLE");
                ers("DRAWLINESTR");
                eri("@CLIENTCHARWIDTH");
                eri("@CLIENTWIDTH");
                eri("LINECOUNT");
                ers("SAVEDATA_TEXT", false);
                eri("RANDDATA");
                builder.set_variable_int("@DEFCOLOR", 0,
                    to_u32(sd->app_settings->GameForegroundColor()) & ~0xff000000);
                builder.set_variable_int("@DEFBGCOLOR", 0,
                    to_u32(sd->app_settings->GameBackgroundColor()) & ~0xff000000);

                auto return_to_title = [&] {
                    EraFuncInfo func_info;
                    try {
                        func_info = engine.get_func_info("SYSPROC_BEGIN_TITLE").value();
                    }
                    // Ignore if function does not exist
                    catch (...) { return; }
                    if (func_info.frame_info.args.size() != 0) {
                        throw hresult_error(E_FAIL, L"malformed entry function");
                    }
                    EraExecIp ip{
                        .chunk = func_info.chunk_idx,
                        .offset = func_info.bc_offset,
                    };
                    engine.reset_exec_to_ip(ip);
                };

                auto run_ui_task = [&](std::unique_ptr<EngineThreadTask> task, bool loaded) {
                    if (!loaded) { return; }
                    if (task->kind == EngineThreadTaskKind::ReturnToTitle) {
                        return_to_title();
                    }
                    else if (task->kind == EngineThreadTaskKind::CustomFunc || task->kind == EngineThreadTaskKind::CustomFuncAndClearHaltState) {
                        task->f();
                    }
                    else {
                        throw hresult_error(E_FAIL, L"unexpected task kind");
                    }
                };

                // Collect files used by engine
                {
                    std::vector<std::filesystem::path> misc_csvs;
                    std::vector<std::filesystem::path> chara_csvs;
                    auto load_csv = [&](std::filesystem::path const& csv, EraCsvLoadKind kind) {
                        auto [data, size] = read_utf8_file(csv);
                        builder.load_csv(to_string(csv.c_str()).c_str(), { data.get(), size }, kind);
                    };
                    for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"CSV")) {
                        if (!entry.is_regular_file()) { continue; }
                        auto const& path = entry.path();
                        auto filename = path.filename();
                        std::wstring_view sv{ filename.c_str() };
                        if (ieq(sv, L"_Rename.csv")) { load_csv(path, ERA_CSV_LOAD_KIND__RENAME); }
                        else if (ieq(sv, L"_Replace.csv")) { load_csv(path, ERA_CSV_LOAD_KIND__REPLACE); }
                        else if (ieq(sv, L"VariableSize.csv")) { load_csv(path, ERA_CSV_LOAD_KIND_VARIABLE_SIZE); }
                        else {
                            if (!iends_with(sv, L".csv")) { continue; }
                            if (istarts_with(sv, L"chara")) {
                                chara_csvs.push_back(path);
                            }
                            else {
                                misc_csvs.push_back(path);
                            }
                        }
                    }
                    std::vector<std::filesystem::path> erhs;
                    std::vector<std::filesystem::path> erbs;
                    for (auto const& entry : recur_dir_iter(sd->game_base_dir, L"ERB")) {
                        if (!entry.is_regular_file()) { continue; }
                        auto filename = entry.path().filename();
                        if (iends_with(filename.c_str(), L".erh")) {
                            erhs.push_back(entry.path());
                        }
                        else if (iends_with(filename.c_str(), L".erb")) {
                            erbs.push_back(entry.path());
                        }
                        else {
                            // Do nothing
                        }
                    }

                    // Source loading takes a lot of time, so mark thread as started here
                    mark_thread_started();

                    // Use a dedicated thread to read ERB files
                    auto [erb_tx, erb_rx] = util::sync::spsc::sync_channel<std::tuple<std::filesystem::path, std::unique_ptr<uint8_t[]>, size_t>>(64);
                    std::jthread erb_thread;
                    MEraEngineAsyncErbLoader async_erb_loader = nullptr;
                    if (appcfg->EnableParallelLoading()) {
                        // Use async loader from MEraEngineBuilder
                        async_erb_loader = builder.start_async_erb_loader();
                        erb_thread = std::jthread([&, async_erb_loader = std::move(async_erb_loader)] {
                            for (auto& erb : erbs) {
                                if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                                auto [data, size] = read_utf8_file(erb);
                                async_erb_loader.load_erb(to_string(erb.c_str()).c_str(), { data.get(), size });
                            }
                        });
                    }
                    else {
                        // Use our own channel
                        erb_thread = std::jthread([&, erb_tx = std::move(erb_tx)] {
                            for (auto& erb : erbs) {
                                if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                                auto [data, size] = read_utf8_file(erb);
                                std::tuple<std::filesystem::path, std::unique_ptr<uint8_t[]>, size_t> erb_data{ erb, std::move(data), size };
                                if (!erb_tx.send(erb_data)) { return; }
                            }
                        });
                    }
                    auto se_erb_rx = tenkai::cpp_utils::ScopeExit([&] {
                        erb_rx = nullptr;
                    });

                    // Load CSV files
                    for (auto& csv : misc_csvs) {
                        auto filename = csv.filename();
                        std::wstring_view sv{ filename.c_str() };
                        if (ieq(sv, L"ABL.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_ABL); }
                        else if (ieq(sv, L"EXP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EXP); }
                        else if (ieq(sv, L"TALENT.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_TALENT); }
                        else if (ieq(sv, L"PALAM.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_PALAM); }
                        else if (ieq(sv, L"TRAIN.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_TRAIN); }
                        else if (ieq(sv, L"MARK.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_MARK); }
                        else if (ieq(sv, L"ITEM.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_ITEM); }
                        else if (ieq(sv, L"BASE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_BASE); }
                        else if (ieq(sv, L"SOURCE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_SOURCE); }
                        else if (ieq(sv, L"EX.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EX); }
                        else if (ieq(sv, L"STR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STR); }
                        else if (ieq(sv, L"EQUIP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_EQUIP); }
                        else if (ieq(sv, L"TEQUIP.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_EQUIP); }
                        else if (ieq(sv, L"FLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_FLAG); }
                        else if (ieq(sv, L"TFLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_FLAG); }
                        else if (ieq(sv, L"CFLAG.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_FLAG); }
                        else if (ieq(sv, L"TCVAR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_C_VAR); }
                        else if (ieq(sv, L"CSTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_STR); }
                        else if (ieq(sv, L"STAIN.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STAIN); }
                        else if (ieq(sv, L"CDFLAG1.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_D_FLAG1); }
                        else if (ieq(sv, L"CDFLAG2.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_C_D_FLAG2); }
                        else if (ieq(sv, L"STRNAME.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_STR_NAME); }
                        else if (ieq(sv, L"TSTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_T_STR); }
                        else if (ieq(sv, L"SAVESTR.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_SAVE_STR); }
                        else if (ieq(sv, L"GLOBAL.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GLOBAL); }
                        else if (ieq(sv, L"GLOBALS.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GLOBALS); }
                        else if (ieq(sv, L"GAMEBASE.CSV")) { load_csv(csv, ERA_CSV_LOAD_KIND_GAME_BASE); }
                        else {
                            // Do nothing
                        }
                    }
                    for (auto& csv : chara_csvs) {
                        load_csv(csv, ERA_CSV_LOAD_KIND_CHARA_);
                    }
                    if (appcfg->ReadResourcesDir()) {
                        auto resources_dir = sd->game_base_dir + L"resources";
                        if (util::fs::file_exists(resources_dir.c_str())) {
                            using std::filesystem::recursive_directory_iterator;
                            for (auto const& entry : recursive_directory_iterator(resources_dir.c_str())) {
                                if (!entry.is_regular_file()) { continue; }
                                auto const& path = entry.path();
                                if (iends_with(path.c_str(), L".csv")) {
                                    load_csv(path, ERA_CSV_LOAD_KIND_IMAGE_RESOURCES);
                                }
                            }
                        }
                    }
                    builder.finish_load_csv();

                    auto try_handle_thread_event = [&] {
                        if (auto task = engine_task_rx.try_recv()) {
                            run_ui_task(std::move(*task), false);
                        }
                    };

                    // Load ERB files
                    for (auto& erh : erhs) {
                        if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                        try_handle_thread_event();
                        auto [data, size] = read_utf8_file(erh);
                        builder.load_erh(to_string(erh.c_str()).c_str(), { data.get(), size });
                    }
                    builder.finish_load_erh();
                    if (appcfg->EnableParallelLoading()) {
                        builder.wait_for_async_loader();
                    }
                    else {
                        auto t0 = std::chrono::high_resolution_clock::now();
                        auto t1 = t0;
                        size_t load_count{};
                        while (auto erb_opt = erb_rx.recv()) {
                            if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                            try_handle_thread_event();
                            auto& [name, data, size] = *erb_opt;
                            t1 = std::chrono::high_resolution_clock::now();
                            if (t1 - t0 >= std::chrono::milliseconds(10) || sd->has_execution_error) {
                                // Update UI loading progress periodically
                                t0 = t1;
                                sd->has_execution_error = false;
                                auto msg = winrt::format(L"Loading ERB ({} / {} loaded) `{}`... ",
                                    load_count, erbs.size(), name.c_str());
                                queue_ui_work([&, msg = std::move(msg)] {
                                    sd->ui_ctrl->RoutineReuseLastLine(msg);
                                });
                            }
                            load_count++;
                            builder.load_erb(to_string(name.c_str()).c_str(), { data.get(), size });
                        }
                    }
                }

                // Finialize compilation
                if (!sd->ui_is_alive.load(std::memory_order_relaxed)) { return; }
                engine = builder.build();

                return_to_title();

                // Disable auto redraw before running VM
                queue_ui_work([sd] {
                    sd->ui_ctrl->SetRedrawState(0);
                });

                // Main loop
                EraExecutionBreakReason stop_reason = ERA_EXECUTION_BREAK_REASON_CALLBACK_BREAK;
                uint64_t instructions_to_exec = UINT64_MAX;
                bool force_exec = false;
                bool need_update_config = false;
                // (?) Notify UI thread that engine is about to execute instructions
                queue_ui_work([sd, stop_reason] {
                    sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                });
                while (sd->ui_is_alive.load(std::memory_order_relaxed)) {
                    if (need_update_config) {
                        need_update_config = false;
                        auto cfg = engine.get_config();
                        update_config_fn(cfg);
                        engine.set_config(cfg);
                    }
                    callback->reset_ui_cache();

                    // If not reached max instructions, we treat engine as halted
                    auto is_halted = [&] {
                        return is_execution_break_reason_fatal(stop_reason);
                    };
                    auto clear_halted = [&] {
                        stop_reason = ERA_EXECUTION_BREAK_REASON_CALLBACK_BREAK;
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                    };
                    auto set_halted = [&] {
                        stop_reason = ERA_EXECUTION_BREAK_REASON_DEBUG_BREAK_INSTRUCTION;
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                    };
                    bool should_notify_stop = false;
                    while (force_exec || !is_halted()) {
                        should_notify_stop = true;

                        //std::atomic_bool alwaystrue{ true };
                        stop_reason = engine.do_execution(
                            //force_exec ? alwaystrue : engine_task_rx.is_vacant_var(),
                            engine_task_rx.is_vacant_var(),
                            instructions_to_exec);

                        break;
                    }
                    force_exec = false;
                    const bool is_limited_exec = instructions_to_exec != UINT64_MAX;
                    if (is_limited_exec) {
                        // Limit reached, auto halt
                        instructions_to_exec = UINT64_MAX;
                    }

                    callback->m_tick_compensation = 0;

                    if (sd->has_execution_error) {
                        // Dump stack trace when execution error occurs
                        sd->has_execution_error = false;
                        struct SrcData {
                            hstring path;
                            uint32_t line, column;
                            hstring msg;
                        };
                        std::vector<SrcData> print_msgs;
                        auto stack_trace = engine.get_stack_trace();
                        for (const auto& frame : stack_trace.frames | std::views::reverse) {
                            auto ip = frame.ip;
                            auto func_info = engine.get_func_info_by_ip(ip).value();
                            auto chunk_info = engine.get_chunk_info(func_info.chunk_idx).value();
                            auto src_info = engine.get_src_info_from_ip(ip).value();
                            auto path = to_hstring(src_info.filename);
                            auto resolved = engine.resolve_src_span(src_info.filename, src_info.span).value();
                            auto msg = format(L"  {}({},{}):{}\nSnippet: {}",
                                path,
                                resolved.loc.line,
                                resolved.loc.col + 1,
                                to_hstring(func_info.name),
                                to_hstring(resolved.snippet)
                            );
                            print_msgs.emplace_back(
                                path,
                                resolved.loc.line, resolved.loc.col + 1,
                                std::move(msg)
                            );
                        }
                        queue_ui_work([sd, msgs = std::move(print_msgs)] {
                            sd->ui_ctrl->SetSkipDisplay(0);

                            sd->ui_ctrl->RoutinePrint(L"函数堆栈跟踪 (最近调用者最先显示):", ERA_PEF_IS_LINE);
                            for (auto const& msg : msgs) {
                                sd->ui_ctrl->RoutinePrintSourceButton(msg.msg, msg.path,
                                    msg.line, msg.column, ERA_PEF_IS_LINE);
                            }

                            sd->ui_ctrl->SetRedrawState(2);
                        });
                    }

                    if (should_notify_stop) {
                        queue_ui_work([sd, stop_reason] {
                            sd->ui_ctrl->OnEngineExecutionInterrupted(stop_reason);
                        });
                        if (is_limited_exec) {
                            if (stop_reason == ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS) {
                                stop_reason = ERA_EXECUTION_BREAK_REASON_DEBUG_BREAK_INSTRUCTION;
                            }
                        }
                    }

                    // Handle one task event
                    do {
                        auto task_opt = engine_task_rx.recv();
                        if (!task_opt) { break; }
                        auto& task = *task_opt;
                        if (task->kind == EngineThreadTaskKind::ReturnToTitle) {
                            return_to_title();
                            clear_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::SetHaltState) {
                            set_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::ClearHaltState) {
                            clear_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::SingleStepAndHalt) {
                            instructions_to_exec = 1;
                            clear_halted();
                            force_exec = true;
                            break;
                        }
                        else if (task->kind == EngineThreadTaskKind::CustomFunc) {
                            if (task->f) {
                                task->f();
                            }
                        }
                        else if (task->kind == EngineThreadTaskKind::CustomFuncAndClearHaltState) {
                            if (task->f) {
                                task->f();
                            }
                            clear_halted();
                        }
                        else if (task->kind == EngineThreadTaskKind::SyncSettingsWithFunc) {
                            if (task->f) {
                                task->f();
                            }
                            need_update_config = true;
                        }
                        //run_ui_task(std::move(task), true);
                    } while (!engine_task_rx.is_vacant());
                }
            }
            catch (...) {
                sd->thread_exception = std::current_exception();
            }
#if _DEBUG
        });
#else
        }, WorkItemPriority::Normal, WorkItemOptions::TimeSliced);
#endif
        m_sd->ui_is_alive.store(true, std::memory_order_release);
        m_sd->ui_is_alive.notify_one();

        // Initialize engine UI component
        InitEngineUI();

        // Check whether engine has panicked
        m_sd->thread_started.wait(false, std::memory_order_acquire);
        if (!m_sd->thread_is_alive.load(std::memory_order_relaxed)) {
            assert(m_sd->thread_exception);
            std::rethrow_exception(m_sd->thread_exception);
        }

        // If DevTools is open, connect to it
        if (IsDevToolsOpen()) {
            m_devtools_wnd.Content().as<DevTools::MainPage>().SetConnectedEngineControl(*this);
        }
    }
    catch (std::exception const& e) {
        throw hresult_error(E_FAIL, to_hstring(e.what()));
    }
    void EngineControl::UISideDisconnect() {
        // No longer relevant, so also clear the event handlers
        m_ev_EngineExecutionInterrupted.clear();
        // Close DevTools connection
        if (IsDevToolsOpen()) {
            m_devtools_wnd.Content().as<DevTools::MainPage>().SetConnectedEngineControl(nullptr);
        }
        // Clear engine resources
        m_outstanding_input_req = nullptr;
        if (auto sd = std::exchange(m_sd, nullptr)) {
            if (sd->thread_task_op) {
                sd->thread_task_op.Cancel();
            }
            sd->ui_is_alive.store(false, std::memory_order_relaxed);
            sd->ui_is_alive.notify_one();
            sd->ui_redraw_block_engine.store(false, std::memory_order_relaxed);
            sd->ui_redraw_block_engine.notify_one();
            // Close the channel to signal the engine thread to stop
            m_engine_task_tx = nullptr;
            // Wait for engine thread to stop
            // NOTE: Don't execute any engine tasks after this point, for example some tasks
            //       may set m_outstanding_input_req, which gets in the way of our disconnecting
            //       process.
            while (m_ui_task_rx.recv()) {}
        }
    }
    void EngineControl::QueueEngineTask(std::unique_ptr<EngineThreadTask> task) {
        if (!m_sd) { return; }
        // In order for engine to handle tasks properly, we need to interrupt pending input requests
        if (m_outstanding_input_req) {
            m_outstanding_input_req = nullptr;
        }
        m_engine_task_tx.send(task);
    }
    void EngineControl::QueueEngineTask(EngineThreadTaskKind task_kind) {
        QueueEngineTask(std::make_unique<EngineThreadTask>(task_kind));
    }
    void EngineControl::QueueEngineFuncTask(std::move_only_function<void(MEraEngine const&)> f, bool clear_halt) {
        QueueEngineTask(std::make_unique<EngineThreadTask>([sd = m_sd.get(), f = std::move(f)]() mutable {
            f(sd->engine);
        }, clear_halt));
    }
    void EngineControl::OnEngineExecutionInterrupted(EraExecutionBreakReason reason) {
        m_last_execution_break_reason = reason;
        m_ev_EngineExecutionInterrupted(reason);

        // If engine control is enabled, print control buttons after the error
        if (m_app_settings->EnableEngineControlOnError()) {
            if (reason != ERA_EXECUTION_BREAK_REASON_CALLBACK_BREAK &&
                reason != ERA_EXECUTION_BREAK_REASON_STOP_FLAG &&
                reason != ERA_EXECUTION_BREAK_REASON_DEBUG_BREAK_INSTRUCTION &&
                reason != ERA_EXECUTION_BREAK_REASON_CODE_QUIT &&
                reason != ERA_EXECUTION_BREAK_REASON_REACHED_MAX_INSTRUCTIONS)
            {
                using ButtonType = EngineUIPrintLineDataButton::EngineErrorControlButton::ButtonType;
                RoutinePrint(L"由于发生错误, 引擎已经停止运行。接下来要做什么? ", 0);
                RoutinePrintEngineErrorControlButton(L"[重试]", ButtonType::Retry);
                RoutinePrintEngineErrorControlButton(L"[继续(不推荐)]", ButtonType::Continue);
                RoutinePrint({}, ERA_PEF_IS_LINE);
                m_reused_last_line = true;
            }
        }
    }
    void EngineControl::UpdateDevToolsWindow() {
        if (!IsDevToolsOpen()) { return; }

        // Update window title
        m_devtools_wnd.Title(format(L"{} - MEraEmu DevTools", EngineTitle()));
    }
    void EngineControl::InitEngineUI() {
        // Reset UI resources
        m_vsis_noref = nullptr;
        m_vsis_d2d_noref = nullptr;
        m_d2d_ctx = nullptr;
        m_ui_lines.clear();
        m_cur_composing_line = {};
        m_reused_last_line = false;
        m_empty_text_layout = nullptr;
        FlushCurrentPrintLine(true);    // Add an empty line at the top of the screen
        m_soft_deleted_ui_lines_count = m_soft_deleted_ui_lines_pos = 0;
        m_cur_line_alignment = {};
        m_cur_font_style = {};
        m_cur_font_name = m_app_settings->GameDefaultFontName();
        m_auto_redraw = true;
        m_skip_display = false;
        m_no_skip_display_cnt = 0;
        m_cur_printc_count = 0;
        m_user_skipping = false;
        m_cur_pointer.pt = { -1, -1 };
        m_brush_map.clear();
        m_font_map.clear();
        m_graphics_objects.clear();
        m_sprite_objects.clear();
        EngineForeColor(m_app_settings->GameForegroundColor());
        EngineBackColor(m_app_settings->GameBackgroundColor());
        if (m_sound.hub) {
            double old_vol = -1;
            old_vol = m_sound.hub.get_output_volume();
            drop_background(std::exchange(m_sound, {}));
            if (old_vol >= 0) {
                AudioVolume(old_vol);
            }
        }
        UpdateEngineImageOutputLayout(true);
        VirtualSurfaceImageSource img_src(0, 0);
        EngineOutputImage().Source(img_src);
        m_vsis_noref = img_src.as<IVirtualSurfaceImageSourceNative>().get();
        m_vsis_d2d_noref = img_src.as<ISurfaceImageSourceNativeWithD2D>().get();
        // Initialize Direct2D immediately after the creation of EngineOutputImage
        InitD2DDevice(!m_app_settings->EnableHardwareAcceleration());
        // TODO: Handle Windows.UI.Xaml.Media.CompositionTarget.SurfaceContentsLost
        // TODO...

        // Enable redraw notification
        struct RedrawCallback : implements<RedrawCallback, IVirtualSurfaceUpdatesCallbackNative> {
            RedrawCallback(EngineControl* ui_ctrl) : m_ui_ctrl(ui_ctrl) {}

            STDMETHOD(UpdatesNeeded)() {
                return tenkai::winrt::ExceptionBoundary([&] {
                    // TODO: SAFETY
                    m_ui_ctrl->RedrawDirtyEngineImageOutput();
                });
            }

        private:
            EngineControl* const m_ui_ctrl;
        };
        m_vsis_noref->RegisterForUpdatesNeeded(make<RedrawCallback>(this).get());

        // Set UI focus
        VisualStateManager::GoToState(*this, L"ExecutionStarted", true);
        UserInputTextBox().Focus(FocusState::Programmatic);
    }
    void EngineControl::UpdateEngineUI() try {
        // Process UI works sent from the engine thread
        while (auto work = m_ui_task_rx.try_recv()) {
            (*work)();
        }

        // Handle engine thread termination
        if (!m_sd->thread_is_alive.load(std::memory_order_relaxed)) {
            VisualStateManager::GoToState(*this, L"ExecutionEnded", true);
            if (m_sd->thread_exception) {
                EmitUnhandledExceptionEvent(m_sd->thread_exception);
            }
        }
    }
    catch (...) {
        EmitUnhandledExceptionEvent(std::current_exception());
    }
    void EngineControl::EmitUnhandledExceptionEvent(std::exception_ptr ex) {
        hresult code;
        hstring ex_msg;
        try { std::rethrow_exception(ex); }
        catch (...) {
            code = to_hresult();
            ex_msg = to_message();
        }
        m_ev_UnhandledException(*this, make<EngineUnhandledExceptionEventArgs>(code, ex_msg));
    }
    void EngineControl::RedrawDirtyEngineImageOutput() {
        int height = GetAccUIHeightInLines();
        DWORD update_rt_cnt{};
        check_hresult(m_vsis_noref->GetUpdateRectCount(&update_rt_cnt));
        std::vector<RECT> update_rts(update_rt_cnt);
        check_hresult(m_vsis_noref->GetUpdateRects(update_rts.data(), update_rt_cnt));

        // Update every dirty rectangle
        for (auto const& update_rt : update_rts) {
            com_ptr<ID2D1DeviceContext> ctx;
            POINT offset{};
            check_hresult(m_vsis_d2d_noref->BeginDraw(update_rt,
                guid_of<decltype(ctx)>(), ctx.put_void(), &offset));
            tenkai::cpp_utils::ScopeExit se_begin_draw([&] {
                // Deliberately ignores errors
                m_vsis_d2d_noref->EndDraw();
            });
            // NOTE: Scale is applied to font size, so do nothing here
            ctx->SetTransform(
                //D2D1::Matrix3x2F::Scale(m_xscale, m_yscale) *
                D2D1::Matrix3x2F::Translation(offset.x - update_rt.left, offset.y - update_rt.top)
            );
            if (m_app_settings->EnableFontSmoothing()) {
                ctx->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE);
            }
            else {
                ctx->SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_ALIASED);
            }
            ctx->SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
            D2D1_DRAW_TEXT_OPTIONS text_options = D2D1_DRAW_TEXT_OPTIONS_NONE;
            if (!m_app_settings->EnablePixelSnapping()) {
                text_options |= D2D1_DRAW_TEXT_OPTIONS_NO_SNAP;
            }
            int dip_rt_top = update_rt.top;
            int dip_rt_bottom = update_rt.bottom;
            // Clear stale bitmap area
            ctx->Clear(D2D1::ColorF(D2D1::ColorF::White, 0));
            // Obtain lines requiring redraws
            auto ib = begin(m_ui_lines), ie = end(m_ui_lines);
            int line_start = std::ranges::upper_bound(ib, ie, dip_rt_top,
                [](auto const& a, auto const& b) { return a < b; },
                [&](auto const& e) { return e.acc_height * m_ui_param_cache.line_height_px_f; }
            ) - ib;
            int line_end = std::ranges::upper_bound(ib, ie, dip_rt_bottom,
                [](auto const& a, auto const& b) { return a < b; },
                [&](auto const& e) { return e.acc_height * m_ui_param_cache.line_height_px_f; }
            ) - ib;

            // Look forward rendering
            if (line_end < size(m_ui_lines)) {
                auto& line_data = m_ui_lines[line_end];
                line_end = line_data.lookforward_render_pos;
                line_end++;
                if (line_end >= size(m_ui_lines)) {
                    line_end = size(m_ui_lines);
                }
            }
            // Look back rendering
            if (line_start < line_end) {
                auto& line_data = m_ui_lines[line_start];
                line_start = line_data.lookback_render_pos;
            }

            // Fallback brush
            const auto default_fore_color = to_u32(m_app_settings->GameForegroundColor()) | 0xff000000;
            auto brush = GetOrCreateSolidColorBrush(default_fore_color);

            for (int line = line_start; line < line_end; line++) {
                auto& line_data = m_ui_lines[line];
                int offx = 0;
                int offy = (line_data.acc_height - line_data.height) * m_ui_param_cache.line_height_px_f;

                auto draw_fn = [&] {
                    ctx->DrawTextLayout(D2D1::Point2F(offx, offy), line_data.txt_layout.get(),
                        brush, text_options);

                    if (m_app_settings->DebugShowLayoutBounds()) {
                        D2D1_RECT_F rect = line_data.get_visual_bounding_box();
                        rect.left += offx;
                        rect.right += offx;
                        rect.top += offy;
                        rect.bottom += offy;
                        auto bound_brush = GetOrCreateSolidColorBrush(D2D1::ColorF::Magenta | 0xff000000);
                        ctx->DrawRectangle(rect, bound_brush, 1.0f);
                    }
                };

                // If we have an active button at this line, apply and flush effects
                if (m_cur_active_button.line == line && m_cur_active_button.button_idx < size(line_data.buttons)) {
                    auto const& btn_data = line_data.buttons[m_cur_active_button.button_idx];
                    EngineUIPrintLineDataStyle style{
                        .starti = btn_data.starti,
                        .len = btn_data.len,
                        .data = EngineUIPrintLineDataStyle::Color(to_u32(m_app_settings->GameHighlightColor()) | 0xff000000)
                    };
                    line_data.styles.push_back(style);
                    tenkai::cpp_utils::ScopeExit se_style([&] {
                        line_data.styles.pop_back();
                        try { line_data.flush_effects(this); }
                        catch (...) {}
                    });
                    line_data.flush_effects(this);
                    draw_fn();
                }
                else {
                    draw_fn();
                }
            }
        }

        if (m_sd->ui_redraw_block_engine.exchange(false, std::memory_order_relaxed)) {
            m_sd->ui_redraw_block_engine.notify_one();
        }
    }
    void EngineControl::UpdateEngineImageOutputLayout(bool invalidate_all, bool recreate_ui_lines) {
        // Before committing changes to user, handle soft-deleted lines from CLEARLINE
        if (size_t soft_del_lines_count = std::exchange(m_soft_deleted_ui_lines_count, 0)) {
            auto ie = begin(m_ui_lines) + m_soft_deleted_ui_lines_pos;
            auto ib = ie - soft_del_lines_count;
            // Check how many previous lines are affected
            uint32_t min_line = ib - begin(m_ui_lines);
            for (auto it = ib; it != ie; ++it) {
                auto& line = *it;
                uint32_t i = it - begin(m_ui_lines);
                if (line.render_backward_height >= i) {
                    min_line = 0;
                    break;
                }
                min_line = std::min(min_line, i - line.render_backward_height);
            }
            auto& line = m_ui_lines[min_line];
            const auto new_height = (line.acc_height - line.height) * m_ui_param_cache.line_height_px_f;
            m_last_redraw_dirty_height = std::min(m_last_redraw_dirty_height, (uint32_t)new_height);

            // Erase lines
            m_ui_lines.erase(ib, ie);

            // Fix line heights
            for (size_t i = m_soft_deleted_ui_lines_pos - soft_del_lines_count; i < size(m_ui_lines); i++) {
                UpdateLineAccMetrics(i);
            }
        }

        auto& ui_params = m_ui_param_cache;
        bool need_invalidate_line_metrics = recreate_ui_lines;
        bool fully_invalidate_line_metrics = recreate_ui_lines;
        if (invalidate_all) {
            // Recalculate UI parameters
            auto sp = BackgroundSwapchainPanel();
            ui_params.xscale = sp.CompositionScaleX();
            ui_params.yscale = sp.CompositionScaleY();
            float new_ui_scale;
            if (m_app_settings->AutoDetectGameUIScale()) {
                new_ui_scale = std::max(ui_params.xscale, ui_params.yscale);
            }
            else {
                new_ui_scale = m_app_settings->GameUIScale();
            }
            if (ui_params.ui_scale != new_ui_scale) {
                ui_params.ui_scale = new_ui_scale;
                need_invalidate_line_metrics = true;
            }
            // Align canvas width to 4 pixels to prevent blurring
            auto new_canvas_width = std::floor(ActualWidth() / 4) * 4;
            EngineOutputImage().Width(new_canvas_width);
            auto new_canvas_width_px = int(new_canvas_width * ui_params.xscale);
            if (ui_params.canvas_width_px != new_canvas_width_px) {
                ui_params.canvas_width_px = new_canvas_width_px;
                need_invalidate_line_metrics = true;
            }

            auto new_line_height_px_f = m_app_settings->GameLineHeight() * ui_params.ui_scale;
            if (m_app_settings->EnablePixelSnapping()) {
                new_line_height_px_f = std::round(new_line_height_px_f);
            }
            if (ui_params.line_height_px_f != new_line_height_px_f) {
                ui_params.line_height_px_f = new_line_height_px_f;
                need_invalidate_line_metrics = true;
                fully_invalidate_line_metrics = true;
            }
            auto new_font_size_px_f = m_app_settings->GameFontSize() * ui_params.ui_scale;
            if (m_app_settings->EnablePixelSnapping()) {
                new_font_size_px_f = std::round(new_font_size_px_f);
            }
            if (ui_params.font_size_px_f != new_font_size_px_f) {
                ui_params.font_size_px_f = new_font_size_px_f;
                need_invalidate_line_metrics = true;
                fully_invalidate_line_metrics = true;
                // Clear font cache to force re-creation of fonts of new size
                m_font_map.clear();
            }
            ui_params.line_char_capacity = uint32_t(ui_params.canvas_width_px / ui_params.font_size_px_f);

            // Redraw all lines
            m_last_redraw_dirty_height = 0;
        }

        if (need_invalidate_line_metrics) {
            RelayoutUILines(fully_invalidate_line_metrics);
        }

        if (!m_vsis_noref) { return; }

        auto height = int(GetAccUIHeightInLines() * ui_params.line_height_px_f);
        ui_params.canvas_height_px = height;
        check_hresult(m_vsis_noref->Resize(ui_params.canvas_width_px, height));
        check_hresult(m_vsis_noref->Invalidate(
            { 0, (long)m_last_redraw_dirty_height, (long)ui_params.canvas_width_px, height }));
        m_last_redraw_dirty_height = height;
        // TODO: Must call InvalidateMeasure() for updates to be visible. Is this an XAML bug?
        EngineOutputImage().InvalidateMeasure();
    }
    void EngineControl::InitD2DDevice(bool force_software) {
        D3D_FEATURE_LEVEL feature_levels[]{
            D3D_FEATURE_LEVEL_11_1,
            D3D_FEATURE_LEVEL_11_0,
            D3D_FEATURE_LEVEL_10_1,
            D3D_FEATURE_LEVEL_10_0,
            D3D_FEATURE_LEVEL_9_3,
            D3D_FEATURE_LEVEL_9_2,
            D3D_FEATURE_LEVEL_9_1,
        };
        com_ptr<ID3D11Device> d3d_dev;
        check_hresult(D3D11CreateDevice(
            nullptr,
            force_software ? D3D_DRIVER_TYPE_WARP : D3D_DRIVER_TYPE_HARDWARE,
            0,
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            feature_levels,
            static_cast<UINT>(std::size(feature_levels)),
            D3D11_SDK_VERSION,
            d3d_dev.put(),
            nullptr,
            nullptr
        ));
        auto dxgi_dev = d3d_dev.as<IDXGIDevice3>();

        com_ptr<ID2D1Device> d2d_dev;
        check_hresult(D2D1CreateDevice(dxgi_dev.get(), nullptr, d2d_dev.put()));
        com_ptr<ID2D1DeviceContext> d2d_ctx;
        check_hresult(d2d_dev->CreateDeviceContext(
            D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
            d2d_ctx.put()
        ));
        d2d_ctx.as(m_d2d_ctx);
        check_hresult(m_d2d_ctx->CreateSpriteBatch(m_d2d_sprite_batch.put()));

        // Associate output image with created Direct2D device
        check_hresult(m_vsis_d2d_noref->SetDevice(d2d_dev.get()));
    }
    void EngineControl::RelayoutUILines(bool recreate_all) {
        const float new_width = m_ui_param_cache.canvas_width_px;
        if (recreate_all) {
            m_empty_text_layout = nullptr;
            for (auto& line : m_ui_lines) {
                line.txt_layout = nullptr;
            }
        }

        constexpr size_t PARALLEL_THRESHOLD = 4000;
        if (size(m_ui_lines) > PARALLEL_THRESHOLD) {
            // Multi-threaded process
            auto total_cnt = size(m_ui_lines);
            auto split_cnt = (total_cnt + total_cnt - 1) / PARALLEL_THRESHOLD;
            if (split_cnt > 4) {
                split_cnt = 4;
            }
            std::vector<std::future<void>> workers;
            for (size_t i = 1; i < split_cnt; i++) {
                auto start_pos = total_cnt * i / split_cnt;
                auto end_pos = total_cnt * (i + 1) / split_cnt;
                workers.push_back(std::async(std::launch::async, [=] {
                    for (size_t i = start_pos; i < end_pos; i++) {
                        auto& line = m_ui_lines[i];
                        line.update_width(this, new_width);
                        line.flush_metrics(this);
                    }
                }));
            }
            for (size_t i = 0; i < total_cnt; i++) {
                auto& line = m_ui_lines[i];
                if (i < total_cnt * 1 / split_cnt) {
                    line.update_width(this, new_width);
                }
                else {
                    for (auto& worker : workers) {
                        worker.get();
                    }
                    workers.clear();
                }
                if (recreate_all) {
                    line.flush_effects(this);
                }

                line.flush_metrics(this);
                UpdateLineAccMetrics(i);
            }
        }
        else {
            // Process directly on the UI thread
            for (size_t i = 0; i < size(m_ui_lines); i++) {
                auto& line = m_ui_lines[i];
                line.update_width(this, new_width);
                if (recreate_all) {
                    line.flush_effects(this);
                }

                line.flush_metrics(this);
                UpdateLineAccMetrics(i);
            }
        }
    }
    uint64_t EngineControl::GetAccUIHeightInLines(size_t line_idx) {
        if (m_ui_lines.empty()) { return 0; }
        if (line_idx >= size(m_ui_lines)) { line_idx = size(m_ui_lines) - 1; }
        return m_ui_lines[line_idx].acc_height;
    }
    size_t EngineControl::GetLineIndexFromHeight(uint64_t height) {
        auto ib = begin(m_ui_lines), ie = end(m_ui_lines);
        return std::ranges::upper_bound(ib, ie, height,
            [](auto const& a, auto const& b) { return a < b; },
            [&](auto const& e) { return e.acc_height * m_ui_param_cache.line_height_px_f; }
        ) - ib;
    }
    void EngineControl::UpdateLineAccMetrics(size_t i) {
        if (i >= size(m_ui_lines)) { return; }
        auto& line = m_ui_lines[i];
        if (i == 0) {
            line.acc_height = line.height;
            line.lookback_render_pos = line.lookforward_render_pos = 0;
        }
        else {
            auto const& prev_line = m_ui_lines[i - 1];
            line.acc_height = prev_line.acc_height + line.height;
            // Look back area
            line.lookback_render_pos = prev_line.lookback_render_pos;
            while (line.lookback_render_pos < i &&
                line.lookback_render_pos + m_ui_lines[line.lookback_render_pos].render_forward_height <= i)
            {
                line.lookback_render_pos++;
            }
            // Look forward area
            {
                size_t pos = line.render_backward_height >= i ? 0 : i - line.render_backward_height;
                for (; pos < i; pos++) {
                    m_ui_lines[pos].lookforward_render_pos = i;
                }
                line.lookforward_render_pos = i;
            }
        }
    }
    void EngineControl::InvalidateLineAtIndex(size_t line) {
        if (line >= size(m_ui_lines)) { return; }
        auto width = long(m_ui_param_cache.canvas_width_px);
        auto const& cur_line = m_ui_lines[line];
        auto line_start_idx = line >= cur_line.render_backward_height ? line - cur_line.render_backward_height : 0;
        auto line_end_idx = line + cur_line.render_forward_height;
        if (line_end_idx > 0) { line_end_idx--; } // Inclusive
        if (line_end_idx >= size(m_ui_lines)) { line_end_idx = size(m_ui_lines) - 1; }
        auto const& line_start = m_ui_lines[line_start_idx];
        auto const& line_end = m_ui_lines[line_end_idx];
        auto height_1 = (long)std::floor((line_start.acc_height - line_start.height) * m_ui_param_cache.line_height_px_f);
        auto height_2 = (long)std::floor(line_end.acc_height * m_ui_param_cache.line_height_px_f);
        height_2 = std::min(height_2, long(m_ui_param_cache.canvas_height_px));
        if FAILED(m_vsis_noref->Invalidate({ 0, height_1, width, height_2 })) {
            // Sometimes the invalidation fails (because of OOB), so we need to redraw the whole thing
            UpdateEngineImageOutputLayout(true);
        }
    }
    void EngineControl::UpdateAndInvalidateActiveButton(Point const& pt) {
        auto pt_x = pt.X * m_ui_param_cache.xscale;
        auto pt_y = pt.Y * m_ui_param_cache.yscale;
        auto line = GetLineIndexFromHeight(pt_y);

        // Check for buttons
        ActiveButtonData new_active_button;
        if ((pt_x >= 0 && pt_y >= 0) && line < size(m_ui_lines)) {
            auto const& cur_line = m_ui_lines[line];
            auto start_line_idx = cur_line.lookback_render_pos;
            auto end_line_idx = cur_line.lookforward_render_pos;
            if (end_line_idx >= size(m_ui_lines)) { end_line_idx = size(m_ui_lines) - 1; }
            end_line_idx++;

            for (auto i = start_line_idx; i < end_line_idx; i++) {
                auto const& cur_line = m_ui_lines[i];
                if (cur_line.buttons.empty()) { continue; }

                // Perform hit test on text
                BOOL is_trailing_hit;
                BOOL is_inside;
                DWRITE_HIT_TEST_METRICS hit_test_metrics;
                check_hresult(cur_line.txt_layout->HitTestPoint(
                    pt_x, pt_y - (cur_line.acc_height - cur_line.height) * m_ui_param_cache.line_height_px_f,
                    &is_trailing_hit, &is_inside, &hit_test_metrics
                ));
                if (!is_inside) { continue; }
                auto cur_pos = hit_test_metrics.textPosition;
                for (size_t j = 0; j < size(cur_line.buttons); j++) {
                    auto const& cur_btn = cur_line.buttons[j];
                    if (cur_btn.starti <= cur_pos && cur_pos < cur_btn.starti + cur_btn.len) {
                        // Found the target button
                        new_active_button.line = i;
                        new_active_button.button_idx = j;
                        break;
                    }
                }

            }
        }

        // If we are on a button, request redraw for that (/ those) lines
        if (new_active_button != m_cur_active_button) {
            InvalidateLineAtIndex(m_cur_active_button.line);
            m_cur_active_button = new_active_button;
            InvalidateLineAtIndex(new_active_button.line);
        }
    }
    bool EngineControl::TryFulfillInputRequest(bool clear_input) {
        if (auto& input_req = m_outstanding_input_req) {
            auto input_tb = UserInputTextBox();
            auto input = input_tb.Text();
            if (clear_input) {
                input_tb.Text({});
            }
            if (input_req->try_fulfill(input)) {
                m_input_countdown_timer.Stop();
                input_req = nullptr;

                // Echo back
                if (!input.empty()) { RoutinePrint(input, ERA_PEF_IS_LINE); }

                return true;
            }
        }
        return false;
    }
    ID2D1SolidColorBrush* EngineControl::GetOrCreateSolidColorBrush(uint32_t color) {
        auto& entry = m_brush_map[color];
        if (!entry) {
            check_hresult(m_d2d_ctx->CreateSolidColorBrush(
                D2D1::ColorF(color, (color >> 24) / 255.f), entry.put()));
        }
        return entry.get();
    }
    IDWriteTextFormat* EngineControl::GetOrCreateTextFormat(hstring const& font_family) {
        auto& entry = m_font_map[font_family];
        if (!entry) {
            check_hresult(g_dwrite_factory->CreateTextFormat(
                font_family.c_str(),
                nullptr,
                DWRITE_FONT_WEIGHT_REGULAR,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                std::max(m_ui_param_cache.font_size_px_f, 1e-6f),
                L"",
                entry.put()
            ));
        }
        return entry.get();
    }
    void EngineControl::OnInputCountDownTick(IInspectable const&, IInspectable const&) {
        auto& input_req = m_outstanding_input_req;
        if (!input_req) {
            // TODO: Is this unreachable?
            m_input_countdown_timer.Stop();
            return;
        }

        // Check remaining time
        auto cur_t = winrt::clock::now();
        auto remaining_time = std::chrono::milliseconds(input_req->time_limit) -
            (cur_t - m_input_start_t);
        if (remaining_time.count() <= 0) {
            // Expired
            input_req->time_limit = std::chrono::duration_cast<std::chrono::milliseconds>(remaining_time).count();
            tenkai::cpp_utils::ScopeExit se_input_req([&] {
                input_req->try_fulfill_void();
                input_req = nullptr;
            });
            m_input_countdown_timer.Stop();
            if (input_req->show_expiry_msg) {
                auto expire_msg = input_req->expiry_msg;
                if (!expire_msg.empty()) {
                    // HACK: Temporarily disable auto redraw to work around game output flashing issue
                    auto prev_redraw = std::exchange(m_auto_redraw, false);
                    auto se_prev_redraw = tenkai::cpp_utils::ScopeExit([&] {
                        m_auto_redraw = prev_redraw;
                    });

                    RoutinePrint(expire_msg, ERA_PEF_IS_LINE);
                    // HACK: Enforce a redraw so that we won't lose texts because of
                    //       engine-thread-queued UI work running too early
                    if (m_auto_redraw) {
                        m_sd->ui_redraw_block_engine.store(true, std::memory_order_relaxed);
                    }
                }
            }
        }
        else {
            // Not expired, print messages if needed
            if (input_req->show_time_prompt) {
                auto prompt = format(L"还剩 {:.1f} 秒",
                    std::chrono::duration<double>(remaining_time).count()
                );
                if (m_input_last_prompt != prompt) {
                    m_input_last_prompt = prompt;
                    RoutineReuseLastLine(prompt);
                    UpdateEngineImageOutputLayout(false);
                }
            }
        }
    }
    void EngineControl::HandleEngineLeftClick(Windows::Foundation::Point groundtruth_pt, Windows::Foundation::Point engine_pt) {
        m_cur_pointer.pt = groundtruth_pt;
        auto pt = engine_pt;
        UpdateAndInvalidateActiveButton(pt);
        bool handled_button{};
        if (m_cur_active_button.line < size(m_ui_lines)) {
            auto const& cur_line = m_ui_lines[m_cur_active_button.line];
            if (m_cur_active_button.button_idx < size(cur_line.buttons)) {
                auto const& cur_button = cur_line.buttons[m_cur_active_button.button_idx];
                // Clicking a button
                handled_button = true;
                auto input_button_fn = [this](EngineUIPrintLineDataButton::InputButton const& v) -> fire_and_forget {
                    auto input_tb = UserInputTextBox();
                    auto old_text = input_tb.Text();
                    input_tb.Text(v.input);
                    TryFulfillInputRequest(false);
                    input_tb.Text(old_text);
                    co_return;
                };
                auto source_button_fn = [this](EngineUIPrintLineDataButton::SourceButton const& v) -> fire_and_forget {
                    ContentDialog cd;
                    auto cmd = std::format(L"/C code -g \"{}:{}:{}\"", v.path, v.line, v.column);
                    cd.XamlRoot(XamlRoot());
                    cd.Title(box_value(L"执行外部命令?"));
                    cd.Content(box_value(winrt::format(L""
                        "MEraEmu 将在你的系统中执行以下 Shell 命令:\n{}\n"
                        "如果不信任此命令, 请不要继续。确实要继续执行吗?",
                        std::wstring_view{ cmd }.substr(3)
                    )));
                    cd.PrimaryButtonText(L"是, 执行");
                    cd.CloseButtonText(L"取消");
                    cd.DefaultButton(ContentDialogButton::Primary);
                    cd.PrimaryButtonClick([&](auto&&, auto&&) {
                        STARTUPINFOW si{ sizeof si };
                        PROCESS_INFORMATION pi;
                        bool succeeded = CreateProcessW(
                            L"C:\\Windows\\System32\\cmd.exe", cmd.data(),
                            nullptr, nullptr,
                            false, CREATE_NO_WINDOW,
                            nullptr, nullptr,
                            &si,
                            &pi
                        );
                        if (succeeded) {
                            CloseHandle(pi.hProcess);
                            CloseHandle(pi.hThread);
                        }
                    });
                    co_await cd.ShowAsync();
                };
                auto engine_error_control_button_fn = [this](EngineUIPrintLineDataButton::EngineErrorControlButton const& v) -> fire_and_forget {
                    using ButtonType = EngineUIPrintLineDataButton::EngineErrorControlButton::ButtonType;
                    // Remove the button
                    // NB: Must copy v.type before RoutineClearLine not to invalidate the reference
                    auto type = v.type;
                    RoutinePrint({}, ERA_PEF_IS_LINE);
                    RoutineClearLine(1);
                    if (type == ButtonType::Retry) {
                        QueueEngineTask(EngineThreadTaskKind::ClearHaltState);
                    }
                    else if (type == ButtonType::Continue) {
                        QueueEngineFuncTask([](MEraEngine const& engine) {
                            engine.goto_next_safe_ip();
                        }, true);
                    }
                    co_return;
                };
                std::visit(overloaded{ input_button_fn, source_button_fn, engine_error_control_button_fn }, cur_button.data);
            }
        }
        if (!handled_button) {
            // Bring to bottom
            RootScrollViewer().ChangeView(nullptr, 1e100, nullptr, true);
            // Try to skip input requests
            if (auto& input_req = m_outstanding_input_req) {
                if (input_req->can_click) {
                    input_req->try_fulfill_void();
                    input_req = nullptr;
                }
            }
        }
    }
    bool EngineControl::FlushCurrentPrintLine(bool force_push) {
        if (!force_push && m_cur_composing_line.parts.empty()) { return false; }

        // Handle reused last line
        if (m_reused_last_line) {
            RoutineClearLine(1);
            m_reused_last_line = false;
        }

        com_ptr<IDWriteTextFormat> txt_fmt;
        try {
            txt_fmt.copy_from(GetOrCreateTextFormat(m_cur_font_name));
        }
        catch (...) {
            // Font does not exist, fallback
            txt_fmt.copy_from(GetOrCreateTextFormat({}));
        }

        auto line_i = size(m_ui_lines);

        m_ui_lines.emplace_back(hstring{});
        auto& cur_line = m_ui_lines.back();
        switch (m_cur_line_alignment) {
        case 0:
            cur_line.alignment = HorizontalAlignment::Left;
            break;
        case 1:
            cur_line.alignment = HorizontalAlignment::Center;
            break;
        case 2:
            cur_line.alignment = HorizontalAlignment::Right;
            break;
        }
        uint32_t implicit_button_scan_start{};
        auto materialize_implicit_buttons_fn = [&] {
            if (implicit_button_scan_start >= cur_line.txt.size()) { return; }
            std::wstring_view cur_str{ cur_line.txt };
            cur_str = cur_str.substr(implicit_button_scan_start);

            // PRECONDITION: it != rend(vec)
            auto vec_idx_from_rev_it_fn = [](auto const& vec, auto const& it) {
                return (uint32_t)(std::distance(it, vec.rend()) - 1);
            };
            struct FindResult {
                uint32_t start_pos, end_pos;
                int64_t value;
            };
            // Corresponding regex: \[ *-?\d+ *\]
            auto find_fn = [&]() -> std::optional<FindResult> {
                if (cur_str.empty()) { return std::nullopt; }
                auto ib = rbegin(cur_str), ie = rend(cur_str);
                auto it = ib;
#define FIND_ADVANCE(it) do { if (++(it) == ie) { return std::nullopt; } } while (0)
                while (true) {
                    while (*it != L']') { FIND_ADVANCE(it); }
                    // Read ']'
                    auto it_end = it;
                    FIND_ADVANCE(it);
                    while (*it == L' ') { FIND_ADVANCE(it); }
                    // Read digits
                    if (!(L'0' <= *it && *it <= L'9')) { continue; }
                    auto it_num_end = it;
                    while (L'0' <= *it && *it <= L'9') { FIND_ADVANCE(it); }
                    // Read '-'
                    if (*it == L'-') { FIND_ADVANCE(it); }
                    auto it_num_start = it - 1;
                    while (*it == L' ') { FIND_ADVANCE(it); }
                    // Read '['
                    if (*it != L'[') { continue; }
                    // Accept
                    auto num_start_pos = vec_idx_from_rev_it_fn(cur_str, it_num_start);
                    auto num_end_pos = vec_idx_from_rev_it_fn(cur_str, it_num_end) + 1;
                    auto num_len = num_end_pos - num_start_pos;
                    if (auto value = parse<int64_t>(cur_str.substr(num_start_pos, num_len))) {
                        return FindResult{
                            .start_pos = vec_idx_from_rev_it_fn(cur_str, it),
                            .end_pos = vec_idx_from_rev_it_fn(cur_str, it_end) + 1,
                            .value = *value,
                        };
                    }
                    // Value overflowed; don't treat it as a valid button
                    continue;
                }
#undef FIND_ADVANCE
            };
            auto opt_find_result = find_fn();
            while (opt_find_result) {
                auto& find_result = *opt_find_result;
                EngineUIPrintLineDataButton btn_data{
                    .starti = find_result.start_pos,
                    .len = find_result.end_pos - find_result.start_pos,
                    .data = EngineUIPrintLineDataButton::InputButton{ to_hstring(find_result.value) }
                };
                if (find_result.end_pos != cur_str.size()) {
                    // Button `[D] XXX`
                    btn_data.len = cur_str.size() - find_result.start_pos;
                    btn_data.starti += implicit_button_scan_start;
                    cur_line.buttons.push_back(std::move(btn_data));

                    cur_str = cur_str.substr(0, find_result.start_pos);
                    opt_find_result = find_fn();
                }
                else {
                    // Button `XXX [D]`
                    cur_str = cur_str.substr(0, find_result.start_pos);
                    opt_find_result = find_fn();
                    if (opt_find_result) {
                        cur_str = cur_str.substr(0, opt_find_result->end_pos);
                        auto endi = btn_data.starti + btn_data.len;
                        btn_data.starti = opt_find_result->end_pos;
                        btn_data.len = endi - btn_data.starti;
                    }
                    else {
                        auto endi = btn_data.starti + btn_data.len;
                        btn_data.starti = 0;
                        btn_data.len = endi;
                    }
                    btn_data.starti += implicit_button_scan_start;
                    cur_line.buttons.push_back(std::move(btn_data));
                }
            }

            implicit_button_scan_start = cur_line.txt.size();
        };
        for (auto& part : m_cur_composing_line.parts) {
            if (part.forbid_button || part.is_isolated) {
                // End current part and handle buttons
                materialize_implicit_buttons_fn();
            }

            auto starti = cur_line.txt.size();
            // TODO: Improve string concatenation performance
            cur_line.txt = cur_line.txt + part.str;
            // Explicitly add styles if not default
            if (part.color != to_u32(m_app_settings->GameForegroundColor())) {
                cur_line.styles.push_back({
                    .starti = starti, .len = part.str.size(),
                    .data = EngineUIPrintLineDataStyle::Color(part.color)
                    });
            }
            if (part.style != 0) {
                cur_line.styles.push_back({
                    .starti = starti, .len = part.str.size(),
                    .data = EngineUIPrintLineDataStyle::Style(part.style)
                    });
            }
            if (part.font_name != m_app_settings->GameDefaultFontName()) {
                cur_line.styles.push_back({
                    .starti = starti, .len = part.str.size(),
                    .data = EngineUIPrintLineDataStyle::Font(part.font_name)
                    });
            }
            if (part.inline_obj) {
                cur_line.inline_objs.push_back({
                    .starti = starti, .len = part.str.size(),
                    .obj = std::move(part.inline_obj)
                    });
            }

            // Handle part flags
            if (part.forbid_button) {
                // Skip current part
                implicit_button_scan_start = cur_line.txt.size();
            }
            if (part.is_isolated) {
                // Finish current part early
                materialize_implicit_buttons_fn();
            }
        }
        materialize_implicit_buttons_fn();
        for (auto& button : m_cur_composing_line.explicit_buttons) {
            cur_line.buttons.push_back(std::move(button));
        }

        // Reset composing line
        m_cur_composing_line = {};

        cur_line.ensure_layout(this, m_ui_param_cache.canvas_width_px);
        cur_line.flush_effects(this);
        cur_line.flush_metrics(this);
        {
            // Invalidate the area of the line (and the previous line, if overlapping)
            size_t pos = cur_line.render_backward_height >= line_i ? 0 : line_i - cur_line.render_backward_height;
            auto& line = m_ui_lines[pos];
            auto new_height = (line.acc_height - line.height) * m_ui_param_cache.line_height_px_f;
            m_last_redraw_dirty_height = std::min(m_last_redraw_dirty_height, (uint32_t)new_height);
        }
        UpdateLineAccMetrics(line_i);

        m_cur_printc_count = 0;

        // Truncate too long history
        auto cur_line_count = GetAccUIHeightInLines();
        if (cur_line_count > m_app_settings->GameHistoryLinesCount()) {
            // No need to invalidate the area of the removed lines; they must be already out of view
            uint32_t start_idx = cur_line_count - m_app_settings->GameHistoryLinesCount();
            while (start_idx > 0) {
                start_idx--;
                auto& cur_line = m_ui_lines[start_idx];
                bool is_empty = cur_line.is_empty();
                cur_line.put_empty(this);
                if (is_empty) { break; }
            }
        }

        return true;
    }
    void EngineControl::RoutinePrint(hstring content, PrintExtendedFlags flags) {
        if (GetEffectiveSkipDisplay()) {
            // Engine requested to skip output
            return;
        }

        bool updated = false;

        uint32_t color;
        if (flags & ERA_PEF_IGNORE_COLOR) {
            color = to_u32(m_app_settings->GameForegroundColor());
        }
        else {
            color = to_u32(EngineForeColor());
        }
        bool forbid_button{};
        bool is_isolated{};
        if (flags & ERA_PEF_IS_SINGLE) {
            updated = FlushCurrentPrintLine() || updated;
        }
        if (flags & (ERA_PEF_LEFT_PAD | ERA_PEF_RIGHT_PAD)) {
            // PRINTC / PRINTLC
            is_isolated = true;

            bool align_right = flags & ERA_PEF_RIGHT_PAD;
            auto str_width = rust_get_wstring_width((const uint16_t*)content.c_str());
            const auto printc_char_count = m_app_settings->GamePrintCCharCount();
            if (str_width < printc_char_count) {
                int space_cnt = printc_char_count - str_width;
                if (align_right) {
                    content = format(L"{:{}}{}", L"", space_cnt, content);
                }
                else {
                    content = format(L"{}{:{}}", content, L"", space_cnt);
                }
            }
            m_cur_printc_count++;
        }
        if (flags & ERA_PEF_FORCE_PLAIN) {
            forbid_button = true;
        }
        std::wstring_view content_sv{ content };
        size_t newline_pos;
        while ((newline_pos = content_sv.find(L'\n')) != content_sv.npos) {
            auto subsv = content_sv.substr(0, newline_pos);
            content_sv = content_sv.substr(newline_pos + 1);
            if (!subsv.empty()) {
                m_cur_composing_line.parts.push_back({
                    .str = hstring(subsv), .color = color,
                    .style = m_cur_font_style, .font_name = m_cur_font_name,
                    .forbid_button = forbid_button, .is_isolated = is_isolated });
            }
            updated = FlushCurrentPrintLine(true) || updated;
        }
        if (!content_sv.empty()) {
            m_cur_composing_line.parts.push_back({
                .str = hstring(content_sv), .color = color,
                .style = m_cur_font_style, .font_name = m_cur_font_name,
                .forbid_button = forbid_button, .is_isolated = is_isolated });
        }
        if (flags & ERA_PEF_IS_LINE || m_cur_printc_count >= m_app_settings->GamePrintCCountPerLine()) {
            updated = FlushCurrentPrintLine(true) || updated;
        }
        else if (flags & ERA_PEF_IS_SINGLE) {
            updated = FlushCurrentPrintLine() || updated;
        }

        // Resize to trigger updates
        if (updated && m_auto_redraw) {
            UpdateEngineImageOutputLayout(false);
        }
    }
    void EngineControl::RoutineHtmlPrint(hstring const& content, int64_t no_single) {
        if (!m_app_settings->EnableHtmlRendering()) {
            RoutinePrint(content, ERA_PEF_FORCE_PLAIN | ERA_PEF_IS_LINE);
            return;
        }

        using Parser = SimpleHtmlParser<wchar_t>;
        Parser parser(content);
        //try { parser.check_syntax(); }
        //catch (...) {
        //    // Syntax error; fallback to plain text
        //    RoutinePrint(content, ERA_PEF_FORCE_PLAIN | ERA_PEF_IS_LINE);
        //    return;
        //}
        //parser.reset();

        auto gather_to_part_fn = [&](ComposingLineData::DataPart& part) {
            part.color = to_u32(m_app_settings->GameForegroundColor());
            part.style = 0;
            part.font_name = m_app_settings->GameDefaultFontName();

            // TODO: button.title, nonbutton.title

            // Collect current part data from html tags
            for (auto const& tag : parser.current_tags()) {
                if (tag.name == L"font") {
                    if (auto it = tag.attrs.find(L"face"); it != tag.attrs.end()) {
                        part.font_name = it->second;
                    }
                    if (auto it = tag.attrs.find(L"color"); it != tag.attrs.end()) {
                        part.color = parse_color_string(it->second) | 0xff000000;
                    }
                    // TODO: font.bcolor
                }
                else if (tag.name == L"b") {
                    part.style |= ERA_FONT_STYLE_BOLD;
                }
                else if (tag.name == L"i") {
                    part.style |= ERA_FONT_STYLE_ITALIC;
                }
                else if (tag.name == L"u") {
                    part.style |= ERA_FONT_STYLE_UNDERLINE;
                }
                else if (tag.name == L"s") {
                    part.style |= ERA_FONT_STYLE_STRIKEOUT;
                }
            }
        };

        bool updated = false;
        // NOTE: HTML_PRINT is SINGLE by default
        if (!no_single) {
            updated = FlushCurrentPrintLine() || updated;
        }
        try {
            auto ui_lines_guard = MakeUILinesSnapshotGuard();

            auto old_alignment = m_cur_line_alignment;
            auto old_font_name = std::move(m_cur_font_name);
            auto se_restore = tenkai::cpp_utils::ScopeExit([&] {
                m_cur_line_alignment = old_alignment;
                m_cur_font_name = std::move(old_font_name);
            });

            PrintButtonRegionContext ctx;

            bool last_is_br{};
            parser.parse([&](Parser::VisitEventKind event_kind, Parser::Tag const& tag, param::hstring const& param_text) {
                hstring const& text = param_text;
                if (event_kind == Parser::VisitEventKind::EnterTag) {
                    last_is_br = false;

                    if (tag.name == L"p") {
                        // Paragraph introduces a new line
                        updated = FlushCurrentPrintLine() || updated;
                    }
                    else if (tag.name == L"br") {
                        // Soft line break; do not flush current line
                        m_cur_composing_line.parts.push_back({ .str = hstring(L"\n"), .forbid_button = true });
                        last_is_br = true;
                    }
                    else if (tag.name == L"button") {
                        ctx = BeginPrintButtonRegion();
                    }
                    else if (tag.name == L"shape") {
                        auto& type = tag.attrs.at(L"type");
                        if (type == L"rect") {
                            // Rectangle shape
                            auto param = parse_comma_separated_ui_length(tag.attrs.at(L"param"), 1, 4);
                            com_ptr<EngineUIPrintLineDataInlineObject::InlineObject> obj;
                            if (param.size() == 1) {
                                EngineUILength x, y, width, height;
                                x = y = EngineUILength::FontPercent(0);
                                width = param[0];
                                height = EngineUILength::FontPercent(100);
                                obj = make_self<EngineUIPrintLineDataInlineObject::InlineObject>(
                                    this, EngineUIPrintLineDataInlineObject::ShapeRect{
                                        .x = x, .y = y, .width = width, .height = height
                                    }
                                );
                            }
                            else if (param.size() == 4) {
                                obj = make_self<EngineUIPrintLineDataInlineObject::InlineObject>(
                                    this, EngineUIPrintLineDataInlineObject::ShapeRect{
                                        .x = param[0], .y = param[1], .width = param[2], .height = param[3]
                                    }
                                );
                            }
                            else {
                                throw std::runtime_error("Invalid shape rect param count");
                            }
                            if (obj) {
                                m_cur_composing_line.parts.push_back({
                                    .str = hstring(L"■"), .inline_obj = std::move(obj), .forbid_button = true,
                                    });
                                gather_to_part_fn(m_cur_composing_line.parts.back());
                            }
                        }
                        else if (type == L"space") {
                            // Space shape
                            auto param = parse_comma_separated_ui_length(tag.attrs.at(L"param"), 1, 1);
                            // HACK: Skip if width is 0 to avoid game bugs
                            if (param[0].len != 0) {
                                auto obj = make_self<EngineUIPrintLineDataInlineObject::InlineObject>(
                                    this, EngineUIPrintLineDataInlineObject::ShapeSpace{ .size = param[0] }
                                );
                                m_cur_composing_line.parts.push_back({
                                    .str = hstring(L"�"), .inline_obj = std::move(obj), .forbid_button = true,
                                    });
                                gather_to_part_fn(m_cur_composing_line.parts.back());
                            }
                        }
                    }
                }
                else if (event_kind == Parser::VisitEventKind::LeaveTag || event_kind == Parser::VisitEventKind::ImplicitlyLeaveTag) {
                    if (tag.name == L"p") {
                        // Parse align
                        if (auto it = tag.attrs.find(L"align"); it != tag.attrs.end()) {
                            if (it->second == L"center") {
                                m_cur_line_alignment = 1;
                            }
                            else if (it->second == L"right") {
                                m_cur_line_alignment = 2;
                            }
                            else if (it->second == L"left") {
                                m_cur_line_alignment = 0;
                            }
                            else {
                                throw std::runtime_error("Invalid align value");
                            }
                        }

                        if (last_is_br) {
                            // Ignore empty lines after <br>
                            last_is_br = false;
                            m_cur_composing_line.parts.pop_back();
                        }

                        updated = FlushCurrentPrintLine() || updated;
                    }
                    else if (tag.name == L"button") {
                        hstring value;
                        if (auto it = tag.attrs.find(L"value"); it != tag.attrs.end()) {
                            value = it->second;
                        }
                        EndPrintButtonRegion(ctx, [&] {
                            return EngineUIPrintLineDataButton::InputButton{ .input = value };
                        });
                    }
                    else if (tag.name == L"img") {
                        hstring sprite_name;
                        sprite_name = tag.attrs.at(L"src");
                        auto sprite_it = m_sprite_objects.find(sprite_name);
                        if (sprite_it != m_sprite_objects.end()) {
                            auto& sprite = sprite_it->second;
                            EngineUILength width, height = EngineUILength::FontPercent(100), ypos;
                            if (auto it = tag.attrs.find(L"ypos"); it != tag.attrs.end()) {
                                ypos = parse_comma_separated_ui_length(it->second, 1, 1)[0];
                            }
                            if (auto it = tag.attrs.find(L"height"); it != tag.attrs.end()) {
                                height = parse_comma_separated_ui_length(it->second, 1, 1)[0];
                            }
                            if (auto it = tag.attrs.find(L"width"); it != tag.attrs.end()) {
                                width = parse_comma_separated_ui_length(it->second, 1, 1)[0];
                            }
                            else {
                                width = EngineUILength((float)height.len * sprite.width / sprite.height, height.type);
                            }
                            auto obj = make_self<EngineUIPrintLineDataInlineObject::InlineObject>(
                                this, EngineUIPrintLineDataInlineObject::Image{
                                    .sprite = sprite_name, .width = width, .height = height, .ypos = ypos
                                }
                            );
                            m_cur_composing_line.parts.push_back({
                                .str = hstring(L"�"), .inline_obj = std::move(obj), .forbid_button = true,
                                });
                            gather_to_part_fn(m_cur_composing_line.parts.back());
                        }
                        else {
                            // Sprite does not exist or is invalid; print html content as plain text
                            auto start_pos = tag.start_tag_pos_range.first;
                            auto end_pos = tag.end_tag_pos_range.second;
                            hstring html(std::wstring_view(content).substr(start_pos, end_pos - start_pos));
                            m_cur_composing_line.parts.push_back({ .str = html, .forbid_button = true });
                            gather_to_part_fn(m_cur_composing_line.parts.back());
                        }
                    }
                }
                else if (event_kind == Parser::VisitEventKind::OnText) {
                    last_is_br = false;

                    m_cur_composing_line.parts.push_back({ .str = text, .forbid_button = true });
                    gather_to_part_fn(m_cur_composing_line.parts.back());
                }
            });

            // Finish HTML printing
            ui_lines_guard.release();

            if (!no_single) {
                updated = FlushCurrentPrintLine() || updated;
            }
        }
        catch (...) {
            // Syntax error; recover and fallback to plain text
            RoutinePrint(content, ERA_PEF_FORCE_PLAIN | ERA_PEF_IS_LINE);
            return;
        }

        // Resize to trigger updates
        if (updated && m_auto_redraw) {
            UpdateEngineImageOutputLayout(false);
        }
    }
    hstring EngineControl::RoutineHtmlPopPrintingStr() {
        if (!FlushCurrentPrintLine()) {
            return {};
        }
        auto se_pop_line = tenkai::cpp_utils::ScopeExit([&] {
            m_ui_lines.pop_back();
        });
        return RoutineHtmlGetPrintedStr(0);
    }
    hstring EngineControl::RoutineHtmlGetPrintedStr(int64_t line_no) {
        if (line_no < 0 || (size_t)line_no >= size(m_ui_lines)) {
            return {};
        }
        return m_ui_lines[line_no].to_html_string();
    }
    int64_t EngineControl::RoutineHtmlStringLen(hstring const& content, bool return_pixel) {
        using Parser = SimpleHtmlParser<wchar_t>;
        Parser parser(content);

        // Temporarily render the html content to calculate the width
        size_t cur_line_no = size(m_ui_lines);
        auto ui_lines_guard = MakeUILinesSnapshotGuard();
        auto se_auto_redraw = tenkai::cpp_utils::ScopeExit([&, old_auto_redraw = m_auto_redraw] {
            m_auto_redraw = old_auto_redraw;
        });
        m_cur_composing_line = {};
        m_auto_redraw = false;

        RoutineHtmlPrint(content, 0);

        if (cur_line_no >= size(m_ui_lines)) {
            return {};
        }
        float total_width = m_ui_lines[cur_line_no].get_actual_width() / m_ui_param_cache.ui_scale;
        return (int64_t)std::ceil(return_pixel ? total_width : (total_width * 2.0f / m_ui_param_cache.font_size_px_f));
    }
    void EngineControl::RoutineInput(std::unique_ptr<InputRequest> request) {
        assert(request);

        if (!m_engine_task_tx.is_vacant()) {
            // Abandon the input request until the engine has processed the task we sent
            return;
        }

        // Flush intermediate print contents first
        FlushCurrentPrintLine();
        UpdateEngineImageOutputLayout(false);

        m_input_start_t = winrt::clock::now();
        m_input_last_prompt = {};
        m_input_countdown_timer.Stop();
        auto& input_req = m_outstanding_input_req;
        input_req = std::move(request);
        // Break user skip if requested
        if (input_req->break_user_skip) {
            m_user_skipping = false;
        }
        // Check if we can fulfill the input request immediately
        if (input_req->is_one) {
            if (TryFulfillInputRequest(false)) {
                UserInputTextBox().Text({});
                return;
            }
        }
        // If user is skipping, skip current request
        if ((m_user_skipping || m_cur_pointer.right_button_down) && input_req->can_skip) {
            m_user_skipping = true;
            std::exchange(input_req, nullptr)->try_fulfill_void();
            return;
        }
        else {
            m_user_skipping = false;
        }
        // Handle timed input
        if (input_req->time_limit >= 0) {
            // TODO: More precise countdown handling
            if (input_req->time_limit > 0) {
                m_input_countdown_timer.Start();
            }
            OnInputCountDownTick(nullptr, nullptr);
        }
    }
    void EngineControl::RoutineReuseLastLine(hstring const& content) {
        FlushCurrentPrintLine();

        if (m_reused_last_line) {
            RoutineClearLine(1);
            m_reused_last_line = false;
        }
        // TODO: Cut off newlines in content
        RoutinePrint(content, ERA_PEF_IS_LINE);
        // Mark as reused last line
        m_reused_last_line = true;
    }
    void EngineControl::RoutineClearLine(uint64_t count) {
        auto old_count = (uint64_t)GetCurrentUILinesCount();
        if (count == 0 || old_count == 0) { return; }
        if (count > old_count) {
            count = old_count;
        }

        // NOTE: Actual removal of lines is handled by `UpdateEngineImageOutputLayout` now.
        //       We only soft-delete the lines here.
        if (m_soft_deleted_ui_lines_count > 0) {
            // Maybe a sequence of PRINT-CLEARLINE-PRINT-CLEARLINE... mixed calls.
            // Pop the intermediate not-yet-drawn lines first.
            auto intermediate_count = size(m_ui_lines) - m_soft_deleted_ui_lines_pos;
            auto erase_count = std::min(count, intermediate_count);
            auto ib = begin(m_ui_lines) + m_soft_deleted_ui_lines_pos;
            auto ie = ib + erase_count;
            m_ui_lines.erase(ib, ie);
            count -= erase_count;
        }
        if (m_soft_deleted_ui_lines_count == 0) {
            // First CLEARLINE call, the simplest case.
            m_soft_deleted_ui_lines_pos = size(m_ui_lines);
        }
        m_soft_deleted_ui_lines_count += count;

        if (m_auto_redraw) {
            UpdateEngineImageOutputLayout(false);
        }
    }
    void EngineControl::RoutinePrintSourceButton(hstring const& content, hstring const& path,
        uint32_t line, uint32_t column, PrintExtendedFlags flags
    ) {
        RoutinePrintButtonGeneric(content, flags, EngineUIPrintLineDataButton::SourceButton{ path, line, column });
    }
    void EngineControl::RoutinePrintButton(hstring const& content, hstring const& value, PrintExtendedFlags flags) {
        RoutinePrintButtonGeneric(content, flags, EngineUIPrintLineDataButton::InputButton{ value });
    }
    void EngineControl::RoutinePrintEngineErrorControlButton(hstring const& content, EngineUIPrintLineDataButton::EngineErrorControlButton::ButtonType type) {
        RoutinePrintButtonGeneric(content, 0, EngineUIPrintLineDataButton::EngineErrorControlButton{ type });
    }
    void EngineControl::RoutinePrintButtonGeneric(hstring const& content, PrintExtendedFlags flags, EngineUIPrintLineDataButton::ButtonData data) {
        if (content.empty()) { return; }
        auto ctx = BeginPrintButtonRegion();
        RoutinePrint(content, flags | ERA_PEF_FORCE_PLAIN);
        EndPrintButtonRegion(ctx, [&] { return data; });
    }
    auto EngineControl::BeginPrintButtonRegion() -> PrintButtonRegionContext {
        uint32_t ui_line_start = size(m_ui_lines) - m_reused_last_line;
        uint32_t ui_line_offset{};
        for (auto const& part : m_cur_composing_line.parts) {
            ui_line_offset += part.str.size();
        }
        return PrintButtonRegionContext{ ui_line_start, ui_line_offset };
    }
    template<typename F>
    void EngineControl::EndPrintButtonRegion(PrintButtonRegionContext ctx, F&& f) {
        // Apply to composed lines
        for (size_t i = ctx.ui_line_start; i < size(m_ui_lines); i++) {
            auto& line_data = m_ui_lines[i];
            line_data.buttons.push_back(EngineUIPrintLineDataButton{
                .starti = ctx.ui_line_offset,
                .len = line_data.txt.size() - ctx.ui_line_offset,
                .data = f(),
                });
            ctx.ui_line_offset = 0;
        }
        // Apply to current composing line
        if (!m_cur_composing_line.parts.empty()) {
            uint32_t cur_line_len{};
            for (auto& part : m_cur_composing_line.parts) {
                cur_line_len += part.str.size();
            }
            m_cur_composing_line.explicit_buttons.push_back(EngineUIPrintLineDataButton{
                .starti = ctx.ui_line_offset,
                .len = cur_line_len - ctx.ui_line_offset,
                .data = f(),
                });
        }
    }
    int64_t EngineControl::RoutineGCreate(int64_t gid, int64_t width, int64_t height) {
        if (gid <= 0) {
            // Invalid gid
            return 0;
        }
        if (m_graphics_objects.contains(gid)) {
            // Already exists
            return 0;
        }

        m_graphics_objects.insert({ gid, GraphicsObject((uint32_t)width, (uint32_t)height) });

        return 1;
    }
    int64_t EngineControl::RoutineGCreateFromFile(int64_t gid, hstring const& path) {
        if (gid <= 0) {
            // Invalid gid
            return 0;
        }
        if (m_graphics_objects.contains(gid)) {
            // Already exists
            return 0;
        }

        m_graphics_objects.insert({ gid, GraphicsObject(path) });

        return 1;
    }
    int64_t EngineControl::RoutineGDispose(int64_t gid) {
        return m_graphics_objects.erase(gid);
    }
    int64_t EngineControl::RoutineGCreated(int64_t gid) {
        return m_graphics_objects.contains(gid);
    }
    int64_t EngineControl::RoutineGDrawSprite(int64_t gid, hstring const& sprite_name, int64_t dest_x, int64_t dest_y, int64_t dest_width, int64_t dest_height, EraColorMatrix_t const* color_matrix) {
        auto graphics_it = m_graphics_objects.find(gid);
        if (graphics_it == m_graphics_objects.end()) {
            // Invalid gid
            return 0;
        }
        auto& graphics = graphics_it->second;
        if (!graphics.try_ensure_loaded(this)) {
            return false;
        }

        auto sprite_it = m_sprite_objects.find(sprite_name);
        if (sprite_it == m_sprite_objects.end()) {
            // Invalid sprite name
            return 0;
        }
        auto& sprite = sprite_it->second;
        if (!sprite.try_ensure_loaded(this)) {
            return false;
        }
        auto& sprite_graphics = sprite.get_graphics_object(this);

        // Draw the sprite to the current graphics object
        if (dest_width == -1) {
            dest_width = graphics.get_dimensions().first;
        }
        if (dest_height == -1) {
            dest_height = graphics.get_dimensions().second;
        }
        m_d2d_ctx->SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
        m_d2d_ctx->SetTarget(graphics.bitmap.get());
        return RunDrawingSession([&](ID2D1DeviceContext3* ctx) {
            if (color_matrix && !is_identity(*color_matrix)) {
                // Slow path: draw with color matrix effect
                // TODO: Implement color matrix (https://learn.microsoft.com/en-us/windows/win32/direct2d/color-matrix)
                RoutinePrint(L"UI ERROR: Color matrix is not supported yet.", ERA_PEF_IS_LINE);
                return;
            }
            else {
                // Fast path: draw bitmap directly
                D2D1_RECT_F rect;
                rect.left = float(dest_x);
                rect.top = float(dest_y);
                rect.right = float(rect.left + dest_width);
                rect.bottom = float(rect.top + dest_height);
                D2D1_RECT_U src_rect;
                src_rect.left = saturate_to_u32(sprite.x);
                src_rect.top = saturate_to_u32(sprite.y);
                src_rect.right = saturate_to_u32(src_rect.left + sprite.width);
                src_rect.bottom = saturate_to_u32(src_rect.top + sprite.height);
                auto const& sb = m_d2d_sprite_batch;
                sb->Clear();
                sb->AddSprites(1, &rect, &src_rect);
                ctx->DrawSpriteBatch(sb.get(), sprite_graphics.bitmap.get(),
                    D2D1_BITMAP_INTERPOLATION_MODE_LINEAR, D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE);
            }
        });
    }
    int64_t EngineControl::RoutineGClear(int64_t gid, uint32_t color) {
        auto graphics_it = m_graphics_objects.find(gid);
        if (graphics_it == m_graphics_objects.end()) {
            // Invalid gid
            return 0;
        }
        auto& graphics = graphics_it->second;
        if (!graphics.try_ensure_loaded(this)) {
            return false;
        }

        // Clear the bitmap to the specified color
        m_d2d_ctx->SetTarget(graphics.bitmap.get());
        return RunDrawingSession([&](ID2D1DeviceContext3* ctx) {
            ctx->Clear(D2D1::ColorF(color));
        });
    }
    int64_t EngineControl::RoutineSpriteCreate(hstring const& name, int64_t gid, int64_t x, int64_t y, int64_t width, int64_t height) {
        if (m_sprite_objects.contains(name)) {
            // Already exists
            return 0;
        }
        auto graphics_it = m_graphics_objects.find(gid);
        if (graphics_it == m_graphics_objects.end()) {
            // Invalid gid
            return 0;
        }
        auto& graphics = graphics_it->second;

        // Adjust width and height if negative
        if (width == -1) {
            if (!graphics.try_ensure_loaded(this)) {
                return false;
            }
            width = graphics.get_dimensions().first;
        }
        if (height == -1) {
            if (!graphics.try_ensure_loaded(this)) {
                return false;
            }
            height = graphics.get_dimensions().second;
        }

        m_sprite_objects.insert({ name, SpriteObject(gid, (int32_t)x, (int32_t)y, (int32_t)width, (int32_t)height) });
        return 1;
    }
    int64_t EngineControl::RoutineSpriteDispose(hstring const& name) {
        return m_sprite_objects.erase(name);
    }
    int64_t EngineControl::RoutineSpriteCreated(hstring const& name) {
        return m_sprite_objects.contains(name);
    }
    int64_t EngineControl::RoutineSpriteWidth(hstring const& name) {
        auto it = m_sprite_objects.find(name);
        if (it == m_sprite_objects.end()) {
            return 0;
        }
        return it->second.width;
    }
    int64_t EngineControl::RoutineSpriteHeight(hstring const& name) {
        auto it = m_sprite_objects.find(name);
        if (it == m_sprite_objects.end()) {
            return 0;
        }
        return it->second.height;
    }
    int64_t EngineControl::RoutinePlaySound(hstring const& path, int64_t loop_count, bool is_bgm) {
        // Do not handle sound if sound functions are disabled
        if (!m_app_settings->ReadSoundDir()) { return -1; }

        if (!m_sound.hub) {
            [](com_ptr<EngineControl> self, hstring path, int64_t loop_count, bool is_bgm) -> util::winrt::fire_forget {
                try {
                    co_await self->m_sound.ensure_inited_async(self.get());
                    self->RoutinePlaySound(path, loop_count, is_bgm);
                }
                catch (...) {
                    // Failed to initialize sound system
                    self->EmitUnhandledExceptionEvent(std::current_exception());
                }
            }(get_strong(), path, loop_count, is_bgm);
            return 0;
        }

        auto real_path = winrt::format(L"{}sound\\{}", m_sd->game_base_dir, path);

        if (is_bgm) {
            // Play BGM
            m_sound.hub.play_bgm(real_path);
            return 0;
        }
        else {
            // Play SE
            return (int64_t)m_sound.hub.play_sfx(real_path);
        }
    }
    int64_t EngineControl::RoutineStopSound(int64_t sound_id) {
        if (!m_sound.hub) { return 0; }

        if (sound_id == 0) {
            // Stop all BGMs
            m_sound.hub.stop_bgm();
        }
        if (sound_id == INT64_MAX) {
            // Stop all SEs
            m_sound.hub.stop_all_sfx();
        }

        return 1;
    }

    int64_t EngineControl::GetCurrentUILinesCount() {
        // LogicalCount = PhysicalCount - TopEmptyLines - SoftDeletedLines
        return (int64_t)m_ui_lines.size() - 1 - m_soft_deleted_ui_lines_count;
    }
    void EngineControl::SetCurrentLineAlignment(int64_t value) {
        m_cur_line_alignment = (uint32_t)value;
    }
    int64_t EngineControl::GetCurrentLineAlignment() {
        return m_cur_line_alignment;
    }
    void EngineControl::SetCurrentFontStyle(int64_t value) {
        m_cur_font_style = (uint32_t)value;
    }
    int64_t EngineControl::GetCurrentFontStyle() {
        return m_cur_font_style;
    }
    void EngineControl::SetCurrentFontName(hstring const& value) {
        // TODO: Verify font name?
        m_cur_font_name = value.empty() ? m_app_settings->GameDefaultFontName() : value;
    }
    hstring EngineControl::GetCurrentFontName() {
        return m_cur_font_name;
    }
    void EngineControl::SetRedrawState(int64_t value) {
        if (value == 0) {
            m_auto_redraw = false;
        }
        else if (value == 1) {
            m_auto_redraw = true;
        }
        else {
            m_auto_redraw = true;
            UpdateEngineImageOutputLayout(false);
        }
    }
    int64_t EngineControl::GetRedrawState() {
        return m_auto_redraw;
    }
    void EngineControl::SetSkipDisplay(int64_t value) {
        if (value == 0) {
            m_skip_display = false;
        }
        else {
            m_skip_display = true;
        }
    }
    int64_t EngineControl::GetSkipDisplay() {
        return m_skip_display;
    }
    int64_t EngineControl::GetEffectiveSkipDisplay() {
        return m_no_skip_display_cnt > 0 ? false : GetSkipDisplay();
    }
    void EngineControl::PushNoSkipDisplay() {
        if (m_no_skip_display_cnt++ == 0) {
            // Do nothing
        }
    }
    void EngineControl::PopNoSkipDisplay() {
        if (m_no_skip_display_cnt == 0) {
            // TODO: Treat extra pops as an error?
            return;
        }
        if (--m_no_skip_display_cnt == 0) {
            // Do nothing
        }
    }

    void EngineControl::GraphicsObject::ensure_loaded(EngineControl* ctrl) {
        if (bitmap) { return; }

        auto bitmap_props = D2D1::BitmapProperties1(
            D2D1_BITMAP_OPTIONS_TARGET,
            D2D1::PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED)
        );
        if (file_path.empty()) {
            // Create an empty bitmap
            check_hresult(ctrl->m_d2d_ctx->CreateBitmap(
                D2D1::SizeU(width, height),
                nullptr, 0, bitmap_props, bitmap.put()
            ));
        }
        else {
            // Load from file
            com_ptr<IWICBitmapDecoder> decoder;
            check_hresult(g_wic_factory->CreateDecoderFromFilename(
                file_path.c_str(), nullptr, GENERIC_READ, WICDecodeMetadataCacheOnDemand, decoder.put()
            ));
            com_ptr<IWICBitmapFrameDecode> frame;
            check_hresult(decoder->GetFrame(0, frame.put()));
            com_ptr<IWICFormatConverter> converter;
            check_hresult(g_wic_factory->CreateFormatConverter(converter.put()));
            check_hresult(converter->Initialize(
                frame.get(), GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nullptr, 0.f,
                WICBitmapPaletteTypeMedianCut
            ));
            check_hresult(ctrl->m_d2d_ctx->CreateBitmapFromWicBitmap(
                converter.get(),
                bitmap_props,
                bitmap.put()
            ));
        }
    }
    bool EngineControl::GraphicsObject::try_ensure_loaded(EngineControl* ctrl) {
        if (is_bad) { return false; }
        try {
            ensure_loaded(ctrl);
            return true;
        }
        catch (...) {
            is_bad = true;
            return false;
        }
    }

    DP_DEFINE_PROP(EngineTitle, box_value(L"MEraEmu"), [](DependencyObject const& sender, DependencyPropertyChangedEventArgs const& e) {
        auto that = sender.as<EngineControl>();
        that->UpdateDevToolsWindow();
    });
}
