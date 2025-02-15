#pragma once

// A utility header that is meant to share project-specific code.

#ifndef JSON_USE_IMPLICIT_CONVERSIONS
#define JSON_USE_IMPLICIT_CONVERSIONS 0
#endif

#include <nlohmann/json.hpp>

typedef SSIZE_T ssize_t;

inline uint32_t parse_color_string(std::string const& str) {
    uint32_t v;
    if (sscanf(str.c_str(), "#%08x", &v) != 1) {
        throw std::invalid_argument("Invalid color format");
    }
    return v;
}
inline uint32_t parse_color_string(std::wstring const& str) {
    uint32_t v;
    if (swscanf(str.c_str(), L"#%08x", &v) != 1) {
        throw std::invalid_argument("Invalid color format");
    }
    return v;
}
inline std::string to_color_string(uint32_t v) {
    char buf[10];
    snprintf(buf, sizeof buf, "#%08x", v);
    return buf;
}

namespace winrt {
    inline void to_json(nlohmann::json& j, const winrt::hstring& str) {
        j = winrt::to_string(str);
    }
    inline void from_json(const nlohmann::json& j, winrt::hstring& str) {
        str = winrt::to_hstring(j.get<std::string>());
    }

    namespace Windows::UI {
        inline void to_json(nlohmann::json& j, const winrt::Windows::UI::Color& color) {
            uint32_t v = color.A << 24 | color.R << 16 | color.G << 8 | color.B;
            char buf[10];
            snprintf(buf, sizeof buf, "#%08x", v);
            j = buf;
        }
        inline void from_json(const nlohmann::json& j, winrt::Windows::UI::Color& color) {
            std::string str = j.get<std::string>();
            uint32_t v = parse_color_string(str);
            color.A = (v >> 24) & 0xFF;
            color.R = (v >> 16) & 0xFF;
            color.G = (v >> 8) & 0xFF;
            color.B = v & 0xFF;
        }
    }
}
