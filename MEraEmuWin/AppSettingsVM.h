#pragma once

#include "AppSettingsVM.g.h"

#ifndef JSON_USE_IMPLICIT_CONVERSIONS
#define JSON_USE_IMPLICIT_CONVERSIONS 0
#endif

#include <nlohmann/json.hpp>

namespace winrt::MEraEmuWin::implementation {
    struct AppSettingsVM : AppSettingsVMT<AppSettingsVM> {
        AppSettingsVM() = default;

        MEraEmuWin::AppSettingsVM DeepClone() {
            auto clone = make_self<AppSettingsVM>();
            nlohmann::json json;
            to_json(json, *this);
            json.get_to(*clone);
            return *clone;
        }

#define X_FIELDS \
    /* General */ \
    X(bool, ReadResourcesDir, read_resources_dir) \
    X(bool, ReadFontsDir, read_fonts_dir) \
    X(bool, ReadSoundDir, read_sound_dir) \
    X(bool, EnableParallelLoading, enable_parallel_loading) \
    X(bool, EnableJIT, enable_jit) \
    X(bool, EnableEngineControlOnError, enable_engine_control_on_error) \
    /* Game */ \
    X(uint32_t, SaveDataCount, save_data_count) \
    X(bool, EnableAutoSave, enable_auto_save) \
    /* Display */ \
    X(bool, AutoDetectGameUIScale, auto_detect_game_ui_scale) \
    X(double, GameUIScale, game_ui_scale) \
    X(hstring, GameDefaultFontName, game_default_font_name) \
    X(double, GameFontSize, game_font_size) \
    X(double, GameLineHeight, game_line_height) \
    X(uint32_t, GameHistoryLinesCount, game_history_lines_count) \
    X(Windows::UI::Color, GameBackgroundColor, game_background_color) \
    X(Windows::UI::Color, GameForegroundColor, game_foreground_color) \
    X(Windows::UI::Color, GameHighlightColor, game_highlight_color) \
    X(uint32_t, GamePrintCCountPerLine, game_printc_count_per_line) \
    X(uint32_t, GamePrintCCharCount, game_printc_char_count) \
    X(bool, EnableFontSmoothing, enable_font_smoothing) \
    X(bool, EnablePixelSnapping, enable_pixel_snapping) \
    X(bool, EnableHtmlRendering, enable_html_rendering) \
    X(bool, EnableGdiCompatRender, enable_gdi_compat_render) \
    X(bool, EnableHardwareAcceleration, enable_hardware_acceleration) \
    X(bool, DebugShowLayoutBounds, debug_show_layout_bounds)

#define GenGetSetter(type, name, member)    \
        type name() { return member; }      \
        void name(type value) { member = value; }

#define X GenGetSetter
        X_FIELDS;
#undef X

        friend void to_json(nlohmann::json& j, const AppSettingsVM& p) {
#define X(type, name, member) j[#member] = p.member;
            X_FIELDS;
#undef X
        }

        friend void from_json(const nlohmann::json& j, AppSettingsVM& p) {
#define X(type, name, member) \
    do { \
        if (auto it = j.find(#member); it != j.end()) { \
            it->get_to(p.member); \
        } \
    } while (0);
            X_FIELDS;
#undef X
        }

        // Non-midl methods
        std::string to_json_string() {
            nlohmann::json j;
            to_json(j, *this);
            return j.dump(4);
        }

    private:
        // General
        bool read_resources_dir = true;
        bool read_fonts_dir = true;
        bool read_sound_dir = true;
        bool enable_parallel_loading = false;
        bool enable_jit = false;
        bool enable_engine_control_on_error = false;

        // Game
        uint32_t save_data_count = 20;
        bool enable_auto_save = true;

        // Display
        bool auto_detect_game_ui_scale = true;
        double game_ui_scale = 1.0;
        hstring game_default_font_name = L"MS Gothic";
        double game_font_size = 16.0;
        double game_line_height = 16.0;
        uint32_t game_history_lines_count = 20000;
        Windows::UI::Color game_background_color = Windows::UI::Colors::Black();
        Windows::UI::Color game_foreground_color = Windows::UI::Colors::Silver();
        Windows::UI::Color game_highlight_color = Windows::UI::Colors::Yellow();
        uint32_t game_printc_count_per_line = 5;
        uint32_t game_printc_char_count = 25;
        bool enable_font_smoothing = false;
        bool enable_pixel_snapping = true;
        bool enable_html_rendering = true;
        bool enable_gdi_compat_render = false;
        bool enable_hardware_acceleration = true;
        bool debug_show_layout_bounds = false;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct AppSettingsVM : AppSettingsVMT<AppSettingsVM, implementation::AppSettingsVM> {};
}
