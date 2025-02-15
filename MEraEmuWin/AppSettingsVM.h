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

#define GenGetSetter(type, name, member)    \
        type name() { return member; }      \
        void name(type value) { member = value; }

        // General
        GenGetSetter(bool, EnableParallelLoading, enable_parallel_loading);
        GenGetSetter(bool, EnableJIT, enable_jit);
        GenGetSetter(bool, EnableEngineControlOnError, enable_engine_control_on_error);

        // Game
        GenGetSetter(uint32_t, SaveDataCount, save_data_count);
        GenGetSetter(bool, EnableAutoSave, enable_auto_save);

        // Display
        GenGetSetter(bool, AutoDetectGameUIScale, auto_detect_game_ui_scale);
        GenGetSetter(double, GameUIScale, game_ui_scale);
        GenGetSetter(hstring, GameDefaultFontName, game_default_font_name);
        GenGetSetter(double, GameFontSize, game_font_size);
        GenGetSetter(double, GameLineHeight, game_line_height);
        GenGetSetter(Windows::UI::Color, GameBackgroundColor, game_background_color);
        GenGetSetter(Windows::UI::Color, GameForegroundColor, game_foreground_color);
        GenGetSetter(Windows::UI::Color, GameHighlightColor, game_highlight_color);
        GenGetSetter(uint32_t, GamePrintCCountPerLine, game_printc_count_per_line);
        GenGetSetter(uint32_t, GamePrintCCharCount, game_printc_char_count);
        GenGetSetter(bool, EnableFontSmoothing, enable_font_smoothing);
        GenGetSetter(bool, EnablePixelSnapping, enable_pixel_snapping);
        GenGetSetter(bool, EnableHtmlRendering, enable_html_rendering);

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(AppSettingsVM,
            // General
            enable_parallel_loading, enable_jit, enable_engine_control_on_error,
            // Game
            save_data_count, enable_auto_save,
            // Display
            auto_detect_game_ui_scale, game_ui_scale, game_default_font_name,
            game_font_size, game_line_height, game_background_color,
            game_foreground_color, game_highlight_color, game_printc_count_per_line,
            game_printc_char_count, enable_font_smoothing, enable_pixel_snapping,
            enable_html_rendering
        );

        // Non-midl methods
        std::string to_json_string() {
            nlohmann::json j;
            to_json(j, *this);
            return j.dump(4);
        }

    private:
        // General
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
        Windows::UI::Color game_background_color = Windows::UI::Colors::Black();
        Windows::UI::Color game_foreground_color = Windows::UI::Colors::Silver();
        Windows::UI::Color game_highlight_color = Windows::UI::Colors::Yellow();
        uint32_t game_printc_count_per_line = 5;
        uint32_t game_printc_char_count = 25;
        bool enable_font_smoothing = false;
        bool enable_pixel_snapping = true;
        bool enable_html_rendering = true;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct AppSettingsVM : AppSettingsVMT<AppSettingsVM, implementation::AppSettingsVM> {};
}
