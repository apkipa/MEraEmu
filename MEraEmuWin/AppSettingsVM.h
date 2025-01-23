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

        // Game
        GenGetSetter(uint32_t, SaveDataCount, save_data_count);
        GenGetSetter(bool, EnableAutoSave, enable_auto_save);

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(AppSettingsVM,
            // General
            enable_parallel_loading, enable_jit,
            // Game
            save_data_count, enable_auto_save
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

        // Game
        uint32_t save_data_count = 20;
        bool enable_auto_save = true;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct AppSettingsVM : AppSettingsVMT<AppSettingsVM, implementation::AppSettingsVM> {};
}
