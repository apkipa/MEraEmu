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

        bool EnableParallelLoading() { return enable_parallel_loading; }
        void EnableParallelLoading(bool value) { enable_parallel_loading = value; }
        bool EnableJIT() { return enable_jit; }
        void EnableJIT(bool value) { enable_jit = value; }

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(AppSettingsVM, enable_parallel_loading, enable_jit);

    private:
        bool enable_parallel_loading = false;
        bool enable_jit = false;
    };
}

namespace winrt::MEraEmuWin::factory_implementation {
    struct AppSettingsVM : AppSettingsVMT<AppSettingsVM, implementation::AppSettingsVM> {};
}
