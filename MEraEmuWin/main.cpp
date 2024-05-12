#include "pch.h"

#include <winrt/Tenkai.h>
#include "App.h"

using namespace winrt;

int APIENTRY wWinMain(_In_ HINSTANCE hInst, _In_opt_ HINSTANCE, _In_ PWSTR pCmdLine, _In_ int nCmdShow) {
    init_apartment(apartment_type::single_threaded);
    Tenkai::AppService::InitializeForApplication([&](auto&&) {
        // HACK: Keep application alive FOREVER to prevent crashes on
        //       application shutdown
        make_self<MEraEmuWin::implementation::App>().detach();
    });
    Tenkai::AppService::RunLoop();
    Tenkai::AppService::UninitializeForApplication();

    // Remove reference to threadpoolwinrt.dll to prevent crashes
    clear_factory_cache();

    return 0;
}
