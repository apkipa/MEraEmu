#include "pch.h"

#include <winrt/Tenkai.h>
#include "App.h"

using namespace winrt;

#if 0
void fix_threadpoolwinrt() {
    // Remove reference to threadpoolwinrt.dll to prevent crashes
    clear_factory_cache();
}
#else
#include <FixThreadpoolwinrt.h>
void fix_threadpoolwinrt() {
    // Import early
    EnsureFixThreadpoolwinrtLoaded();
}
#endif

int APIENTRY wWinMain(_In_ HINSTANCE hInst, _In_opt_ HINSTANCE, _In_ PWSTR pCmdLine, _In_ int nCmdShow) {
    init_apartment(apartment_type::single_threaded);

    // Show splash screen eagerly
    {
        auto callback = Tenkai::ApplicationInitializationCallback([](auto const& o) {
            auto params = o.as<Tenkai::ApplicationInitializationCallbackParams>();
            params.MainWindow().ExtendsContentIntoTitleBar(true);
        });
        Tenkai::AppService::SetStartupSplashScreenParams(Tenkai::SplashScreenKind::Simple, true, callback);
    }

    Tenkai::AppService::InitializeForApplication([&](auto&&) {
        // HACK: Keep application alive FOREVER to prevent crashes on
        //       application shutdown
        make_self<MEraEmuWin::implementation::App>().detach();
    });
    Tenkai::AppService::RunLoop();
    Tenkai::AppService::UninitializeForApplication();

    fix_threadpoolwinrt();

    return 0;
}
