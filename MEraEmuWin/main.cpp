#include "pch.h"

#include <winrt/Tenkai.h>
#include "App.h"

using namespace winrt;

int APIENTRY wWinMain(HINSTANCE hInst, HINSTANCE, PWSTR pCmdLine, int nCmdShow) {
    init_apartment(apartment_type::single_threaded);
    Tenkai::AppService::InitializeForApplication([](auto&&) {
        make<MEraEmuWin::implementation::App>();
    });
    Tenkai::AppService::RunLoop();
    return 0;
}
