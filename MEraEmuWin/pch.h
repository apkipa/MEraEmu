#pragma once

#define _CRT_SECURE_NO_WARNINGS
#define NOMINMAX
#include <initguid.h>
#include <windows.h>
#include <unknwn.h>
#include <restrictederrorinfo.h>
#include <hstring.h>
#include <uxtheme.h>
#include <d3d11_2.h>
#include <d2d1_3.h>
#include <d2d1_3helper.h>
#include <dwrite_3.h>
#include <wincodec.h>
#include <Shlobj.h>
#ifdef GetCurrentTime
#undef GetCurrentTime
#endif

#include <winrt/base.h>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.Foundation.Collections.h>
#include <winrt/Windows.System.h>
#include <winrt/Windows.System.Threading.h>
#include <winrt/Windows.UI.Core.h>
#include <winrt/Windows.UI.Input.h>
#include <winrt/Windows.UI.Text.h>
#include <winrt/Windows.UI.Xaml.h>
#include <winrt/Windows.UI.Xaml.Shapes.h>
#include <winrt/Windows.UI.Xaml.Controls.h>
#include <winrt/Windows.UI.Xaml.Controls.Primitives.h>
#include <winrt/Windows.UI.Xaml.Data.h>
#include <winrt/Windows.UI.Xaml.Media.h>
#include <winrt/Windows.UI.Xaml.Media.Imaging.h>
#include <winrt/Windows.UI.Xaml.Interop.h>
#include <winrt/Windows.UI.Xaml.Markup.h>
#include <winrt/Windows.UI.Xaml.Navigation.h>
#include <winrt/Windows.UI.Xaml.Documents.h>
#include <winrt/Windows.UI.Xaml.Hosting.h>
#include <winrt/Windows.UI.Xaml.Input.h>
#include <winrt/Windows.UI.Xaml.Core.Direct.h>
#include <winrt/Windows.UI.ViewManagement.h>
#include <winrt/Windows.ApplicationModel.h>
#include <winrt/Windows.ApplicationModel.Core.h>
#include <winrt/Windows.ApplicationModel.Activation.h>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>
#include <winrt/Windows.Storage.h>
#include <winrt/Windows.Storage.Streams.h>
#include <winrt/Windows.Media.h>
#include <winrt/Windows.Media.Audio.h>
#include <winrt/Windows.Media.Render.h>

#include <winrt/Microsoft.UI.Xaml.Automation.Peers.h>
#include <winrt/Microsoft.UI.Xaml.Controls.h>
#include <winrt/Microsoft.UI.Xaml.Controls.Primitives.h>
#include <winrt/Microsoft.UI.Xaml.Media.h>
#include <winrt/Microsoft.UI.Xaml.XamlTypeInfo.h>

#include <winrt/WinUIEditor.h>

#include <winrt/Tenkai.UI.ViewManagement.h>
#include <winrt/Tenkai.UI.Xaml.h>
#include <winrt/Tenkai.UI.Xaml.Data.h>
#include <winrt/Tenkai.UI.Xaml.Controls.h>
#include <winrt/Tenkai.Storage.h>
#include <winrt/Tenkai.Storage.Streams.h>

//#include <winrt/Microsoft.Xaml.Interactivity.h>
//#include <winrt/Microsoft.Xaml.Interactions.Core.h>
//#include <winrt/Microsoft.Xaml.Interactions.Media.h>

//#include <include/WinUIIncludes.hpp>

#include <Tenkai.hpp>

#include <windows.ui.xaml.media.dxinterop.h>

#include <ppl.h>
#include <ppltasks.h>
#include <pplawait.h>

#include <functional>
#include <algorithm>
#include <cwctype>
#include <cctype>
#include <memory>
#include <ranges>
#include <thread>
#include <vector>
#include <array>
#include <mutex>
#include <condition_variable>
#include <variant>
#include <future>
#include <atomic>
#include <chrono>
#include <set>
#include <random>
#include <fstream>
#include <filesystem>

#include "util.hpp"
#include "Shared.hpp"
