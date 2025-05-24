# MEraEmu Architectures

## MeraEmuCore

The core engine which encapsulates EraBasic code loading & execution logic as the backend, and allows users to integrate with platform-dependent UI frontends, including but not limited to terminal UI and native GUI.

> [!NOTE]
>
> The engine currently supports Linux, Windows and Android, and utilizes platform abilities (such as JIT) to maximize performance. Targeting the web platform (WebAssembly) is possible but **strongly discouraged**; huge performance drop is expected due to its immaturity.

`mee_bridge` exposes relevant FFI interfaces for engine control.

## MEraEmuWin

MEraEmuWin is built upon XAML Islands v1, a modern UI framework supported by Windows 10 1903 and above. MEraEmuWin renders content with Direct2D + DirectWrite, currently to a infinite-sized virtual canvas provided by DWM, and so may be constrained by its limitations.

Engine runs on a dedicated background thread; UI thread communicates with the engine thread in the hope of maximizing logic and rendering parallelism where possible.

## MEraEmuDroid

TBD