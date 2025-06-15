# MEraEmu Architectures

## MeraEmuCore

The core engine which encapsulates EraBasic code loading & execution logic as the backend, and allows users to integrate with platform-dependent UI frontends, including but not limited to terminal UI and native GUI.

> [!NOTE]
>
> The engine currently supports ~~Linux~~, Windows ~~and Android~~, and utilizes platform abilities (such as ~~JIT~~) to maximize performance. Targeting the web platform (WebAssembly) is possible but **strongly discouraged**; huge performance drop is expected due to its immaturity (such as limited threading and JIT support).

`mee_bridge` exposes relevant FFI interfaces for engine control, to native or JavaScript.

`mee_engine` contains the core engine logic. Inside the engine, code is first preprocessed to handle macros & replacements, then run through the lexer to produce a sequence of tokens (such as newlines). Parser transforms token sequences into ASTs, represented in an compact form (Struct of Arrays). Finally, code generator reads the ASTs to allocate global variables and function entries, and then generates bytecode for each function, each file being a single code chunk. Bytecode is of fixed size (4 bytes), in order to simplify bytecode manipulation. In order to load code into engine and get it running, one should first construct a `MEraEngineBuilder` to load CSVs, then ERHs, next ERBs, and finally call `build()` to finalize building and get a `MEraEngine`, which contains runtime data and VM variables for storing execution states.

`mee_engine` allows for customization of built-in commands and functions (before loading any CSVs). In fact, many commands (such as `PRINTFORM*`) are registered into engine internally in this way, making things more modular and paves the way for user extension.

`mee_cli` is an engine frontend implemented in console, which is highly portable but provides severely limited interaction support.

## ~~MEraEmuWin~~

MEraEmuWin is built upon XAML Islands v1, a modern UI framework supported by Windows 10 1903 and above. MEraEmuWin renders content with Direct2D + DirectWrite, currently to a infinite-sized virtual canvas provided by DWM, and so may be constrained by its limitations.

Engine runs on a dedicated background thread; UI thread communicates with the engine thread in the hope of maximizing logic and rendering parallelism where possible.

## ~~MEraEmuDroid~~

TBD