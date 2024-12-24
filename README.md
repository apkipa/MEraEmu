# MEraEmu

MEraEmu (Modern Era Emulator) is an EraBasic script emulator for modern Windows.

> [!NOTE]
> This project is not meant as a replacement for Emuera([EM+EE](https://evilmask.gitlab.io/emuera.em.doc/index.html)), nor will it ever be (no full instructions set support, for instance). The intention is to provide alternative experiences that are not found in Emuera.

> [!WARNING]
> This project is WIP (work-in-progress), and while some games are able to boot into gameplay, do expect absence of functions, glitches and crashes. Expect its worst-case performance to be ~4x worse than Emuera.

## Features

* Uses modern platform abilities to deliver beautiful and fluid UI experience with minimal interaction latency.
* ~~Compatible save format with Emuera.~~
* DevTools which eases script development, featuring:
  * Setting breakpoints to inspect code execution in real time.
  * ~~Edit code and see changes without restarting.~~
  * ~~Creating engine checkpoints to go back in time.~~

## Requirements

* Windows 10 1903+

## Building

This project requires:

* The latest version of Rust compiler (at the time of writing 1.80, although MSRV is not strictly defined).
* The latest version of Visual Studio 2022, with C++ & UWP workload.
* [vcpkg](https://github.com/microsoft/vcpkg) for providing C++ dependencies. Run the following command to install them:

  ```shell
  vcpkg install nlohmann-json
  ```

You may also need the NuGet package *Tenkai.UWP* (see https://github.com/apkipa/TenkaiXamlSample), which is required by the UI part when building for Windows.

You should be able to build the project simply by opening the VS solution and clicking Build button, VS will automatically invoke the Rust compiler to build the engine core and link it with the C++ UI part.