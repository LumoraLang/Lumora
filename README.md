# Lumora

A modern, high-performance programming language.

## About

Lumora is a statically-typed, compiled programming language designed for performance and safety. It leverages advanced compilation techniques to generate optimized machine code, providing a robust platform for building efficient applications.

## Features

*   **Statically Typed:** Ensures type safety at compile time, reducing runtime errors.
*   **High Performance:** Generates optimized machine code for fast execution.
*   **Modular Design:** Supports module imports for organized and reusable code.
*   **Configurable Builds:** Customize compilation settings, optimization levels, and external dependencies via `lumora.yaml`.
*   **Interoperability:** Seamlessly integrate with C/C++ code.

## Getting Started

### Prerequisites

To use Lumora, you will need:

*   Rust (latest stable version) - required to build the Lumora toolchain.
*   Clang (for linking)
*   LLVM (version 18.x, specifically `llvm-as` and `llc` tools)

### Building Lumora

To build the Lumora toolchain, navigate to the project root and run:

```bash
cargo build --release
```

The `lumora` executable will be located at `target/release/lumora`.

### Compiling Lumora Code

To compile a Lumora source file (`.lum`), use the `lumora` executable:

```bash
./target/release/lumora <input.lum> [output_executable_name]
```

Example:

```bash
./target/release/lumora examples/hello.lum hello_world
```

This will generate an executable named `hello_world` in the `build/` directory (default output directory).

### Configuration

Lumora projects can be configured using a `lumora.yaml` file in the project root. This file allows you to specify various build, linker, and dependency settings.

```yaml
# lumora.yaml - Example Configuration File

# Build settings control how your Lumora code is compiled.
build_settings:
  # output_dir: Specifies the directory where compiled executables and intermediate files will be placed.
  # Default: "build"
  output_dir: "build"

  # optimization_level: Sets the LLVM optimization level.
  # Common values: "O0" (no optimization), "O1", "O2", "O3" (highest optimization), "Os" (optimize for size), "Oz" (optimize for smallest size).
  # Default: "" (no specific optimization flag passed, LLVM's default applies)
  optimization_level: "O3"

  # debug_info: A boolean indicating whether to include debug information in the compiled output.
  # Set to true for easier debugging with tools like GDB.
  # Default: false
  debug_info: true

  # target_triple: Specifies the target architecture for compilation (e.g., "x86_64-unknown-linux-gnu", "arm-none-eabi").
  # Leave empty to compile for the host system's default target.
  # Default: ""
  target_triple: ""

  # output_type: Specifies the type of output to generate.
  # Possible values: "Executable", "SharedLibrary", "StaticLibrary".
  # - "Executable": Generates a standalone executable (default).
  # - "SharedLibrary": Generates a shared library (.so on Linux, .dylib on macOS, .dll on Windows).
  # - "StaticLibrary": Generates a static library (.a on Linux/macOS, .lib on Windows).
  # Default: "Executable"
  output_type: "Executable"

# Linker settings control how the compiled object files are linked into a final executable.
linker_settings:
  # libraries: A list of system libraries to link against (e.g., "m" for math, "pthread" for pthreads).
  # These are typically passed to the linker with a -l prefix.
  # Default: []
  libraries:
    - "m"
    - "pthread"

  # flags: A list of additional flags to pass directly to the linker.
  # Use this for custom linker options not covered by other settings.
  # Default: []
  flags:
    - "-static-libgcc"
    - "-pie"

# External dependencies allow you to link your Lumora project with external C/C++ source files,
# object files, or static libraries.
external_dependencies:
  # Each entry can be a path to a .c, .cpp, .o (object), or .a (archive) file.
  # .c and .cpp files will be compiled by Clang before linking.
  # Default: []
  - "src/c_utils/my_utility.c"
  - "libs/my_static_lib.a"
  - "obj/precompiled_module.o"
```

## License

This project is licensed under the [MIT License](LICENSE).
