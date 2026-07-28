```
#########                               
#########                               
#########                               
#########                               
#########                               
#########                               
#########                               
#########                               
#######                                 
####  .#####         #                  
## ############*    ##                  
# ####################                  
#####################                   
#*      ############                    
#          #######                      
```

# Lumora

Lumora is a modern, statically-typed programming language compiler built with C++ and LLVM. It features a powerful extension system allowing for custom syntax extensions and macros via dynamically loaded shared libraries.

## Examples

|               OpenGL Triangle                   |                     Raylib                         |
| :-----------------------------------------------------------: | :--------------------------------------------------------------------------------------------: |
| <img width="935" height="688" alt="image" src="https://github.com/user-attachments/assets/e0f55544-4b15-4df1-a9de-4808606be248" /> | <img width="832" height="656" alt="image" src="https://github.com/user-attachments/assets/c405ad30-90be-423a-b8a1-9f9851efb2bb" /> |

|                  Kernel                     |                 GTK3                    |
| :------------------------------------------------------------------------: | :------------------------------------------------------------------: |
| <img width="752" height="483" alt="image" src="https://github.com/user-attachments/assets/683aa3d7-b51a-43d6-81c6-f05a3d8e37fe" /> | <img width="832" height="656" alt="image" src="https://github.com/user-attachments/assets/0b32988c-e44f-4970-8a6e-7427f9844ed9" /> |


## Features

- **LLVM Backend:** Generates high-performance native code using LLVM IR.
- **Extension System:** Supports compiler plugins (macros) loaded at runtime (e.g., `@log`, `@assert`).
- **Configuration Driven:** Build settings managed via `lumora.conf`.
- **Modern Syntax:** clean, expression-oriented syntax.

## Building from Source

### Prerequisites
- CMake 3.20+
- C++23 compatible compiler (GCC/Clang)
- LLVM/Clang development libraries

### Build Instructions

```bash
cmake -S . -B build
make -j$(nproc) -C build
```

This will produce the `lumorac` compiler executable at `build/`.

## Usage

### Basic Compilation

To compile a Lumora source file:

```bash
./build/lumorac example/main.lm
```

This will generate an executable in the `build/` directory (e.g., `build/myapp`).

### Command Line Options

```
Usage: ./lumorac [options] [files...]

Options:
  --conf <file>     Use specified lumora.conf (default: lumora.conf)
  --dump-tokens     Print tokens and exit
  --dump-ast        Print AST and exit
  --dump-ir         Print generated LLVM IR
  --stop-ir         Stop after IR emission (no opt/link)
  --no-opt          Skip optimization steps
  --verbose         Verbose build output
  --ext-dir <dir>   Load extensions from directory
  --output <dir>    Override output directory
  -h, --help        Print help message
```

## Extensions

Lumora supports extensions that can hook into the parser, semantic analyzer, and code generator. 

The example extension provided in `extensions/example` implements:
- `@log(args...)`: Prints formatted output to stdout.
- `@assert(condition, message)`: Asserts a condition, terminating the program if false.

To use extensions, ensure they are built (enabled by default with `LUMORA_BUILD_EXTENSIONS`) and referenced in your `lumora.conf` or via `--ext-dir`.

## Configuration

The build process is controlled by `lumora.conf`.

Example `lumora.conf`:
```toml
name = "myapp"
version = "0.1.0"
output_dir = "build"

[extensions]
dir = "build/extensions/example"

[source.main]
files = ["example/main.lm"]

[opt.release]
level = "O2"
passes = ["mem2reg", "instcombine", "simplifycfg", "gvn"]

[link.binary]
linker = "clang"
inputs = ["build/main.opt.ll"]
output = "build/myapp"
libs   = ["c", "m"]
flags  = ["-no-pie"]
```

# Licensing
Lumora is fully free and open-source, released under the [Apache 2.0 License](LICENSE).
