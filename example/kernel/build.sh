#!/bin/bash
#neo: dont ask why i did this file, idk either
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"
LUMORAC="$SCRIPT_DIR/../../build/lumorac"
mkdir -p "$BUILD_DIR"
echo "[1/1] lumorac"
cd "$SCRIPT_DIR"
"$LUMORAC" --conf lumora.conf
echo ""
echo "Build complete: $BUILD_DIR/kernel.elf"
echo "Bootable ISO:  $BUILD_DIR/kernel.iso"
file "$BUILD_DIR/kernel.elf"
