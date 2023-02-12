#!/bin/bash

set -e

echo "Compiling..."

mkdir build 2>/dev/null || true
elm make --debug src/Main.elm --output build/main.js

