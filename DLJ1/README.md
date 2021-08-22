# Overview

DLJ1 stands for "Deamer LangJam1".

I want to use [Deamer](https://github.com/Deruago/theDeamerProject) to create a language for this jam.

# Directories

This folder contains varies subdirectories:
- definition: contains the [DLDL](https://github.com/Deruago/DLDL) definitions.
- DLJ1: Contains source code

# Build instructions

If there is a ```CMakeLists.txt``` present in this directory, you can use the following commands to build the language:

```bash
mkdir build
cd build

cmake ..
cmake --build . --target DLJ1
```
