Some functions for work with stm32 arm microcontrollers in EMACS.

Video of work: https://youtu.be/M7RBQsq5_lc

### Required:

1) cmake-ide
2) python
3) cmake
4) clang
5) st-link https://github.com/texane/stlink
//5) https://github.com/SL-RU/STM32CubeMX_cmake

### Install:

1) clone repository to /.emacs.d/stm32
2) execute "git submodule update --init" to clone STM32CubeMX_cmake to /.emacs.d/stm32/STM32CubeMX_cmake
3) Change paths to yours in stm32.el
4) add to your init file (require 'stm32)
5) install cmake-ide and [configure](https://syamajala.github.io/c-ide.html)

### Commentary:

WORK IN PROGRESS!!!
1) Create STM32CubeMx project and generate it for SW4STM32
2) M-x stm32-new-project RET *select CubeMX project path*
3) open main.c
4) C-c . C to compile
5) connect stlink to your PC
6) stm32-run-st-util to start gdb server
7) start GDB debugger with stm32-start-gdb
8) in gdb) "load" to upload file to MC and "cont" to run.For more see https://github.com/texane/stlink
9) good luck!


After CubeMx project regeneration or adding new libraries or new sources you need to do M-x stm32-cmake-build


# IMPORTANT

If you have error in cmsis_gcc.h do ```M-x stm32-fix-vfpcc```. It will change some lines in cmsis_gcc.h and will create backup cmsis_gcc.h.bak.

# License:

This program is distributed under the terms of GNU General
Public License, version 3 or any later version. See COPYING 
for details.
