Some functions for work with stm32 arm microcontrollers in EMACS.

Video of work: https://youtu.be/M7RBQsq5_lc

### Required:

1) cmake-ide
2) python
3) cmake
4) clang
5) st-link https://github.com/texane/stlink
6) (optional) openocd

//5) https://github.com/SL-RU/STM32CubeMX_cmake

### Install:

1) clone repository to /.emacs.d/stm32
2) execute "git submodule update --init" to clone STM32CubeMX_cmake to /.emacs.d/stm32/STM32CubeMX_cmake
3) Change paths to yours in stm32.el
4) add to your init file (require 'stm32)
5) install cmake-ide and [configure](https://syamajala.github.io/c-ide.html)

### How to use:

WORK IN PROGRESS!!!

#### GDB and st-link
1) Create STM32CubeMx project and generate it for SW4STM32
2) M-x stm32-new-project RET *select CubeMX project path*
3) open main.c
4) C-c . C to compile
5) connect stlink to your PC
6) stm32-run-st-util to start gdb server
7) start GDB debugger with stm32-start-gdb
8) in gdb) "load" to upload file to MC and "cont" to run.For more see https://github.com/texane/stlink
9) good luck!

#### GDB and Openocd

Openocd requieres a .cfg file to properly function you need to provide the file in this case it must be
called board.cfg, and example file is provided (board.cfg) the file needs to be located in your project root.

1) Create STM32CubeMx project and generate it for SW4STM32
2) M-x stm32-new-project RET *select CubeMX project path*
3) put the board.cfg in your project root(an example file named board.cfg is provided in this repo)
3) open main.c
4) C-c . C to compile
5) connect stlink to your PC
6) stm32-run-openocd to start openocd server
7) start GDB debugger with stm32-start-openocd-gdb
8) debug your project and good luck!


after this you shold be in the debugger window and you can debug your program, but the default gdb window acts like a terminal and is not very helpful in regards of context and data, so its a good idea to use gdb in many windows mode you can acativate it in your startup config file or with M-x gdb-many-windows

### Compilation funcitions

you can build or clean and build your projects

- stm32-cmake-build:
  this is the equivalent of cleand and build of most IDE's and it recompiles every source file of your project

- stm32-make-build:
  the equivalent of build of most IDE's with this you can compile only the modified source files of your project scince the last compilation, this is useful if you only changed a couple of lines in your project as it makes the compilation proces faster.

### Closing stm32-debugger
Once you finished your debugin session you can finish all the windows opened by this plugin with the following function.

- M-x stm32-kill-gdb

After CubeMx project regeneration or adding new libraries or new sources you need to do M-x stm32-cmake-build


# IMPORTANT

If you have error in cmsis_gcc.h do ```M-x stm32-fix-vfpcc```. It will change some lines in cmsis_gcc.h and will create backup cmsis_gcc.h.bak.

# License:

This program is distributed under the terms of GNU General
Public License, version 3 or any later version. See COPYING
for details.
