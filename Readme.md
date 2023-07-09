# STM32-Emacs
***
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Some functions for work with ARM microcontrollers in EMACS. All functions works in respect of projectile project and uses HELM for selection.

- `stm32-start-gdb-server` starts openocd gdb server with selected config
- `stm32-start-gdb-elf` starts openocd gdb server with selected config and `arm-none-eabi-gdb` session with selected `.elf`
- `stm32-flash-to-mcu` executes `load` and `cont` commands in GDB session
- `stm32-kill-gdb` stops openocd and GDB session
- `stm32-open-cubemx` open CubeMX with selected `.ioc` or without


## Required:
***
- openocd
- arm-none-eabi-gdb
- [helm](https://emacs-helm.github.io/helm/)
- [projectile](https://docs.projectile.mx/projectile/index.html)
- [friendly-shell-command](https://github.com/p3r7/friendly-shell)
- [s.el](https://github.com/magnars/s.el)
- (optional) CubeMX


## Install:
***
0) install and configure helm, projectile and other deps
1) clone repository to /.emacs.d/stm32
2) add to your init file (require 'stm32)
3) customize if needed


## How to use:
***
Openocd requieres a `.cfg` file to properly function you need to place `.cfg` in any directory of current project.

1) Create project and compile `.elf`
2) <kbd>M-x</kbd>`stm32-start-gdb-eld`<kbd>[RET]</kbd>, select openocd config `.cfg`, then select `.elf`
6) <kbd>M-x</kbd>`stm32-run-openocd`<kbd>[RET]</kbd> to start openocd server
8) in gdb console execute `load` to upload file to MCU and `cont` to run. Or execute <kbd>M-x</kbd>`stm32-flash-to-mcu`<kbd>[RET]</kbd>.
9) debug your project and good luck!


#### RTOS support
***
If you have a project using an RTOS (FreeRTOS for example) you can debug your project without problems but only openocd [supports](http://openocd.org/doc/html/GDB-and-OpenOCD.html) debugging tasks or threads, this is because openocd searches the tasks of your project and presents them as threads to gdb with this you can wath the stack of every task and also the current state of the task, but in order to do that you need to check if Openocd supports your particular [RTOS](http://openocd.org/doc/html/GDB-and-OpenOCD.html)(section: 21-6 RTOS Support).

If your RTOS is supported you need to do the following steps for enabling debugging tasks in Openocd:


- add the `RTOSFLAG` to the openocd.cfg file this flag varies (the file opencd.cfg contains an example for FreeRTOS users)
 ```$_TARGETNAME configure -rtos 'RTOSFLAG'```
- add the symbols definition (if requered [section:21.6](openocd.org/doc/html/GDB-and-OpenOCD.html)) to your proyect. for FreeRTOS users an example file is provided (FreeRTOS-openocd.c)
  - some RTOS require further configurations in the CMakeLists file (FreeRTOS is one of them), for more information check the example file FreeRTOS-openocd.c
- recompile your proyect and start debugging with Openocd <kbd>M-x</kbd>`stm32-start-openocd-gdb`<kbd>[RET]</kbd>.



### Optional gdb steps
***
Once you are in the debuger window you can load and test your prject, but the default gdb window acts like a terminal and is not very helpful in regards of context and data, so its a good idea to use gdb in many windows mode you can activate it in your startup config file with `(gdb-many-windows 1)` or with <kbd>M-x</kbd> gdb-many-windows <kbd>[RET]</kbd>


### Closing stm32-debugger
***
Once you finished your debugging session you can finish all the windows opened by this plugin with the following function.

<kbd>M-x</kbd> `stm32-kill-gdb` <kbd>[RET]</kbd>

This will kill the gdb process and the st-link or openocd proces depending on which do you chose to run

After CubeMx project regeneration or adding new libraries or new sources you need to do M-x stm32-cmake-build


# License:
***
This program is distributed under the terms of GNU General
Public License, version 3 or any later version. See COPYING
for details.
