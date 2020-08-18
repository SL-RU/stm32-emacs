;;; stm32.el --- Support for the STM32 mircocontrollers programming
;; 
;; Filename: stm32.el
;; Description: GDB, CubeMX and flash functionality based on cmake-ide
;; Author: Alexander Lutsai <s.lyra@ya.ru>
;; Maintainer: Alexander Lutsai <s.lyra@ya.ru>
;; Created: 05 Sep 2016
;; Version: 0.01
;; Package-Requires: ()
;; Last-Updated: 11 Sep 2016
;;           By: Alexander Lutsai
;;     Update #: 0
;; URL: https://github.com/SL-RU/stm32-emacs
;; Doc URL: https://github.com/SL-RU/stm32-emacs
;; Keywords: stm32 emacs
;; Compatibility: emacs irony-mode
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; Required:
;; 1) irony-mode https://github.com/Sarcasm/irony-mode
;; 2) python
;; 3) cmake
;; 4) st-link https://github.com/texane/stlink
;; 5) clang
;; //4) https://github.com/SL-RU/STM32CubeMX_cmake
;;
;; 1) (require 'irony-mode)
;; 2) Create STM32CubeMx project and generate it for Makefile toolchain
;; 3) M-x stm32-new-project RET *select CubeMX project path*
;; 4) open main.c
;; 5) M-x stm32-cmake-build to compile
;; 6) connect stlink to your PC
;; 7) stm32-run-st-util to start gdb server
;; 8) start GDB debugger with stm32-start-gdb
;; 9) in gdb) "load" to upload file to MC and "cont" to run.For more see https://github.com/texane/stlink
;; 5) good luck!
;;
;;
;; After CubeMx project regeneration or adding new libraries or new sources you need to do stm32-cmake-build
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup stm32 nil
  "STM32 projects integration"
  :group 'development)

(defcustom stm32-st-util-command "st-util"
  "The command to use to run st-util."
  :group 'stm32
  :type 'string)

(defcustom stm32-openocd-command "openocd"
  "The command to run openocd."
  :group 'stm32
  :type 'string)

(defcustom stm32-openocd-gdb-start
  "arm-none-eabi-gdb -iex \"target extended-remote localhost:3333\" -i=mi "
  "Command to run gdb for gud with openocd remote port."
  :group 'stm32
  :type 'string)

(defcustom stm32-openocd-config-name
  "openocd.cfg"
  "Openocd requieres a .cfg file to properly function you need to provide the file, it must be called *stm32-openocd-config-name* and must be placed in the project's root directory."
  :group 'stm32
  :type 'string)

(defcustom stm32-template-files `("CMakeLists.txt"
				  "CMakeSetCompiler.cmake"
				  "CMakeIgnore.txt")
  "Name of script for generating makefiles."
  :group 'stm32
  :type 'string)

(defcustom stm32-template (concat user-emacs-directory
				  "stm32/STM32CubeMX_cmake/")
  "Directory with scripts for generating makefiles."
  :group 'stm32
  :type 'string)

(defcustom stm32-gdb-start
  "arm-none-eabi-gdb -iex \"target extended-remote localhost:4242\" -i=mi "
  "Command to run gdb for gud."
  :group 'stm32
  :type 'string)

(defcustom stm32-cubemx
  "~/STM32CubeMX/STM32CubeMX"
  "Path to stm32CubeMx binary."
  :group 'stm32
  :type 'string)

(defcustom stm32-vfpcc-fix-fix
  "//fix of vfpcc register in old versions of cmsis
#define __get_FPSCR __builtin_arm_get_fpscr
#define __set_FPSCR __builtin_arm_set_fpscr"
  "Fix of vfpcc register in old versions of cmsis.  In cmsis_gcc.h."
  :group 'stm32
  :type 'string)

(defcustom stm32-vfpcc-fix-source
  (concat "/**\n"
          "  \\brief   Get FPSCR\n"
          "  \\details Returns the current value of the Floating Point Status/Control register.\n"
          "  \\return               Floating Point Status/Control register value\n"
          " */\n"
          "__attribute__((always_inline)) __STATIC_INLINE uint32_t __get_FPSCR(void)\n"
          "{\n"
          "#if ((defined (__FPU_PRESENT) && (__FPU_PRESENT == 1U)) && \\\n"
          "     (defined (__FPU_USED   ) && (__FPU_USED    == 1U))     )\n"
          "  uint32_t result;\n"
          "\n"
          "  __ASM volatile (\"VMRS %0, fpscr\" : \"=r\" (result) );\n"
          "  return(result);\n"
          "#else\n"
          "   return(0);\n"
          "#endif\n"
          "}\n"
          "\n"
          "\n"
          "/**\n"
          "  \\brief   Set FPSCR\n"
          "  \\details Assigns the given value to the Floating Point Status/Control register.\n"
          "  \\param [in]    fpscr  Floating Point Status/Control value to set\n"
          " */\n"
          "__attribute__( ( always_inline ) ) __STATIC_INLINE void __set_FPSCR(uint32_t fpscr)\n"
          "{\n"
          "#if ((defined (__FPU_PRESENT) && (__FPU_PRESENT == 1U)) && \\\n"
          "     (defined (__FPU_USED   ) && (__FPU_USED    == 1U))     )\n"
          "  __ASM volatile (\"VMSR fpscr, %0\" : : \"r\" (fpscr) : \"vfpcc\", \"memory\");\n"
          "#else\n"
          "  (void)fpscr;\n"
          "#endif\n"
          "}")
  "Fix of vfpcc register in old versions of cmsis.  In cmsis_gcc.h."
  :group 'stm32
  :type 'string)

(defcustom stm32-vfpcc-fix-path
  "Drivers/CMSIS/Include/cmsis_gcc.h"
  "Fix of vfpcc register in old versions of cmsis.  In cmsis_gcc.h."
  :group 'stm32
  :type 'string)


(defcustom stm32-build-dir
  "build"
  "Directory for cmake build."
  :group 'stm32
  :type 'string)

(require 'cl-lib)
(require 'gdb-mi)
(require 'gud)
(require 'irony)

(defun stm32-get-project-root-dir ()
  "Return root path of current project."
  (if irony--working-directory
      (let
	  ((dir (substring irony--working-directory 0 (- (length stm32-build-dir)))))
	(if (file-exists-p dir)
	    (progn (message (concat "Project dir: "
				    dir))
		   dir) ;return dir
	  (progn
	    (message "No root. Build directory must be /build/")
	    (message dir)
            nil)))))


(defun stm32-get-project-build-dir ()
  "Return path to build dir of current project."
  (if (stm32-get-project-root-dir)
      (let ((dir (concat
		  (stm32-get-project-root-dir)
		  stm32-build-dir)))
	(if (file-exists-p dir)
	    (progn (message (concat "Project build dir: "
				    dir))
		   dir) ;return dir
          (message "No build dir")
          nil))))

(defun stm32-get-project-name ()
  "Return path of current project."
  (if (stm32-get-project-root-dir)
      (let* ((pth (substring (stm32-get-project-root-dir) 0 -1))
	     (name (car (last (split-string pth "/")))))
	(message (concat "Project name: " name))
	name)
    (message "Wrong root directory")))

(defun stm32-cmake-build (&optional path)
  "Execute cmake and create build directory if not exists.  Use existing project path's or use optional arg PATH."
  (interactive)
  (let ((dir (or path (stm32-get-project-build-dir))))
    (if (not dir)
        (message "No 'build' directory in your project's root. Run stm32-new-project")
      (when (not (file-directory-p dir))
	(make-directory dir))
      (when (file-directory-p dir)
	(message "cmake project...")
	(message "and make...")
	(compile
	 (concat "cd " dir "; cmake ..; make;"))
	(message "ok")))))

(defun stm32-make-build (&optional path)
  "Execute make.  Use existing project path's or use optional arg PATH."
  (interactive)
  (let ((dir (or path (stm32-get-project-build-dir))))
    (if (not dir)
        (message "No 'build' directory in your project's root. Run stm32-new-project")
      (when (not (file-directory-p dir))
	(make-directory dir))
      (when (file-directory-p dir)
	(message "cmake project...")
	(message "and make...")
	(compile
	 (concat "cd " dir "; make;"))
	(message "ok")))))

(defun stm32-new-project ()
  "Create new stm32  project from existing code."
  (interactive)
  (let* ((fil (read-directory-name "Select STM32CubeMx directory:"))
	 (nam (car (last (split-string-and-unquote fil "/") 2))))
    (when (file-exists-p fil)
      (when (y-or-n-p (concat "Create project " nam
			      " in " fil "? "))
	(progn
	  (message (concat "copying " stm32-template))
	  (dolist (x stm32-template-files)
            (let ((path_teml (concat stm32-template x))
                  (path_prj (concat fil x)))
              (when (or (not (file-exists-p path_prj))
                        (y-or-n-p (concat "File " x
			                  " already exists. Overwrite "
                                          path_prj " with template? ")))
	        (progn (copy-file path_teml path_prj t)
                       (message (concat "copied " (concat fil x)))))))
	  (message (concat "First build " fil stm32-build-dir))
	  (stm32-cmake-build (concat fil stm32-build-dir)) ;build
	  (message "done"))))))

(defun stm32-run-st-util ()
  "Run st-util gdb server."
  (interactive)
  (let ((p (get-buffer-process "*st-util*")))
    (when p
      (if (y-or-n-p "Kill currently running st-util? ")
	  (interrupt-process p)
	(user-error "St-util already running!"))))
  (sleep-for 1) ;wait for st-util being killed
  (with-temp-buffer "*st-util*"
		    
		    (async-shell-command stm32-st-util-command
					 "*st-util*"
					 "*Messages*")
		    ;;(pop-to-buffer "*st-util*")
		    ))

(defun stm32-start-gdb ()
  "Strart gud arm-none-eabi-gdb and connect to st-util."
  (interactive)
  (let ((dir (stm32-get-project-build-dir))
	(name (stm32-get-project-name))
	(p (get-buffer-process "*st-util*")))
    (when (not p)
      (stm32-run-st-util))
    (when dir
      (let ((pth (concat dir "/" name ".elf")))
	(when (file-exists-p pth)
	  (progn
	    (message pth)
	    (gdb (concat stm32-gdb-start pth))))))))

(defun stm32-start-openocd-gdb ()
  "Strart gud arm-none-eabi-gdb and connect to openocd."
  (interactive)
  (let ((dir (stm32-get-project-build-dir))
	(name (stm32-get-project-name))
	(p (get-buffer-process "*openocd*")))
    (when (not p)
      (stm32-run-openocd))
    (when dir
      (let ((pth (concat dir "/" name ".elf")))
	(when (file-exists-p pth)
	  (progn
	    (message pth)
	    (gdb (concat stm32-openocd-gdb-start pth))))))))

(defun stm32-run-openocd ()
  "Start openocd server."
  (interactive)
  (let ((p (get-buffer-process "*openocd*")))
    (when p
      (if (y-or-n-p "Kill currently running openOCD? ")
	  (interrupt-process p)
	(user-error "Openocd process already running"))))

  (sleep-for 1) ;;wait for openocd to being killed

  (let* ((r (stm32-get-project-root-dir))
	 (c stm32-openocd-command)
	 (f (concat r stm32-openocd-config-name)))
    (if (file-exists-p f)
	(with-temp-buffer "*openocd*"
			  (async-shell-command (concat c " -f " f)
					       "*openocd*"
					       "*Messages*")
			  (if (process-live-p (get-buffer-process "*openocd*"))
			      (message "openocd started")
			    (message "openocd failed to start"))
			  )
      (message (concat "OpenOCD config file "
                       stm32-openocd-config-name " not found in:" r)))))

(defun stm32-open-cubemx ()
  "Open current project in cubeMX or just start application."
  (interactive)
  (let* ((p (ignore-errors(stm32-get-project-root-dir)))
	 (c stm32-cubemx))
    (if p
	(let* ((n (stm32-get-project-name))
	       (f (concat p n ".ioc")))
	  (if (file-exists-p f)
	      (async-shell-command (concat c " " f))
	    (async-shell-command c)))
      (async-shell-command c))))

(defun stm32-flash-to-mcu ()
  "Upload compiled binary to stm32 through gdb if gdb has been started."
  (interactive)
  (if (or (and (get-buffer "*openocd*")
               (get-buffer "*gud-target extended-remote localhost:3333*")
               (get-buffer-process "*openocd*")
               (get-buffer-process "*gud-target extended-remote localhost:3333*"))
          (and (get-buffer "*st-util*")
               (get-buffer "*gud-target extended-remote localhost:4242*")
               (get-buffer-process "*st-util*")
               (get-buffer-process "*gud-target extended-remote localhost:4242*")))
      (progn (gdb-io-interrupt)
             (gud-basic-call "load")
             (gud-basic-call "cont"))
    (message "No gdb has been started")))

(defun stm32-fix-vfpcc ()
  "Insert fix of vfpcc register in old versions of cmsis.  In cmsis_gcc.h.  Remove __set_FPSCR and __get_FPSCR functions."
  (interactive)
  (if (stm32-get-project-root-dir)
      (let ((path (concat
		   (stm32-get-project-root-dir)
		   stm32-vfpcc-fix-path))
            (fix-count 0))
	(if (file-exists-p path)
	    (progn
              (message "Fixing old cmsis version")
              (message (concat "cmsis gcc path: "
			       path))
              (with-temp-buffer
                (insert-file-contents path)
                (while (search-forward
                        stm32-vfpcc-fix-source nil t)
                  (progn
                    (setq fix-count (+ fix-count 1))))
                (if (not (eq fix-count 0))
                    (progn (write-file (concat path ".bak"))
                           (message "Backup saved."))))
              (with-temp-buffer
                (insert-file-contents path)
                (setq fix-count 0)
                (while (search-forward
                        stm32-vfpcc-fix-source nil t)
                  (progn
                    (replace-match stm32-vfpcc-fix-fix)
                    (setq fix-count (+ fix-count 1))
                    (message "Found!")))
                (if (not (eq fix-count 0))
                    (progn
                      (write-file path)
                      (message
                       "cmsis_gcc.h successfully fixed"))
                  (message "cmsis_gcc.h already fixed."))))
          (message "No cmsis_gcc.h")))))

(defun stm32-kill-gdb ()
  "Kill all st-util, gdb or openocd processes and buffers."
  (interactive)
  (when (get-buffer-process "*gud-target extended-remote localhost:3333*")
    (kill-process (get-buffer-process "*gud-target extended-remote localhost:3333*")))
  (when (get-buffer-process "*openocd*")
    (kill-process (get-buffer-process "*openocd*")))
  (when (get-buffer-process "*st-util*")
    (kill-process (get-buffer-process "*st-util*")))
  (when (get-buffer-process "*gud-target extended-remote localhost:4242*")
    (kill-process (get-buffer-process "*gud-target extended-remote localhost:4242*")))
  
  (sleep-for 1)
  
  (when (get-buffer "*openocd*")
    (kill-buffer "*openocd*"))
  (when (get-buffer "*gud-target extended-remote localhost:3333*")
    (kill-buffer "*gud-target extended-remote localhost:3333*"))
  (when (get-buffer "*st-util*")
    (kill-buffer "*st-util*"))
  (when (get-buffer "*gud-target extended-remote localhost:4242*")
    (kill-buffer "*gud-target extended-remote localhost:4242*"))
  )

(provide 'stm32)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stm32.el ends here
