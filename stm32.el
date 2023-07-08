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
;; Compatibility: emacs projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Required:
;; 2) python
;; 3) cmake
;; 4) st-link https://github.com/texane/stlink
;; 5) clang
;; //4) https://github.com/SL-RU/STM32CubeMX_cmake
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
  "STM32 projects integration."
  :group 'development)

(defcustom stm32-openocd-command "openocd"
  "The command to run openocd."
  :group 'stm32
  :type 'string)

(defcustom stm32-gdb-command
  "arm-none-eabi-gdb -iex \"target extended-remote localhost:3333\" "
  "Command to run gdb for gud with openocd remote port."
  :group 'stm32
  :type 'string)

(defcustom stm32-cubemx
  "~/STM32CubeMX/STM32CubeMX"
  "Path to stm32CubeMx binary."
  :group 'stm32
  :type 'string)

(require 'cl-lib)
(require 'gdb-mi)
(require 'gud)
(require 'friendly-shell-command)
(require 'projectile)
(require 'helm)
(require 's)

(defun stm32-select-file (root filter)
  "Select file using helm in ROOT directory and find by FILTER."
  (let* ((out (s-split
               "\n"
               (friendly-shell-command-to-string
                (s-concat "find . -type f -name '" filter "'")
                :path root)))
         (files (delete "" out)))
    (when (> (length files) 0)
      (helm :sources (helm-build-sync-source "test"
                       :candidates files
                       :fuzzy-match t)
            :buffer "*helm find stm32*"
            :case-fold-search helm-file-name-case-fold-search))))

(defun stm32-start-gdb-server ()
  "Run gdb server."
  (interactive)
  (let ((p (get-buffer-process "*stm32-gdb-server*")))
    (when p
      (if (y-or-n-p "Kill currently running stm32-gdb-server? ")
	  (interrupt-process p)
	(user-error "Stm32-gdb-server already running!"))))
  (sleep-for 1) ;wait for process being killed
  (let ((root (projectile-project-root)))
    (when 'root
      (let ((file (stm32-select-file root "*.cfg")))
        (when file
          (with-temp-buffer
            "*stm32-gdb-server*"
	    (async-shell-command (s-concat stm32-openocd-command " -f " (concat root file))
				 "*stm32-gdb-server*"
				 "*Messages*")))))))

(defun stm32-start-gdb-elf ()
  "Run arm-none-eabi-gdb and select elf."
  (interactive)
  (let ((root (projectile-project-root))
        (p (get-buffer-process "*stm32-gdb-server*")))
    (when (not p)
      (stm32-start-gdb-server))
    (when 'root
      (let ((file (stm32-select-file root "*.elf")))
        (when file
          (gud-gdb (s-concat stm32-gdb-command " " (concat root file))))))))

(defun stm32-flash-to-mcu ()
  "Upload compiled binary to stm32 through gdb if gdb has been started."
  (interactive)
  (if (and (get-buffer "*stm32-gdb-server*")
               (get-buffer "*gud-target extended-remote localhost:3333*"))
      (progn (gdb-io-interrupt)
             (gud-basic-call "load")
             (gud-basic-call "cont"))
    (message "No gdb has been started")))

(defun stm32-kill-gdb ()
  "Kill all gdb or openocd processes and buffers."
  (interactive)
  (when (get-buffer-process "*gud-target extended-remote localhost:3333*")
    (kill-process (get-buffer-process "*gud-target extended-remote localhost:3333*")))
  (when (get-buffer-process "*stm32-gdb-server*")
    (kill-process (get-buffer-process "*stm32-gdb-server*")))
  (sleep-for 1)
  (when (get-buffer "*stm32-gdb-server*")
    (kill-buffer "*stm32-gdb-server*"))
  (when (get-buffer "*gud-target extended-remote localhost:3333*")
    (kill-buffer "*gud-target extended-remote localhost:3333*")))


(defun stm32--close-process-buffer (process signal)
  "Close process buffer when PROCESS sends exit SIGNAL."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer (process-buffer process))
    (message process "stopped")))

(defun stm32--start-cubemx (&optional file)
  "Run CubeMX in the buffer and open FILE."
  (let* ((name (concat " *CubeMX " file "*"))
         (output-buffer (generate-new-buffer name))
         (cmd (if 'file
                  (concat stm32-cubemx " " file)
                stm32-cubemx))
         (proc (progn
                 (message (concat "Running " cmd))
                 (async-shell-command cmd output-buffer)
                 (get-buffer-process output-buffer))))
    (when (process-live-p proc)
        (set-process-sentinel proc #'stm32--close-process-buffer))))


(defun stm32-open-cubemx ()
  "Open current project in cubeMX or just start application."
  (interactive)
  (save-window-excursion
    (let ((root (projectile-project-root)))
      (if root
          (let ((file (stm32-select-file root "*.ioc")))
            (if file
                (stm32--start-cubemx (concat root file))
              (stm32--start-cubemx)))
        (stm32--start-cubemx)))))

(provide 'stm32)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stm32.el ends here
