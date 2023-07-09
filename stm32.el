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
;; Compatibility: emacs projectile helm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Required: helm, friendly-shell-command, projectile, s
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
  "arm-none-eabi-gdb -iex \"target extended-remote localhost:3333\" -i=mi "
  "Command to run gdb for gud with openocd remote port."
  :group 'stm32
  :type 'string)

(defcustom stm32-cubemx
  "~/STM32CubeMX/STM32CubeMX"
  "Path to stm32CubeMx binary."
  :group 'stm32
  :type 'string)

(require 'gdb-mi)
(require 'gud)
(require 'friendly-shell-command)
(require 'projectile)
(require 'helm)
(require 's)



(defun stm32--select-file (root filter)
  "Select file using helm in ROOT directory and find by FILTER."
  (let* ((out (s-split
               "\n"
               (friendly-shell-command-to-string
                (s-concat "find . -type f -name '" filter "'")
                :path root)))
         (files (delete "" out)))
    (if (> (length files) 0)
        (helm :sources (helm-build-sync-source (concat "Select " filter)
                         :candidates files
                         :fuzzy-match t)
              :buffer "*helm find stm32*"
              :case-fold-search helm-file-name-case-fold-search)
      (user-error "No matching files"))))

(defun stm32--close-process-buffer (process signal)
  "Close process buffer when PROCESS sends exit SIGNAL."
  (when (memq (process-status process) '(exit signal))
    (kill-buffer (process-buffer process))))

(defun stm32--start-process-buffer (buffer-name cmd)
  "Run CMD in the buffer named BUFFER-NAME."
  (let* ((output-buffer (generate-new-buffer buffer-name))
         (proc (progn
                 (message (concat "Running " cmd))
                 (async-shell-command cmd
                                      output-buffer
                                      "*Messages*")
                 (get-buffer-process output-buffer))))
    (when (process-live-p proc)
      (set-process-sentinel proc #'stm32--close-process-buffer))))

(defun stm32--kill-gdb-if-started ()
  "Kill gdb and openocd if running."
  (gud-set-buffer)
  (let ((proc-ocd (get-buffer-process "*stm32-gdb-server*"))
        (proc-gdb (get-buffer-process gud-comint-buffer)))
    (when (or proc-ocd proc-gdb)
      (if (y-or-n-p "Kill currently running gdb? ")
	  (stm32-kill-gdb)
	(user-error "GDB already running!")))))



(defun stm32-start-gdb-server ()
  "Run gdb server with selected config.  Config files are filtered using '*.cfg'."
  (interactive)
  (stm32--kill-gdb-if-started)
  (let ((root (projectile-project-root))
        (buffer-name "*stm32-gdb-server*"))
    (when 'root
      (let* ((file (stm32--select-file root "*.cfg"))
             (cmd (concat stm32-openocd-command " -f " (concat root file)))
             (output-buffer (generate-new-buffer buffer-name)))
        (when file
          (async-shell-command cmd
                               output-buffer
                               "*Messages*")
          (let ((proc (get-buffer-process output-buffer)))
            (when (process-live-p proc)
              proc)))))))

(defun stm32-start-gdb-elf ()
  "Run arm-none-eabi-gdb and select elf."
  (interactive)
  (stm32--kill-gdb-if-started)
  (let* ((root (projectile-project-root))
         (p (get-buffer-process "*stm32-gdb-server*"))
         (server-started (when (not p)
                           (stm32-start-gdb-server))))
    (when (and 'root 'server-started)
      (let ((file (stm32--select-file root "*.elf")))
        (when file
          (save-window-excursion
            (gdb (s-concat stm32-gdb-command " " (concat root file)))))))))

(defun stm32-flash-to-mcu ()
  "Upload compiled binary to stm32 through gdb if gdb has been started."
  (interactive)
  (gud-set-buffer)
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (or proc (error "Current buffer has no process"))
    (with-current-buffer gud-comint-buffer
      (save-excursion
        (interrupt-process proc)
        (gud-basic-call "load")
        (gud-basic-call "cont")))))

(defun stm32-kill-gdb ()
  "Kill all gdb or openocd processes and buffers."
  (interactive)
  (gud-set-buffer)
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (when proc
      (kill-process proc)))
  (when (get-buffer-process "*stm32-gdb-server*")
    (kill-process (get-buffer-process "*stm32-gdb-server*")))
  (sleep-for 1)
  (kill-buffer gud-comint-buffer)
  (when (get-buffer "*stm32-gdb-server*")
    (kill-buffer "*stm32-gdb-server*")))



(defun stm32--start-cubemx (&optional file)
  "Run CubeMX in the buffer and open FILE."
  (let ((name (concat " *CubeMX " file "*"))
        (cmd (if 'file
                 (concat stm32-cubemx " " file)
               stm32-cubemx)))
    (stm32--start-process-buffer name cmd)))

(defun stm32-open-cubemx ()
  "Open current project in cubeMX or just start application."
  (interactive)
  (save-window-excursion
    (let ((root (projectile-project-root)))
      (if root
          (let ((file (stm32--select-file root "*.ioc")))
            (if file
                (stm32--start-cubemx (concat root file))
              (stm32--start-cubemx)))
        (stm32--start-cubemx)))))

(provide 'stm32)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stm32.el ends here
