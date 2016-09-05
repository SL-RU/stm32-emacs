;;e-mail: s.lyra@ya.ru
;;Version: 0.0001 05/sept/16
;;Copyright (c) 2016, Alexander Lutsai. All rights reserved.
;;
;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;
;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; Required:
;; 1) CEDET
;; 2) st-link https://github.com/texane/stlink
;;
;;
;; Commentary:
;; WORK IN PROGRESS!!!111111
;; 1) load file
;; 2) Create STM32CubeMx project and generate it for SW4STM32
;; 3) M-x stm32-new-project RET *select CubeMX project path*
;; 4) open main.c
;; 5) C-c . C to compile
;; 6) connect stlink to your PC
;; 7) stm32-run-st-util to start gdb server
;; 8) start GDB debugger with stm32-start-gdb
;; 9) in gdb) "load" to upload file to MC and "cont" to run.
;; 5) good luck!
;;
;; After CubeMx project regeneration or adding new libraries or new sources you need to do stm32-generate-makefile
;; 

;;; Code:

(defvar *stm32-st-util* "sudo st-util" "command to execute st-util")
(defvar *stm32-templates* `("CubeMX2Makefile.py"
			  "CubeMX2Makefile.tpl"
			  "CubeMX2project.tpl") "command to execute st-util")


(defun my-get-project-root-dir ()
  "Return path of current project"
  (interactive)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
	 (current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (if (null prj)
	(progn (message "buffer has no project")
	       nil)
      (progn (message (concat "Project dir: " (ede-project-root-directory prj)))
	     (ede-project-root-directory prj)))))

(defun my-get-project-name ()
  "Return path of current project"
  (interactive)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
	 (current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (if (null prj)
	(progn (message "buffer has no project")
	       nil)
      (progn (message (concat "Project name: " (ede-name prj)))
	     (ede-name prj)))))

(defun stm32-generate-makefile (&optional path)
  "Generate or regenerate Makefile from CubeMX generated code"
  (interactive)
  (let ((dir (if path
		 path
	       (my-get-project-root-dir))))
    (when dir
      (let ((pth (concat dir "CubeMX2Makefile/CubeMX2Makefile.py")))
	(when (file-exists-p pth)
	  (progn	   
	    (message (shell-command-to-string (concat "/usr/bin/python " pth " " dir)))
	    (message "ok")
	    (my-load-project dir)))))))

(defun my-load-project (&optional path)
   "Load project.el of current project"
   (interactive)
   (let ((dir (if path
		  path
		(my-get-project-root-dir))))
    (when dir
      (let ((pth (concat dir "/project.el")))
	(message pth)
	(when (file-exists-p pth)
	  (progn
	    (message "file exists")
	    (load-file pth)
	    (message (concat pth " loaded"))))))))

(defun stm32-new-project ()
  "Create new stm32  project from existing code"
  (interactive)
  (let ((fil (read-directory-name "Select STM32CubeMx directory: ")))
    (when (file-exists-p fil)
      (when (yes-or-no-p (concat "Create project in " fil " ?"))
	(progn
	  (message "copying CubeMX2Makefile")
	  (copy-directory (concat user-emacs-directory "stm32/CubeMX2Makefile")
			  (concat fil "/CubeMX2Makefile"))
	  (message "Add to ede projects")
	  (ede-check-project-directory fil)
	  (message "First generate")
	  (stm32-generate-makefile fil)
	  (message "done")
	  (my-load-project fil))))))

(defun stm32-run-st-util ()
  "Run st-util gdb server"
  (interactive)
  (with-temp-buffer "*st-util*"
		    (async-shell-command *stm32-st-util*
					 "*st-util*"
					 "*Messages*")
		    (pop-to-buffer "*st-util*")))
(defun stm32-start-gdb ()
  "Strart gud arm-none-eabi-gdb and connect to st-util"
  (interactive)
  (let ((dir (my-get-project-root-dir))
	(name (my-get-project-name)))
    (when dir
      (let ((pth (concat dir "build/" name ".elf")))
	(when (file-exists-p pth)
	  (progn
	    (message pth)
	    (gdb (concat "arm-none-eabi-gdb -iex \"target extended-remote localhost:4242\" -i=mi " pth))))))))

(defun stm32-load-all-projects ()
  "Reads all directories from 'ede-project-directories and loads project.el"
  (interactive)
  (or (eq ede-project-directories t)
      (and (functionp ede-project-directories)
	   (funcall ede-project-directories dir))
      ;; If `ede-project-directories' is a list, maybe add it.
      (when (listp ede-project-directories)
	(dolist (x ede-project-directories)
	  (when (file-exists-p (concat x "/project.el"))
	    (my-load-project x))))))

(provide 'stm32)

;;; stm32.el ends here
