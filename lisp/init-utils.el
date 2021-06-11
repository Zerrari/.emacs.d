;;; init-better-defaults1.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zerrari

;; Author: zerrari <zerrari@zhangyizhongdeMacBook-Pro.local>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'indent-guide)
(require 'rainbow-delimiters)
(require 'hungry-delete)

(indent-guide-global-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-hungry-delete-mode)

(setq tab-width 4)

(setq-default indent-tabs-mode t)

(setq-default c-default-style "linux")

(setq make-backup-files nil)

(setq visible-bell nil)

(setq split-height-threshold nil)

(setq split-width-threshold 0)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(electric-indent-mode 1)

(global-eldoc-mode 0)

(setq make-backup-files nil)

(setq default-directory "~/")

(setq custom-file "~/.emacs.default/elisp/init-custom.el" )

;; recentf
(require 'recentf)
(recentf-mode 1)

;; define functions
(defun newc()
  "Insert a template for c source code."
  (interactive)
  (insert "#include <stdio.h>\n"
	  "#include <stdlib.h>\n"
	  "\n"
	  "int main()\n"
	  "{\n"
	  "\n"
	  "        return 0;\n"
	  "}")
  (forward-line -2)
)

(defun newcpp()
  "Insert a template for c source code."
  (interactive)
  (insert "#include <iostream>\n"
	  "\n"
	  "using namespace std;\n"
	  "\n"
	  "int main()\n"
	  "{\n"
	  "\n"
	  "        return 0;\n"
	  "}")
  (forward-line -2)
)

(add-hook 'c-mode-hook
    (lambda ()
	(if (= (buffer-size) 0)
	    (progn
		(newc)
		(message "load a c template!")))))
            

(add-hook 'c++-mode-hook
    (lambda ()
	(if (= (buffer-size) 0)
	    (progn
		(newcpp)
		(message "load a cpp template!")))))
 
;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun quick-load-init-file ()
  "Load init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file)
  (message "Initialization completed!"))

(defun change-previous-buffer ()
  "Quick change to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun compile-elisp-file ()
  "Compile elisp file to elc."
  (byte-recompile-directory zerrari-emacs-plugin-dir 0 0)
  (message "Compilation completed!"))

(defun install-package (pkg)
  (let ((command-string (concat "git submodule add " pkg " " )))))
  

(provide 'init-utils)

;;; init-utils.el ends here
