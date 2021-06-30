;;; init-utils.el ---                     -*- lexical-binding: t; -*-

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
;; (require 'highlight-indent-guides)
(require 'rainbow-delimiters)
(require 'hungry-delete)
;(require 'format-all)

(indent-guide-global-mode)

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (setq highlight-indent-guides-method 'bitmap)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-hungry-delete-mode)

;; disable key suggest in the echo area.
(setq suggest-key-bindings nil)

(setq help-window-select t)

(setq tab-width 4)

(setq-default indent-tabs-mode t)

(setq-default c-default-style "linux")

(setq auto-save-default nil)

(setq make-backup-files nil)

(setq visible-bell nil)

(setq split-height-threshold nil)

(setq split-width-threshold 0)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(electric-indent-mode 1)

(global-eldoc-mode 0)

(desktop-save-mode 1)

(setq make-backup-files nil)

(setq auto-save-list-file-prefix nil)

(setq default-directory "~/")

(setq custom-file "~/.emacs.default/site-lisp/config/init-custom.el" )

(setq recentf-save-file "~/.emacs.default/var/recentf")

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

(global-prettify-symbols-mode 1)

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805))))

(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)


(setq initial-major-mode 'text-mode)
;; https://www.youtube.com/watch?v=NfjsLmya1PI
(setq initial-scratch-message "Stay hungry,stay foolish.\n")

;; recentf
(require 'recentf)
(recentf-mode 1)

;; define functions

(defun display-startup-echo-area-message ()
  (message nil))

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

(defun quick-save-buffers ()
  "Save buffers quickly and silently."
  (interactive)
  (save-some-buffers t))

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
  (interactive)
  (byte-recompile-directory zerrari-emacs-plugin-dir 0 0)
  message "Compilation completed!")

(defun quick-buffer-jump ()
  "Quickly jump to buffer/file which name is current word."
  (interactive)
  (setq fname (current-word))
  (setq blist (buffer-list))
  (setq status nil)
  (setq switchedbuffer "nil")
  (dolist (value blist)
    (when (and (bufferp value)
               (buffer-file-name value)
               (not status)
               (string-match (concat "^" (regexp-quote fname))
                             (buffer-name value)))
      (progn (switch-to-buffer (buffer-name value))
             (setq status t)
             (setq switchedbuffer (buffer-name value)))
      ))
  (if status                     ;; success search in buffer list.
      (message "skip to %s buffer" switchedbuffer)
    (quick-file-jump)))       ;; find files in current path.

(defun quick-file-jump ()
  "Quickly open and jump file with name begin with current word."
  (interactive)
  (setq fname (current-word))
  (setq switchedfile "nil")
  ;; (setq dflist (directory-files (get-current-path)))
  (setq dflist (directory-files-recursively (get-current-path) ""))
  (setq status nil)
  (dolist (value dflist)
    (when (and (file-regular-p value)
               (string-match
		(regexp-quote fname) value))
      (find-file value)
      (setq switchedfile value)
      (setq status t)))
  (if status
	  (message "open and skip to %s file." switchedfile)
	(message "no file.")))

(defun get-current-path ()
  "Get the current path."
  (interactive)
  (message (file-name-directory (buffer-file-name))))

;; Disable buffer end warnings.
;; https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

(provide 'init-utils)

;;; init-utils.el ends here
