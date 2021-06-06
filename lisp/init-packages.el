;;; init-packages.el                            -*- lexical-binding: t; -*-

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

(require 'cl)

(require 'exec-path-from-shell)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;(defvar zerrari/packages '(
		;;; --- Auto-completion ---
		;company
		;;; --- Better Editor ---
		;hungry-delete
		;ivy
		;swiper
		;counsel
		;smartparens
		;smex
		;; which-key
		;use-package
		;;; --- Vim ---
		;evil
		;evil-leader
		;evil-nerd-commenter
		;;evil-surround
		;key-chord
		;;; --- Major Mode ---
		;markdown-mode
		;;; --- Minor Mode ---
		;exec-path-from-shell
		;wakatime-mode
		;;; --- ui ---
		;all-the-icons
		;all-the-icons-dired
		;rainbow-delimiters
		;diminish
		;;; --- Themes ---
		;doom-themes
		;;; --- Files ---
		;ranger
		;;; --- Programming ---
		;flycheck
		;quickrun
		;;; projectile
		;))

;(setq package-selected-packages zerrari/packages)

 ;(defun my/packages-installed-p ()
     ;(loop for pkg in zerrari/packages
	   ;when (not (package-installed-p pkg)) do (return nil)
	   ;finally (return t)))

 ;(unless (my/packages-installed-p)
     ;(message "%s" "Refreshing package database...")
     ;(package-refresh-contents)
     ;(dolist (pkg zerrari/packages)
       ;(when (not (package-installed-p pkg))
	 ;(package-install pkg))))

(provide 'init-packages)

;;; init-packages.el ends here
