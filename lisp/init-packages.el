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

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)

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


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package indent-guide
  :diminish
  :config
  (indent-guide-global-mode))

(use-package hungry-delete
  :diminish
  :config
  (global-hungry-delete-mode))

(use-package autoinsert
  :diminish
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil))
  

(use-package all-the-icons
  :diminish)

(use-package all-the-icons-dired
  :diminish
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; (use-package projectile
;;   :diminish
;;   :config
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; smart parens
(use-package smartparens
  :diminish
  :config
  (smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))
  
;; ranger
(use-package ranger
  :diminish)
  

;; flycheck
(use-package flycheck
  :diminish
  :ensure t
  :init (global-flycheck-mode))

(use-package quickrun
 :diminish)

(use-package markdown-mode
  :diminish)
  

(use-package wakatime-mode
  :diminish
  :config
  (setq wakatime-api-key "128b0b23-a6c0-4e02-9615-6c26286fc70f")
  (global-wakatime-mode))

(use-package smex
  :diminish
  :config
  (global-set-key (kbd "M-x") 'smex))

;; which-key
(use-package which-key
  :defer 2
  :diminish
  :config
  (which-key-mode))

;; ivy
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
   (("C-s" . swiper-isearch)
    ("C-c s" . counsel-fzf)))

;; evil
(use-package evil
  :diminish
  :init (evil-mode 1))
  

(use-package evil-nerd-commenter
  :defer t
  :diminish)

(evil-define-motion fast-forward ()
  :type inclusive
  (forward-line 10))

(evil-define-motion fast-backward ()
  :type inclusive
  (forward-line -10))

(evil-define-motion goto-line-beginning ()
  :type inclusive
  (beginning-of-line))

(evil-define-motion goto-line-end ()
  :type inclusive
  (end-of-line))

;; evil-leader
(use-package evil-leader
  :diminish
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "e" 'eval-buffer
    "o" 'other-window
    "w" 'save-buffer
    "f" 'ranger
    "c" 'evilnc-comment-or-uncomment-lines
    "d" 'delete-window
    "k" 'kill-current-buffer
    "r" 'quickrun
    "b" 'ivy-switch-buffer
    "s" 'split-window-right
    "q" 'save-buffers-kill-emacs))
  
;; key-chord
(use-package key-chord
  :diminish
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1))

(use-package evil-surround
  :diminish
  :config
  (global-evil-surround-mode 1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; diminish
(use-package diminish
  :defer t)

;; company
(use-package company
  :diminish
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 5)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-margin 1)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	    (set (make-local-variable 'company-backends) '(company-elisp company-dabbrev company-files))))
  (add-hook 'c-mode-hook
	    (lambda ()
	    (set (make-local-variable 'company-backends) '(company-dabbrev company-files))))
  (add-hook 'c++-mode-hook
	    (lambda ()
	    (set (make-local-variable 'company-backends) '(company-dabbrev company-files)))))

(provide 'init-packages)

;;; init-packages.el ends here
