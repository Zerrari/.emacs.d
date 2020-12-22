;;; init-package.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; package install
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20201110.2133/use-package.el")
  (require 'use-package))

;; benchmark-init
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; smart parens
(use-package smartparens
  :config
  (smartparens-global-mode))

;; perspeen
(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab nil)
  :config
  (perspeen-mode))

;; (use-package ivy-posframe
;;  :config
;;  (ivy-posframe-mode 1)
;;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))))

;; evil
;;(use-package evil
;;  :init (evil-mode 1))

;; evil-leader
;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key
;; "e" 'find-file)


;; eaf
;; (add-to-list 'load-path "~/.emacs.d/eaf/")
;; (require 'eaf)
;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;; spaceline
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
;; (spaceline-emacs-theme)

;; hungry dele
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))


;; airline
;;(require 'airline-themes)
;;(load-theme 'airline-light t)

;; doom modeline
(use-package doom-modeline
  :disabled
  :init
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-height 2)
  :config
  (doom-modeline-mode 1))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; vterm
(use-package vterm)

;; aweosome-tab
;;(load-file "~/.emacs.d/site-elisp/awesome-tab/awesome-tab.el")
;;(require 'awesome-tab)
;;(awesome-tab-mode t)
;;(setq awesome-tab-label-fixed-length 10)
;;(setq awesome-tab-height 100)

;; avy
(use-package avy
  :defer 2
  :bind
  (("M-z" . avy-goto-line)))


;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))


;; snails
;; (add-to-list 'load-path "~/.emacs.d/elpa/")
;; (require 'snails)

;; neotree
(use-package neotree
  :bind
  (([f8] . 'neotree-toggle)))

;; diminish
(use-package diminish
  :defer t)

;; all-the-icons
(use-package all-the-icons
  :defer 0
  :ensure t)

;; all-the-icons-dired
(use-package all-the-icons-dired
  :defer 0
  :ensure t)

;; figlet
(use-package figlet
  :defer 0
  :ensure t
  :config
  (setq figlet-default-font "banner"))

;; popup-kill-ring
(use-package popup-kill-ring
  :bind
  (("M-y" . popup-kill-ring)))

;; page-break-lines
(use-package page-break-lines
  :diminish
  :init
  (global-page-break-lines-mode))

;;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; flycheck
(use-package flycheck
  :hook
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; keyfreq
(use-package keyfreq
  :init
  (keyfreq-mode 1)
  :config
  (keyfreq-autosave-mode 1))

;; leetcode
(use-package leetcode
  :config
  (setq leetcode-prefer-language "c")
  (setq leetcode-directory "~/notes/leetcode")
  :bind
  (("M-l" . leetcode)))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  (("M-c" . mc/edit-lines)))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here


