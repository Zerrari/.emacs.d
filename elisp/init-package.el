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

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; benchmark-init
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; expand-region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; indent guide
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;; iedit
(use-package iedit
  :diminish
  :defer t)

;; smex
;; (use-package smex
;;  :bind
;;  (("M-x" . smex)))

;; smart parens
(use-package smartparens
  :diminish
  :config
  (smartparens-global-mode))


(use-package ivy-posframe
  :diminish
  :config
  (ivy-posframe-mode 1)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))))

;; eaf
;; (add-to-list 'load-path "~/.emacs.d/eaf/")
;; (require 'eaf)

;; flycheck
(use-package flycheck
  :diminish
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))



;; hungry delete
(use-package hungry-delete
  :diminish
  :config
  (global-hungry-delete-mode))


;; which-key
(use-package which-key
  :diminish
  :config
  (which-key-mode))


;; aweosome-tab
;;(load-file "~/.emacs.d/site-elisp/awesome-tab/awesome-tab.el")
;;(require 'awesome-tab)
;;(awesome-tab-mode t)
;;(setq awesome-tab-label-fixed-length 10)
;;(setq awesome-tab-height 100)

;; avy
(use-package avy
  :diminish
  :defer 2
  :bind
  (("M-z" . avy-goto-line)))


;; rainbow-delimiters
(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; snails
;; (add-to-list 'load-path "~/.emacs.d/elpa/")
;; (require 'snails)

;; neotree
(use-package neotree
  :diminish
  :config
  :bind
  (([f8] . 'neotree-toggle)))

;; diminish
(use-package diminish
  :defer t)

;; all-the-icons
(use-package all-the-icons
  :disabled
  :defer 0
  :ensure t)
  

;; all-the-icons-dired
(use-package all-the-icons-dired
  :disabled
  :defer 0
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;; figlet
(use-package figlet
  :diminish
  :defer 0
  :ensure t
  :config
  (setq figlet-default-font "banner"))

;; popup-kill-ring
(use-package popup-kill-ring
  :disabled
  :bind
  (("M-y" . popup-kill-ring)))

;; page-break-lines
(use-package page-break-lines
  :diminish
  :init
  (global-page-break-lines-mode))

;;; dashboard
(use-package dashboard
  :diminish
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; selectrum
(use-package selectrum
  :diminish
  :config
  (selectrum-mode +1))



;; flycheck
(use-package flycheck
  :diminish
  :hook
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; keyfreq
(use-package keyfreq
  :diminish
  :init
  (keyfreq-mode 1)
  :config
  (keyfreq-autosave-mode 1)
  :bind
  (("M-f" . keyfreq-show)))

;; leetcode
(use-package leetcode
  :config
  (setq leetcode-prefer-language "c")
  (setq leetcode-directory "~/notes/leetcode")
  :bind
  (("M-l" . leetcode)))

;; multiple-cursors
(use-package multiple-cursors
  :disabled
  :diminish
  :bind
  (("M-c" . mc/edit-lines)))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here


