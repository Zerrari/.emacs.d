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

(use-package indent-guide
  :diminish
  :config
  (indent-guide-global-mode))

(use-package iedit
    :diminish)


(use-package autoinsert
  :diminish
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (define-auto-insert "\.cpp" "cpp-template.cpp")
  (define-auto-insert "\.c" "c-template.c"))
  

(use-package all-the-icons)
  :diminish

(use-package all-the-icons-dired
  :diminish
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; smart parens
(use-package smartparens
  :diminish
  :config
  (smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  )



;; ranger
(use-package ranger
  :diminish
  )

;; flycheck
(use-package flycheck
  :diminish
  :ensure t
  :init (global-flycheck-mode))

(use-package quickrun
 :diminish)

(use-package markdown-mode
  :diminish
  )

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
   ("C-x b" . ivy-switch-buffer)
   ("C-c s" . counsel-fzf)))

;; evil
(use-package evil
  :diminish
  :init (evil-mode 1)
  :bind
  (("M-h" . evil-normal-state)))

(use-package evil-nerd-commenter
  :defer t
  :diminish)

(evil-define-motion fast-forward ()
  :type inclusive
  (forward-line 10))

(evil-define-motion fast-backward ()
  :type inclusive
  (previous-line 10))

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
    "m" 'iedit-mode
    "c" 'evilnc-comment-or-uncomment-lines
    "d" 'delete-window
    "k" 'kill-current-buffer
    "r" 'quickrun
    "b" 'ivy-switch-buffer
    "q" 'save-buffers-kill-emacs
    "lt" 'leetcode-try
    "ls" 'leetcode-submit))
  
;; key-chord
(use-package key-chord
  :diminish
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; diminish
(use-package diminish
  :defer t)

(use-package leetcode
  :diminish
  :config
  (setq leetcode-prefer-language "c")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/Codes/leetcode/algorithms")
  )

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
  (setq company-tooltip-margin 1))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here


