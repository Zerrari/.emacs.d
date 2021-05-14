;;; package --- summary
;;; init-better-defaults

;;; Commentary:

;;; Code:

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

;; recentf
(require 'recentf)
(recentf-mode 1)

;; define functions
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(add-hook 'leetcode--loading-mode-hook 'evil-emacs-state)

(provide 'init-better-defaults)

