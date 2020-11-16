(setq make-backup-files nil)

(global-hl-line-mode 1)

;; recentf
(require 'recentf)
(recentf-mode 1)

;; define functions
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
