;;; package --- summary
;;; init-better-defaults

;;; Commentary:

;;; Code:

(setq make-backup-files nil)

(global-hl-line-mode 1)

(electric-indent-mode 1)

;; recentf
(require 'recentf)
(recentf-mode 1)


;; define functions
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun zerrari-kill-line()
  (interactive)
  (progn
    (beginning-of-line)
    (kill-line)))

(defun zerrari-org-insert-src-blocks()
  (interactive)
  (progn
    (insert "#+begin_src emacs-lisp\n")
    (newline)
    (insert "#+end_src")
    (previous-line)))

(defun zerrari-kill-comments()
  (interactive)
  (progn
    (goto-char (+ 3 (point)))
    (delete-backward-char 3)))

(defun zerrari-package-description()
  (interactive)
  (progn
    (let ((name (buffer-name)))
    (insert ";;; package --- summary\n")
    (newline)
    (insert ";;; " name "\n")
    (newline)
    (insert ";;; Commentary:\n")
    (newline)
    (insert ";;; Code:\n")
    (newline)
    (newline)
    (newline)
    (insert ";;;;;;;;;;;;;;;;;;;;;;;;;\n")
    (insert ";;; " name " ends here")
    (goto-line 9))))

;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(provide 'init-better-defaults)

