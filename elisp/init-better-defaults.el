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

(defun newc()
  "Insert a template for c source code"
  (interactive)
  (insert "#include <stdio.h>\n"
	  "#include <stdlib.h>\n"
	  "\n"
	  "int main()\n"
	  "{\n"
	  "\n"
	  "    return 0;\n"
	  "}")
  (forward-line -2)
)

(defun newcpp()
  "Insert a template for c source code"
  (interactive)
  (insert "#include <iostream>\n"
	  "\n"
	  "using namespace std;\n"
	  "\n"
	  "int main()\n"
	  "{\n"
	  "\n"
	  "    return 0;\n"
	  "}")
  (forward-line -2)
)

(add-hook 'c-mode-hook
          (lambda ()
            (if (= (buffer-size) 0)
	    (progn 
                (newc)
            (message "load a c template!")))
            ))

(add-hook 'c++-mode-hook
          (lambda ()
            (if (= (buffer-size) 0)
		(progn
                (newcpp)
            (message "load a cpp template!")))
            ))
 
;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(add-hook 'leetcode--loading-mode-hook 'evil-emacs-state)

(provide 'init-better-defaults)

