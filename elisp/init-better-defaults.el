;;; init-better-defaults1.el ---                     -*- lexical-binding: t; -*-

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

;; https://www.reddit.com/r/emacs/comments/701pzr/flycheck_error_tally_in_custom_mode_line/

(defun d/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "0"))
         (running (eq 'running flycheck-last-status-change)))
    (cond ((not (eq err 0)) (format "E:•%s" err))
	  (t (format "E:•0")))))

;; format some information on the right side of mode line
;; https://stackoverflow.com/questions/16775855/how-to-fixate-value-on-the-right-side-of-the-modeline/22971471#22971471
;; https://github.com/xiongtx/.emacs.d/blob/347d9990a394fbcb222e4cda9759743e17b1977a/init.org#mode-line

;; (defun mode-line-fill (face reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and window-system (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
;;               'face face))

;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(display-battery-mode 1)

(setq global-mode-string (delq 'battery-mode-line-string global-mode-string))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote ("[%*] "
	     evil-mode-line-tag
	     " %b "
             " l : %l  "
             ))
     ;; Right.
     (quote (
	     (:eval
     (concat
      (cl-loop for state in '((error . "#FB4933"))
               as lighter = (d/flycheck-lighter (car state))
               when lighter
               concat (propertize
                       lighter
                       'face `(:foreground ,(cdr state))))
      " "))
             mode-line-modes))))))
;; (setq-default mode-line-format
;;       (list
;;        "%+  "
;;        "---%b---"
;;        "  (%m) "
;;        'battery-mode-line-string
;;        ))


;; recentf
(require 'recentf)
(recentf-mode 1)

;; define functions
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

(add-hook 'leetcode--loading-mode-hook 'evil-emacs-state)

(provide 'init-better-defaults)

;;; init-better-defaults1.el ends here
