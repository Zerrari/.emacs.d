(require 'flycheck)

(global-flycheck-mode)

;; https://www.reddit.com/r/emacs/comments/701pzr/flycheck_error_tally_in_custom_mode_line/

(defun d/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "0"))
         (running (eq 'running flycheck-last-status-change)))
    (cond ((not (eq err 0)) (format "E : • %s" err))
	  (t (format "E : • 0")))))

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
	     (:eval (propertize " %b " 'face '(:foreground "orange")))
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



(provide 'init-flycheck)
