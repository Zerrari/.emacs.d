;; -----------------------------------------------------------------------------
;; quick-file-jump.el
;; Why this module?
;;     Sometimes, we need to open a file or buffer which name 
;;            began with current word in emacs.
;;     Here is the solution.
;;
;; Install.
;;   put this file (quick-file-jump.el) in your load path and
;;   add follow codes into your initial emacs files (.emacs or init.el)
;;   (require 'quick-file-jump)
;;   (global-set-key (kbd "<M-return>") 'ab/quick-buffer-jump)
;;
;; Author:
;;   Aborn Jiang (aborn.jiang@foxmail.com)
;;   2014-05-13
;; -----------------------------------------------------------------------------
 
(provide 'quick-file-jump)
;; default global key setting
(global-set-key (kbd "<M-return>") 'ab/quick-file-jump)
