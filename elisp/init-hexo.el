;;; package --- summary

;;; init-hexo.el

;;; Commentary:

;;; Code:

(use-package hexo
  :init
   (setq hexo-posix-compatible-shell-file-path "/usr/local/bin/fish"))

(define-prefix-command 'hexo-mode-map)
(global-set-key (kbd "M-b") hexo-mode-map)

(defun hexo-my-blog ()
    (interactive)
    (hexo "~/Documents/blog/"))

(define-key hexo-mode-map (kbd "s") 'hexo-my-blog)

(provide 'init-hexo)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hexo.el ends here
