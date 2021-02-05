;;; package --- summary

;;; init-hexo.el

;;; Commentary:

;;; Code:

(defun hexo-my-blog ()
    (interactive)
    (hexo "~/Documents/blog/"))


(define-prefix-command 'hexo-mode-map)

(global-set-key (kbd "M-p") hexo-mode-map)

(define-key hexo-mode-map (kbd "b") 'hexo-my-blog)

(use-package hexo
  :init
   (setq hexo-posix-compatible-shell-file-path "/usr/local/bin/fish"))

(provide 'init-hexo)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hexo.el ends here
