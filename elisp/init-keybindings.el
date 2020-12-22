;;; package -- summary
;;; init-keybindings

;;; Commentary:

;;; Code:
(global-set-key (kbd "<f1>") 'open-init-file)

;; (global-set-key (kbd "M-b") 'buffer-menu)

;; (global-set-key (kbd "M-s") 'counsel-switch-buffer)

(global-set-key (kbd "M-d") 'zerrari-kill-comments)

(global-set-key (kbd "M-i") 'zerrari-org-insert-src-blocks)

(global-set-key (kbd "M-r") 'recentf-open-files)

(global-set-key (kbd "M-h") 'evil-normal-state)

(global-set-key (kbd "M-f") 'keyfreq-show)

(global-set-key (kbd "M-p") 'zerrari-package-description)

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
