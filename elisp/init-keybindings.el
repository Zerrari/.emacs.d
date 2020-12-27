;;; package -- summary
;;; init-keybindings

;;; Commentary:

;;; Code:

;; Zerrari's keybindings

;; define a keymap

(define-prefix-command 'zerrari-keymap)
(define-key zerrari-keymap (kbd "d") 'zerrari-kill-comments)
(define-key zerrari-keymap (kbd "i") 'zerrari-org-insert-src-blocks)
(define-key zerrari-keymap (kbd "p") 'zerrari-package-description)

;; define a prefix key
(global-set-key (kbd "M-z") zerrari-keymap)

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
