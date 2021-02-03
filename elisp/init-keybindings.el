;;; package -- summary
;;; init-keybindings

;;; Commentary:

;;; Code:

;; Zerrari's keybindings

;; define a keymap

(evil-global-set-key 'normal "j" 'evil-backward-char)
(evil-global-set-key 'normal "k" 'evil-next-line)
(evil-global-set-key 'normal "l" 'evil-previous-line)
(evil-global-set-key 'normal ";" 'evil-forward-char)

(define-prefix-command 'zerrari-keymap)
(define-key zerrari-keymap (kbd "d") 'zerrari-kill-comments)
(define-key zerrari-keymap (kbd "i") 'zerrari-org-insert-src-blocks)
(define-key zerrari-keymap (kbd "p") 'zerrari-package-description)

;; define a prefix key
(global-set-key (kbd "M-z") zerrari-keymap)

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
