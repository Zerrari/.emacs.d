;;; package -- summary
;;; init-keybindings

;;; Commentary:

;;; Code:

;; Zerrari's keybindings

;; define a keymap

(define-key evil-insert-state-map (kbd "jj") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "J") 'fast-forward)
(define-key evil-normal-state-map (kbd "K") 'fast-backward)
(define-key evil-normal-state-map (kbd "H") 'goto-line-beginning)
(define-key evil-normal-state-map (kbd "L") 'goto-line-end)

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
