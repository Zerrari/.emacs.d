;;; package -- summary
;;; init-keybindings

;;; Commentary:

;;; Code:

;; Zerrari's keybindings

;; define a keymap

(define-key evil-normal-state-map (kbd "J") 'fast-forward)
(define-key evil-normal-state-map (kbd "K") 'fast-backward)
(define-key evil-normal-state-map (kbd "H") 'goto-line-beginning)
(define-key evil-normal-state-map (kbd "L") 'goto-line-end)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
