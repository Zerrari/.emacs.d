(setq evil-want-integration t) 
(setq evil-want-keybinding nil)
(setq evil-want-integration t) 
(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-nerd-commenter)
(require 'key-chord)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(evil-mode 1)

;; evil-leader
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key
;;     "e" 'eval-buffer
;;     "o" 'other-window
;;     "w" 'save-buffer
;;     "f" 'ranger
;;     "c" 'evilnc-comment-or-uncomment-lines
;;     "d" 'delete-window
;;     "k" 'kill-current-buffer
;;     "r" 'quickrun
;;     "b" 'change-previous-buffer
;;     "ss" 'split-window-right
;;     "q" 'save-buffers-kill-emacs)

(evil-define-motion fast-forward ()
  :type inclusive
  (forward-line 10))

(evil-define-motion fast-backward ()
  :type inclusive
  (forward-line -10))

(evil-define-motion goto-line-beginning ()
  :type inclusive
  (beginning-of-line))

(evil-define-motion goto-line-end ()
  :type inclusive
  (end-of-line))

;; key-chord
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(define-key evil-normal-state-map (kbd "J") 'fast-forward)
(define-key evil-normal-state-map (kbd "K") 'fast-backward)
(define-key evil-normal-state-map (kbd "H") 'goto-line-beginning)
(define-key evil-normal-state-map (kbd "L") 'goto-line-end)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(provide 'init-evil)
