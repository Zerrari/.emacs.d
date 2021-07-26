;;; init-evil.el --- Evil Configuration              -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zerrari

;; Author: zerrari <zerrari@zhangyizhongdeMacBook-Pro.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

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
(setq key-chord-two-keys-delay 0.7)
(key-chord-mode 1)

(define-key evil-normal-state-map (kbd "J") 'fast-forward)
(define-key evil-normal-state-map (kbd "K") 'fast-backward)
(define-key evil-normal-state-map (kbd "H") 'goto-line-beginning)
(define-key evil-normal-state-map (kbd "L") 'goto-line-end)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(provide 'init-evil)
;;; init-evil.el ends here
