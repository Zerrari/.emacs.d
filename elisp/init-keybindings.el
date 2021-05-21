;;; init-keybindings1.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zerrari

;; Author: zerrari <zerrari@zhangyizhongdeMacBook-Pro.local>
;; Keywords: 

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

;; define a keymap

(define-key evil-normal-state-map (kbd "J") 'fast-forward)
(define-key evil-normal-state-map (kbd "K") 'fast-backward)
(define-key evil-normal-state-map (kbd "H") 'goto-line-beginning)
(define-key evil-normal-state-map (kbd "L") 'goto-line-end)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(provide 'init-keybindings)

;;; init-keybindings1.el ends here
