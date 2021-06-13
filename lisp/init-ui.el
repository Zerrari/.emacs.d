;;; init-ui.el ---                                  -*- lexical-binding: t; -*-

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

(setq ring-bell-function 'ignore blink-cursor-mode nil)

(set-face-attribute 'default nil :font "monaco 24")

;; shut down auto-save mode
(auto-save-mode -1)

;; set line number
(global-linum-mode 1)

;; set cursor type
(setq-default cursor-type 'bar)

;; skip welcome page
(setq inhibit-splash-screen 1)

;; set fullscreen
(setq initial-frame-alist (quote((fullscreen . maximized))))

(provide 'init-ui)

;;; init-ui.el ends here
