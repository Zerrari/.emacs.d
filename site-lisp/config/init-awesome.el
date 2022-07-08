;;; init-awesome.el --- Awesome Configuration        -*- lexical-binding: t; -*-

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

(require 'awesome-tab)
(require 'awesome-tray)

(awesome-tab-mode t)
(awesome-tray-mode 1)

(setq awesome-tab-label-fixed-length 0)

(setq awesome-tab-height 150)

(setq awesome-tray-active-modules nil)

(add-to-list 'awesome-tray-active-modules "buffer-name")

(setq awesome-tray-mode-line-active-color "#2fff2f")

(setq awesome-tray-mode-line-colors "#2fff2f")

(provide 'init-awesome)
;;; init-awesome.el ends here
