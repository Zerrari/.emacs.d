;;; init.el ---                                     -*- lexical-binding: t; -*-

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

(let ((default-directory  "~/.emacs.default/plugins"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.default/plugins")
(add-to-list 'load-path "~/.emacs.default/lisp")

(require 'init-ui)
(require 'init-better-defaults)
(require 'init-lazyload)
(require 'init-evil)
(require 'init-company)
(require 'init-themes)
(require 'init-smartparens)
(require 'init-swiper)
(require 'init-smex)

;;; init.el ends here
