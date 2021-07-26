;;; init-swiper.el --- Swiper Configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zerrari

;; Author: zerrari<zerrari@zhangyizhongdeMacBook-Pro.local>
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

(require 'ivy)

(require 'swiper)
(require 'counsel)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(setq ivy-re-builders-alist
 '((counsel-rg . ivy--regex-plus)
  (swiper . ivy--regex-plus)
  (swiper-isearch . ivy--regex-plus)
  (t . ivy--regex-ignore-order)))

(setq counsel-find-file-ignore-regexp
        (concat
         ;; filename begins with #
         "\\(?:\\`[#.]\\)"
         ;; filename ends with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"
         "\\|\\.elc\\'"
         "\\|\\.pyc\\'"
         "\\|\\.meta\\'"
         ))


(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(provide 'init-swiper)
;;; init-swiper.el ends here
