;;; init-diminish.el --- Diminish Configuration      -*- lexical-binding: t; -*-

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

(require 'diminish)

(diminish 'which-key-mode)
(diminish 'smartparens-mode)
(diminish 'ivy-mode)
(diminish 'flycheck-mode)
(diminish 'company-mode)
(diminish 'indent-guide-mode)
(diminish 'rainbow-delimiters-mode)
(diminish 'hungry-delete-mode)
(diminish 'evil-collection-unimpaired-mode)
;; (diminish 'highlight-indent-guides-mode)
;; (diminish 'global-wakatime-mode)

(provide 'init-diminish)
;;; init-diminish.el ends here
