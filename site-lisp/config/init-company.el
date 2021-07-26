;;; init-company.el --- Company Configuration        -*- lexical-binding: t; -*-

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

(require 'company)

(global-company-mode)

(setq company-minimum-prefix-length 1)

(setq company-idle-delay 0.1)

(setq company-tooltip-limit 5)

(setq company-show-numbers t)

(setq company-tooltip-align-annotations t)

(setq company-tooltip-margin 1)

(add-hook 'emacs-lisp-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-elisp company-dabbrev company-files))))
(add-hook 'c-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-dabbrev company-files))))
(add-hook 'c++-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-dabbrev company-files))))

(provide 'init-company)
;;; init-company.el ends here
