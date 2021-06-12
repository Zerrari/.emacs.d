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


(defvar zerrari-emacs-root-dir "~/.emacs.default")

(defvar zerrari-emacs-config-dir (concat zerrari-emacs-root-dir "/lisp"))

(defvar zerrari-emacs-plugin-dir (concat zerrari-emacs-root-dir "/plugins"))

(let ((default-directory  zerrari-emacs-plugin-dir))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path zerrari-emacs-config-dir)
(add-to-list 'load-path zerrari-emacs-plugin-dir)

;; test startup time
;(require 'benchmark-init-loaddefs)
;(benchmark-init/activate)

(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1086 5))

(setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
			:type (:@type "proxyTypeSocks5")))

(require 'init-ui)
(require 'init-utils)
(require 'init-lazyload)
;(require 'init-packages)
(require 'init-evil)
(require 'init-company)
(require 'init-themes)
(require 'init-smartparens)
(require 'init-swiper)
;(require 'init-smex)
(require 'init-amx)
(require 'init-whichkey)
(require 'init-markdown)
(require 'init-quickrun)
;; (require 'init-wakatime)
(require 'init-flycheck)
(require 'init-icons)
;; (require 'init-vterm)
(require 'init-diminish)
(require 'init-arduino)
(require 'init-general)
(require 'telega)
;; (require 'init-functions)

;;; init.el ends here
