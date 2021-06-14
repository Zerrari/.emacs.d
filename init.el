;;; init.el ---                            -*- lexical-binding: t; -*-

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

;; Test startup time
;(require 'benchmark-init-loaddefs)
;(benchmark-init/activate)



(let (
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

    (defvar zerrari-emacs-root-dir "~/.emacs.default")
    (defvar zerrari-emacs-config-dir (concat zerrari-emacs-root-dir "/lisp"))
    (defvar zerrari-emacs-plugin-dir (concat zerrari-emacs-root-dir "/plugins"))


    ;; Add plugins dir with its subdirs
    (let ((default-directory  zerrari-emacs-plugin-dir))
	(normal-top-level-add-subdirs-to-load-path))

    ;; Add pluins dir to load path
    (add-to-list 'load-path zerrari-emacs-config-dir)
    (add-to-list 'load-path zerrari-emacs-plugin-dir)

    (with-temp-message ""                 ;抹掉插件启动的输出

	;; Make sure ENVs in Emacs same as in shell
	;; (require 'esup)

	(require 'exec-path-from-shell)

	(when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))

	(require 'init-ui)
	(require 'init-utils)

	(require 'init-themes)
	(require 'init-lazyload)

	(require 'init-evil)
	(require 'init-swiper)
	(require 'init-awesome)

	;; (require 'init-functions)
	;; (require 'init-packages)

	(require 'init-general)

	;; 可以延后加载的
	(run-with-idle-timer
	1 nil
	#'(lambda ()
	    (require 'init-company)
	    ;; (require 'init-snails)
	    (require 'init-whichkey)
	    (require 'init-markdown)
	    (require 'init-quickrun)
	    ;; (require 'init-wakatime)
	    (require 'init-flycheck)
	    (require 'init-smartparens)

	    ;; (require 'init-smex)
	    (require 'init-amx)

	    (require 'init-icons)
	    ;; (require 'init-vterm)
	    (require 'init-arduino)
	    ;; (require 'init-tools)
	    (require 'init-diminish)
	    ))))

;;; init.el ends here
