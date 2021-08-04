;;; init-keymap.el --- Keymap Configuration          -*- lexical-binding: t; -*-

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

(require 'general)
(require 'hydra)

(defhydra hydra-vi (:hint nil
		    :pre (set-cursor-color "#40e0d0")
                    :post (progn
                            (set-cursor-color "#ffffff")
                            (message
                             "Thank you, come again.")))
"
^^^^^^^^-----------------------------------------------------------------
_l_: forward       _h_: backward        _j_: next       _k_: previous
"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("q" nil "quit"))


(with-eval-after-load "general"
    (general-evil-setup)

    (general-define-key
	:states '(normal visual emacs motion)
	:prefix "SPC"
	:keymaps '(override messages-buffer-mode-map)
	"a" 'hydra-awesometab/body
	"q" 'hydra-quit/body
	"b" 'hydra-buffers/body
	"c" 'hydra-comments/body
	"h" 'hydra-help/body
	"f" 'hydra-files/body
	"u" 'hydra-ui/body
	"w" 'hydra-windows/body
	"y" 'hydra-youdao/body
	"SPC" 'counsel-M-x
	"r" 'quickrun))



(defhydra hydra-awesometab (:hint nil
			    :idle 2
			    :pre (progn
				   (set-cursor-color "#40e0d0")
				   (message "Awesometab Mode"))
			    :post (set-cursor-color "#ffffff")
			    :exit t)
"
---------------------------------------------------------------------------------
                               Awesome Tab
_j_: tab backward    _k_: tab forward    _d_: kill buffers in current group
_a_: first tab       _s_: end tab        _r_: kill other buffers in current group
_g_: switch group    _h_: tab jump
"
    ("j" awesome-tab-backward)
    ("k" awesome-tab-forward)
    ("a" awesome-tab-select-beg-tab)
    ("s" awesome-tab-select-end-tab)
    ("r" awesome-tab-kill-other-buffers-in-current-group)
    ("d" awesome-tab-kill-all-buffers-in-current-group)
    ("g" awesome-tab-switch-group)
    ("h" awesome-tab-ace-jump)
    ("q" nil "quit"))

(defhydra hydra-buffers (:hint nil
			 :idle 2
			    :pre (progn
				   (set-cursor-color "#40e0d0")
				   (message "Buffers Mode"))
			    :post (set-cursor-color "#ffffff")
			 :exit t)
"
----------------------------------------------------------------------------------
                                   Buffers
_k_: kill buffer and window    _e_: eval buffer     _b_: switch buffer
_d_: kill buffer and window    _p_: previous buffer _s_: save buffers
"
    ("k" kill-buffer-and-window)
    ("d" kill-buffer-and-window)
    ("e" eval-buffer)
    ("b" ivy-switch-buffer)
    ("p" change-previous-buffer)
    ("s" quick-save-buffers)
    ("q" nil "quit"))

(defhydra hydra-ui (:hint nil
			 :idle 2
			    :pre (progn
				   (set-cursor-color "#40e0d0")
				   (message "UI Mode"))
			    :post (set-cursor-color "#ffffff")
			 :exit t)
"
----------------------------------------------------------------------------------
                                   UI
_i_: increase font size    _d_: decrease font size
"
    ("i" increase-font-size)
    ("d" decrease-font-size)
    ("q" nil "quit"))

(defhydra hydra-comments (:exit t
		          :idle 2
			 :pre (progn
				(set-cursor-color "#40e0d0")
				(message "Comments Mode"))

			    :post (set-cursor-color "#ffffff")
			  :hint nil)
"
-------------------------------------------------------------
                           Comments
_r_: comment or uncomment region
_c_: comment or uncomment lines
"
  ("r" comment-or-uncomment-region)
  ("c" evilnc-comment-or-uncomment-lines)
  ("q" nil "quit"))

(defhydra hydra-help (:hint nil
		       :idle 2
		       :pre (progn
			    (set-cursor-color "#40e0d0")
			    (message "Help Mode"))
			    :post (set-cursor-color "#ffffff")
		       :exit t)
"
-------------------------------------------------------------
                           Help
_f_: function     _k_: key    _v_: variable
_i_: Emacs info
"
    ("f" counsel-describe-function)
    ("k" describe-key)
    ("v" counsel-describe-variable)
    ("i" info)
    ("q" nil "quit"))


(defhydra hydra-files (:hint nil
		       :idle 2
		       :pre (progn
			    (set-cursor-color "#40e0d0")
			    (message "Files Mode"))
			    :post (set-cursor-color "#ffffff")
		       :exit t)
"
-------------------------------------------------------------
                           Files
_d_: delete file    _f_: find file          _r_: rename file
_s_: load config    _j_: quick file jump    _p_: open config
"
    ("d" delete-this-file)
    ("f" find-file)
    ("p" quick-open-my-config)
    ("j" quick-file-jump)
    ("r" rename-this-file-and-buffer)
    ("s" quick-load-init-file)
    ("q" nil "quit"))

(defhydra hydra-quit (:hint nil
			    :idle 2
		       :pre (progn
			    (set-cursor-color "#40e0d0")
			    (message "Quit Mode"))
			    :post (set-cursor-color "#ffffff")
			 :exit t)
"
-------------------------------------------------------------
                           Quit
_r_: restart Emacs    _q_: save and quit Emacs    _Q_: quit Emacs
"
    ("r" save-and-restart-emacs)
    ("q" save-buffers-kill-emacs)
    ("Q" kill-emacs))

(defhydra hydra-windows (:hint nil
			 :idle 2
			 :pre (progn
				(set-cursor-color "#40e0d0")
				(message "Windows Mode"))
			 :post (set-cursor-color "#ffffff")
			 :exit t)
"
-------------------------------------------------------------
                           Windows
_k_: kill window    _r_: split window right    _o_: other window
_d_: delete window  _d_: split window below
"
    ("k" delete-window)
    ("d" delete-window)
    ("r" split-window-right)
    ("d" split-window-below)
    ("o" other-window)
    ("q" nil "quit"))

(defhydra hydra-youdao (:hint nil
			 :idle 2
			 :exit t)
"
-------------------------------------------------------------
                           Youdao
_a_: search at point    _s_: search from input     _o_: posframe
"
    ("a" youdao-dictionary-search-at-point+)
    ("s" youdao-dictionary-search-from-input)
    ("o" youdao-dictionary-search-at-point-posframe)
    ("q" nil "quit"))

;; (general-define-key
;;     :states 'normal
;;     :prefix "SPC w"
;;     :keymaps 'override
;;     "k" 'delete-window
;;     "s" 'split-window-right
;;     "o" 'other-window)

;; (general-define-key
;;     :states 'normal
;;     :prefix "SPC f"
;;     :keymaps 'override
;;     "d" 'delete-this-file
;;     "f" 'quick-file-jump
;;     "r" 'rename-this-file-and-buffer
;;     "s" 'quick-load-init-file)

;; (general-define-key
;;     :states 'normal
;;     :prefix "SPC b"
;;     :keymaps 'override
;;     "k" 'kill-buffer-and-window
;;     "e" 'eval-buffer
;;     "b" 'ivy-switch-buffer
;;     "p" 'change-previous-buffer
;;     "w" 'quick-save-buffers
;;     "q" 'save-buffers-kill-emacs)

;; (general-define-key
;;     :states 'normal
;;     :prefix "SPC a"
;;     :keymaps 'override
;;     "j" 'awesome-tab-backward
;;     "k" 'awesome-tab-forward
;;     "d" 'awesome-tab-kill-all-buffers-in-current-group
;;     "g" 'awesome-tab-switch-group
;;     "h" 'awesome-tab-ace-jump)

(provide 'init-keymap)
;;; init-keymap.el ends here
