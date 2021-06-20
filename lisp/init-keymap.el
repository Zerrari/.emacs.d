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


(general-evil-setup)

(general-define-key
    :states 'normal
    :prefix "SPC"
    :keymaps 'override
    "a" 'hydra-awesometab/body
    "b" 'hydra-buffers/body
    "c" 'hydra-comments/body
    "f" 'hydra-files/body
    "w" 'hydra-windows/body
    "r" 'quickrun)

(defhydra hydra-awesometab (:hint nil
			    :idle 2
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
			 :exit t)
"
----------------------------------------------------------------------------------
                                   Buffers
_k_: kill buffer and window    _e_: eval buffer   _b_: switch buffer
_p_: previous buffer           _w_: save buffers  _q_: save buffers and kill Emacs
" 
    ("k" kill-buffer-and-window)
    ("e" eval-buffer)
    ("b" ivy-switch-buffer)
    ("p" change-previous-buffer)
    ("w" quick-save-buffers)
    ("q" save-buffers-kill-emacs))

(defhydra hydra-comments (:exit t
		          :idle 2
			  :hint nil)
"
-------------------------------------------------------------
                           Comments
_r_: comment or uncomment region
_l_: comment or uncomment lines
" 
  ("r" comment-or-uncomment-region)
  ("l" evilnc-comment-or-uncomment-lines)
  ("q" nil "quit"))

(defhydra hydra-files (:hint nil
		       :idle 2
		       :exit t)
"
-------------------------------------------------------------
                           Files
_d_: delete file    _f_: quick file jump    _r_: rename file
_s_: load config
" 
    ("d" delete-this-file)
    ("f" quick-file-jump)
    ("r" rename-this-file-and-buffer)
    ("s" quick-load-init-file)
    ("q" nil "quit"))

(defhydra hydra-windows (:hint nil
			 :idle 2
			 :exit t)
"
-------------------------------------------------------------
                           Windows
_k_: delete window    _s_: spilt window right    _o_: other window
_s_: load config
" 
    ("k" delete-window)
    ("s" split-window-right)
    ("o" other-window)
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
