(require 'general)
(require 'hydra)

(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                    :post (progn
                            (set-cursor-color "#ffffff")
                            (message
                             "Thank you, come again.")))
  "vi"
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
    "c" 'evilnc-comment-or-uncomment-lines
    "r" 'quickrun)

(general-define-key
    :states 'normal
    :prefix "SPC w"
    :keymaps 'override
    "k" 'delete-window
    "s" 'split-window-right
    "o" 'other-window)

(general-define-key
    :states 'normal
    :prefix "SPC f"
    :keymaps 'override
    "d" 'delete-this-file
    "f" 'quick-file-jump
    "r" 'rename-this-file-and-buffer
    "s" 'quick-load-init-file)

(general-define-key
    :states 'normal
    :prefix "SPC b"
    :keymaps 'override
    "k" 'kill-buffer-and-window
    "e" 'eval-buffer
    "b" 'ivy-switch-buffer
    "p" 'change-previous-buffer
    "w" 'quick-save-buffers
    "q" 'save-buffers-kill-emacs)

(general-define-key
    :states 'normal
    :prefix "SPC a"
    :keymaps 'override
    "j" 'awesome-tab-backward
    "k" 'awesome-tab-forward
    "d" 'awesome-tab-kill-all-buffers-in-current-group
    "g" 'awesome-tab-switch-group
    "h" 'awesome-tab-ace-jump)

(provide 'init-keymap)
