(require 'general)

(general-evil-setup)

(general-nmap
  :prefix "SPC"
  "c" 'evilnc-comment-or-uncomment-lines
  "r" 'quickrun)

(general-nmap
  :prefix "SPC w"
  "k" 'delete-window
  "s" 'split-window-right
  "o" 'other-window)

(general-nmap
  :prefix "SPC f"
  "d" 'delete-this-file
  "f" 'quick-file-jump
  "r" 'rename-this-file-and-buffer
  "s" 'quick-load-init-file)

(general-nmap
 :prefix "SPC b"
 "k" 'kill-buffer-and-window
 "e" 'eval-buffer
 "b" 'ivy-switch-buffer
 "p" 'change-previous-buffer
 "w" 'save-buffer
 "q" 'save-buffers-kill-emacs)

(general-nmap
  :prefix "SPC a"
  "j" 'awesome-tab-backward
  "k" 'awesome-tab-forward
  "g" 'awesome-tab-switch-group
  "h" 'awesome-tab-ace-jump)

(provide 'init-general)
