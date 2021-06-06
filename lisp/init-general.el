(require 'general)

(general-evil-setup)

(general-nmap
  :prefix "SPC"
  "c" 'evilnc-comment-or-uncomment-lines
  "r" 'quickrun)

(general-nmap
  :prefix "SPC s"
  "s" 'split-window-right
  "o" 'other-window)

(general-nmap
 :prefix "SPC b"
 "e" 'eval-buffer
 "p" 'change-previous-buffer
 "w" 'save-buffer
 "q" 'save-buffers-kill-emacs)

(provide 'init-general)
