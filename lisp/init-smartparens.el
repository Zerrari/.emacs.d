(require 'smartparens)

(smartparens-global-mode)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)

(provide 'init-smartparens)
