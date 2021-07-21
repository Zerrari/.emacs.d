(require 'lsp-mode)

(require 'lsp-jedi)
(require 'ccls)

;; (add-to-list 'lsp-enabled-clients 'jedi)
(add-to-list 'lsp-enabled-clients 'ccls)

(setq ccls-executable "/usr/local/bin/ccls")


(provide 'init-lsp)
