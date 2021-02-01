;;; package --- summary

;;; init-eglot.el

;;; Commentary:

;;; Code:

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((anaconda-mode "/usr/local/bin/pyls")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'anaconda-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(provide 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
