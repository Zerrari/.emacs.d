(require 'nox)

(dolist (hook (list
               'python-mode-hook
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook '(lambda () (nox-ensure))))

(provide 'init-nox)
