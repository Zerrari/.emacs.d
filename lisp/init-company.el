(require 'company)

(global-company-mode)

(setq company-minimum-prefix-length 1)

(setq company-idle-delay 0.1)

(setq company-tooltip-limit 5)

(setq company-show-numbers t)

(setq company-tooltip-align-annotations t)

(setq company-tooltip-margin 1)

(add-hook 'emacs-lisp-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-elisp company-dabbrev company-files))))
(add-hook 'c-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-dabbrev company-files))))
(add-hook 'c++-mode-hook
    (lambda ()
    (set (make-local-variable 'company-backends) '(company-dabbrev company-files))))

(provide 'init-company)
