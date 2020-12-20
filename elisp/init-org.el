;;; package --- summary
;;; init-org

;;; Commentary:

;;; Code:
(require 'org)
(setq org-src-fontify-natively t)
(require 'org-tempo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp t)))

(setq org-todo-keywords '((type "study" "amusement" "|" "DONE")))

;; (require 'org-evil)


;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
