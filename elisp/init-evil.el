;;; package --- summary

;;; init-evil.el

;;; Commentary:

;;; Code:

(use-package evil
  :init (evil-mode 1)
  :bind
  (("M-h" . evil-normal-state)))

(use-package evil-mc
  :diminish
  :config
  (global-evil-mc-mode 1))

(use-package evil-nerd-commenter
  :defer t
  :diminish)


(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
