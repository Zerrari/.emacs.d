;;; package --- summary

;;; init-modeline.el

;;; Commentary:

;;; Code:

;; minimodeline
(use-package mini-modeline
  :disabled
  :diminish
  :config
  (mini-modeline-mode t))

;; spaceline
(use-package spaceline
  :config
  (spaceline-spacemacs-theme))

;; doom modeline
(use-package doom-modeline
  :disabled
  :init
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-height 2)
  :config
  (doom-modeline-mode 1))

(use-package powerline
  :disabled
  :config
  (powerline-default-theme))

(use-package powerline-evil
  :disabled
  :config
  (powerline-evil-vim-color-theme))

;; airline
;;(require 'airline-themes)
;;(load-theme 'airline-light t)

(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
