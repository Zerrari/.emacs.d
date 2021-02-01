;;; package --- summary

;;; init.el


;;;  #######
;;;       #  ###### #####  #####    ##   #####  #
;;;      #   #      #    # #    #  #  #  #    # #
;;;     #    #####  #    # #    # #    # #    # #
;;;    #     #      #####  #####  ###### #####  #
;;;   #      #      #   #  #   #  #    # #   #  #
;;;  ####### ###### #    # #    # #    # #    # #

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'init-package)
(require 'init-better-defaults)
(require 'init-keybindings)
(require 'init-org)
(require 'init-ui)
(require 'init-theme)
(require 'init-company)
(require 'init-ivy)
(require 'init-yasnippet)
(require 'init-youdao)
(require 'init-dired)
(require 'init-markdown)
(require 'init-hexo)
(require 'init-evil)
(require 'init-modeline)
(require 'init-eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" default))
 '(package-selected-packages
   '(anaconda-mode eglot ccls lsp-ui ivy powerline powerline-evil evil-mc evil-nerd-commenter goto-chg iedit company-posframe selectrum mini-modeline zenburn-theme edit-indirect exec-path-from-shell markdown-mode cl-generic cl-lib hexo dired-k pdf-tools indent-guide expand-region smex ace-window quickrun multiple-cursors dired-icon figlet all-the-icons-dired youdao-dictionary yasnippet which-key vterm use-package spaceline smartparens rainbow-delimiters popup-kill-ring perspeen org-bullets neotree monokai-theme monitor keyfreq ivy-posframe hungry-delete flycheck evil-tutor evil-leader dracula-theme doom-themes doom-modeline diminish dashboard cyberpunk-theme cyberpunk-2019-theme counsel company benchmark-init avy airline-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
