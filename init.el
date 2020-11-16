(load-file "~/.emacs.d/elisp/init-ui.el")
(load-file "~/.emacs.d/elisp/init-theme.el")
(load-file "~/.emacs.d/elisp/init-org.el")
(load-file "~/.emacs.d/elisp/init-better-defaults.el")
(load-file "~/.emacs.d/elisp/init-keybindings.el")
(load-file "~/.emacs.d/elisp/init-package.el")

;; package install
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "94a94c957cf4a3f8db5f12a7b7e8f3e68f686d76ae8ed6b82bd09f6e6430a32c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(package-selected-packages
   '(flycheck cyberpunk-2019-theme cyberpunk-theme doom-themes evil 2048-game org smartparens monokai-theme hungry-delete counsel company))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spaceline smartparens monokai-theme hungry-delete flycheck evil doom-themes cyberpunk-theme cyberpunk-2019-theme counsel company 2048-game)))
