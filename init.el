;; turn off the bell
(setq visible-bell 0)
;; hide tool bar
(tool-bar-mode -1)
;; hide menu bar
(menu-bar-mode -1)
;; hide scroll bar
(scroll-bar-mode -1)
;; set line number
(global-linum-mode 1)
;; set cursor type
(setq-default cursor-type 'bar)
;; skip welcome page
(setq inhibit-splash-screen 1)
;; open the init.el quickly
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; bound the key to the function
(global-set-key (kbd "<f1>") 'open-init-file)
;; change the font size
(set-face-attribute 'default nil :height 100)
;; set font
;(set-face-attribute 'default nil :family Monospace)
;; don't back up files
(setq make-backup-files nil)
;; set fullscreen
(setq initial-frame-alist (quote((fullscreen . maximized))))
;; show matching
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; package install
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; theme
;;(load-theme 'monokai t)
;;(load-theme 'doom-dark+ t) 
(load-theme 'cyberpunk t)
;;evil
;;(require 'evil)
;;(evil-mode 1)

;;smart parens
(require 'smartparens-config)
(add-hook 'after-init-hook 'smartparens-global-mode)
;;swiper && counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "94a94c957cf4a3f8db5f12a7b7e8f3e68f686d76ae8ed6b82bd09f6e6430a32c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(package-selected-packages
   '(cyberpunk-2019-theme cyberpunk-theme doom-themes evil 2048-game org smartparens monokai-theme hungry-delete counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
