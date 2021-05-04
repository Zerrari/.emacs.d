;;; package --- summary
;;; init-ui

;;; Commentary:

;;; Code:

;; turn off the bell
;; (setq visible-bell 0)

(setq ring-bell-function 'ignore blink-cursor-mode nil)

(set-face-attribute 'default nil :font "monaco 24")

;; hide tool bar
(tool-bar-mode -1)

;; hide menu bar
(menu-bar-mode -1)

;; shut down auto-save mode
(auto-save-mode -1)

;; hide scroll bar
(scroll-bar-mode -1)

;; set line number
(global-linum-mode 1)

;; set cursor type
(setq-default cursor-type 'bar)

;; skip welcome page
(setq inhibit-splash-screen 1)

;; set fullscreen
(setq initial-frame-alist (quote((fullscreen . maximized))))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
