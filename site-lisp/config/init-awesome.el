(require 'awesome-tab)
(require 'awesome-tray)

(awesome-tab-mode t)
(awesome-tray-mode 1)

(setq awesome-tab-label-fixed-length 0)

(setq awesome-tab-height 150)

(setq awesome-tray-active-modules nil)

(add-to-list 'awesome-tray-active-modules "buffer-name")

;; (setq awesome-tray-mode-line-active-color "#2fff2f")

(setq awesome-tray-mode-line-colors "#2fff2f")

(provide 'init-awesome)
