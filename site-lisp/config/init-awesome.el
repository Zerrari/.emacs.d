(require 'awesome-tab)
(require 'awesome-tray)

(awesome-tab-mode t)
(awesome-tray-mode 1)

(setq awesome-tab-label-fixed-length 0)

(setq awesome-tab-height 150)

(setq awesome-tray-active-modules nil)

(add-to-list 'awesome-tray-active-modules "buffer-name" "date")

(provide 'init-awesome)
