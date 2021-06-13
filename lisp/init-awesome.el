(require 'awesome-tab)
(require 'awesome-tray)

(awesome-tab-mode t)
(awesome-tray-mode 1)

(setq awesome-tray-active-modules nil)

(add-to-list 'awesome-tray-active-modules "buffer-name" "date")

(provide 'init-awesome)
