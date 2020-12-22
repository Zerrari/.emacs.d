;;; package --- summary

;;; init-awesome-tray.el

;;; Commentary:

;;; Code:

(use-package awesome-tray
  :load-path "~/.emacs.d/site-elisp/awesome-tray"
  :hook
  (after-init . awesome-tray-mode)
  :config
  (awesome-tray-module-battery-info))
 


(provide 'init-awesome-tray)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray.el ends here
