;;; package --- summary
;;; init-awesome-tab

;;; Commentary:

;;; Code:

(use-package awesome-tab
  :load-path "~/.emacs.d/site-elisp/awesome-tab/"
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-label-fixed-length 10)
  (setq awesome-tab-height 100)
  :bind
  (("M-s" . awesome-tab-backward-tab)
   ("M-d" . awesome-tab-forward-tab)
   ("M-a" . awesome-tab-ace-jump)
   ("M-o" . awesome-tab-forward-tab-other-window)))

(provide 'init-awesome-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tab.el ends here
