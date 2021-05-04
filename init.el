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
(require 'init-ui)
(require 'init-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quickrun evil-nerd-commenter evil-leader which-key use-package smex smartparens rainbow-delimiters pythonic project page-break-lines multiple-cursors jsonrpc indent-guide iedit hungry-delete flymake flycheck exec-path-from-shell evil diminish counsel company all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
