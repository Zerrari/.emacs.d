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
(require 'init-org)
(require 'init-ui)
(require 'init-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dired-icon figlet all-the-icons-dired leetcode youdao-dictionary yasnippet which-key vterm use-package spaceline smartparens rainbow-delimiters popup-kill-ring perspeen org-bullets neotree monokai-theme monitor magit keyfreq ivy-posframe hungry-delete flycheck evil-tutor evil-leader dracula-theme doom-themes doom-modeline diminish dashboard cyberpunk-theme cyberpunk-2019-theme counsel company benchmark-init avy airline-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
