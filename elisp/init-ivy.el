;;; package --- summary
;;; init-ivy

;;; Commentary:

;;; Code:

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
   (("C-s" . swiper-isearch)
   ("C-x b" . ivy-switch-buffer)
   ("C-c s" . counsel-fzf)))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy ends here
