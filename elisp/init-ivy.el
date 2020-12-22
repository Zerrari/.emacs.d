;;; package --- summary
;;; init-ivy

;;; Commentary:

;;; Code:

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-s" . swiper-isearch)
   ("C-x b" . ivy-switch-buffer)))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy ends here
