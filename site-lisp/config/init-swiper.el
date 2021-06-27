(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(setq ivy-re-builders-alist
 '((counsel-rg . ivy--regex-plus)
  (swiper . ivy--regex-plus)
  (swiper-isearch . ivy--regex-plus)
  (t . ivy--regex-ignore-order))) 

(setq counsel-find-file-ignore-regexp
        (concat
         ;; filename begins with #
         "\\(?:\\`[#.]\\)"
         ;; filename ends with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"
         "\\|\\.elc\\'"
         "\\|\\.pyc\\'"
         "\\|\\.meta\\'"
         ))


(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(provide 'init-swiper)
