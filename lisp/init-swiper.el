(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(provide 'init-swiper)
