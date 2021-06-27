(require 'lazy-load)

(lazy-load-global-keys
 '(("M-x" . smex))
 "smex")

(lazy-load-global-keys
 '(("C-s" . swiper))
 "swiper")

(lazy-load-global-keys
 '(("C-x C-f" . counsel-find-file))
 "counsel")

;(lazy-load-global-keys
 ;'(("SPC-r" . quickrun))
 ;"quickrun")

;(lazy-load-global-keys
 ;'(("SPC-c" . evilnc-comment-or-uncomment-lines))
 ;"evil-nerd-commenter")

(provide 'init-lazyload)
