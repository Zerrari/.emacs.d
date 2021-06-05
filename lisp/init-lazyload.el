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

(provide 'init-lazyload)
