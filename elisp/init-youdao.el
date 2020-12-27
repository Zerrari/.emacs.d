;;; package --- summary

;;; init-youdao.el

;;; Commentary:

;;; Code:

(use-package youdao-dictionary
  :bind
  (("C-c y" . youdao-dictionary-search-at-point)
   ("C-c h" . youdao-dictionary-search-from-input)))

(provide 'init-youdao)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-youdao.el ends here
