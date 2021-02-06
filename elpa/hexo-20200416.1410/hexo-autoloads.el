;;; hexo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hexo" "hexo.el" (0 0 0 0))
;;; Generated autoloads from hexo.el

(autoload 'hexo "hexo" "\
Start *Hexo*. 

\(fn &optional CUSTOM-REPO-ROOT-PATH)" t nil)

(autoload 'hexo-new "hexo" "\
Call `hexo new` anywhere as long as in any child directory
 under a Hexo repository.
That's to say, you can use this function to create new post, even though
under theme/default/layout/" t nil)

(autoload 'hexo-touch-files-in-dir-by-time "hexo" "\
`touch' markdown article files according their \"date: \" to
make it easy to sort file according date in Dired or `hexo-mode'." t nil)

(autoload 'hexo-toggle-article-status "hexo" "\
Move current file between _post and _draft;
You can run this function in dired or a hexo article." t nil)

(autoload 'hexo-update-current-article-date "hexo" "\
Update article's date stamp (at the head) by current time.
Please run this function in the article." t nil)

(autoload 'hexo-insert-article-link "hexo" "\
Insert a link to other article in _posts/." t nil)

(autoload 'hexo-insert-file-link "hexo" "\
Insert the link toward the files in source/ ,
exclude _posts/ & _drafts/" t nil)

(autoload 'hexo-follow-post-link "hexo" "\
`find-file' a markdown format link.
  [FIXME] Currently, this function only support a link with
  file-name as suffix. e.g.
  [Link](/2015/08/19/coscup-2015/)
  [Link](/2015/08/19/coscup-2015)
  [Link](/coscup-2015/)
" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hexo" '("hexo-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hexo-autoloads.el ends here
