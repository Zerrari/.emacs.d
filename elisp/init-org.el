(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/recording/May.org" "day 8")
             "* TODO  %?")
	("i" "Idea" entry (file+datetree "~/Documents/recording/idea.org")
         "*  %?\nEntered on %U")))

(provide 'init-org)
