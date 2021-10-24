(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mail-host-address "thinkpad")
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "all mail" :query "*" :key "a")))
 '(notmuch-search-oldest-first nil)
 '(org-agenda-custom-commands
   '(("2" "Agenda for two weeks" agenda ""
      ((org-agenda-span 'fortnight)))
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)))
 '(org-agenda-files
   '("~/Documents/school/religion-history/columbus.org" "~/Documents/agenda/uktrip.org" "~/Documents/school/programming/notes/course.org" "~/Documents/agenda/chores.org" "~/Documents/agenda/school.org" "~/Documents/agenda/shoppinglist.org" "~/Documents/agenda/configs.org" "~/Documents/school/ee/ee.org"))
 '(org-export-backends '(ascii beamer html icalendar latex odt))
 '(org-export-headline-levels 5)
 '(org-export-with-toc nil)
 '(package-selected-packages
   '(csharp-mode which-key sudo-edit gnuplot gnuplot-mode pdf-tools dmenu mc-extras multiple-cursors company-ycmd ycmd slime-company slime company org xclip ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x)) (:inherit nil :stipple nil :background "#292b2e" :foreground "#b2b2b2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "#e3dedd"))))
 '(highlight ((t (:background "#444444"))))
 '(message-header-subject ((t (:foreground "color-23" :weight bold))))
 '(message-header-to ((t (:foreground "color-23" :weight bold))))
 '(mode-line ((t (:background "#222226" :foreground "#b2b2b2" :box nil :height 1.0))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#222226" :foreground "#b2b2b2" :box nil :weight light))))
 '(notmuch-message-summary-face ((t nil)))
 '(notmuch-tag-face ((t (:foreground "brightblue" :weight bold))))
 '(region ((t (:background "#444444")))))
 ;not terminal friendly;'(region ((t (:background "#444155")))))

;; enabled functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
