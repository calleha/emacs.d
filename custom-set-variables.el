(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(Spacemacs-ish))
 '(custom-safe-themes
   '("09df676b50ab963e0475a17381cf6b4759063b70cd8a86e980db80ff8456f8f7" "5ea9e2081d3f0085eb88b91ad2061ad4c71c56e97b4d933c1e923fdc7b9fd342" "67a817ac588945bb249a0979e8f733758291267b6e7b1da0f1f969038c74cf02" "a5ce5375c00fec65089ddf30977049c6bf327ce9a38fac27d43f3b08bdc87549" default))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
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
 '(org-agenda-files '("/home/calleha/Documents/reminders.org"))
 '(org-export-backends '(ascii beamer html icalendar latex odt))
 '(org-export-with-toc nil)
 '(package-selected-packages
   '(corfu vertico org-modern org-ai mini-modeline exwm-modeline zygospore sudo-edit vterm org org-ref pdf-tools dmenu eglot csharp-mode yasnippet exwm-edit exwm gnuplot gnuplot-mode mc-extras multiple-cursors which-key xclip ##)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-message-summary-face ((t nil)))
 '(notmuch-tag-face ((t (:foreground "brightblue" :weight bold)))))
 ;not terminal friendly;'(region ((t (:background "#444155")))))

;; enabled functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
