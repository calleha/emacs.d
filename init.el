; Emacs config

;;;* Extensions
;;;** package

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;** melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;;;** org-mode

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


;; add org to elpa
;(require 'package)
;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; LaTeX export
;;(require 'ox-latex)
;;(unless (boundp 'org-latex-classes)
;;  (setq org-latex-classes nil))
;;(add-to-list 'org-latex-classes
;;             '("article"
;;               "\\documentclass{article}"
;;               ("\\section{%s}" . "\\section*{%s}")))

;; org-ref
(autoload 'org-ref "org-ref" "org-ref" t)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;;;** enable xclip
(add-to-list 'load-path "~/.emacs.d/elpa/xclip-1.4/")
(require 'xclip)
(xclip-mode 1)

;;;** mail
(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'ssl
      smtpmail-smtp-service 465)

;;;** notmuch
(autoload 'notmuch "notmuch" "notmuch mail" t)
;; C-c m opens up notmuch from any buffer
(global-set-key (kbd "C-c m") `notmuch)

;;;** company
;(add-hook 'after-init-hook 'global-company-mode)
;; rebind M-n and M-p to C-n and C-p for company
;(define-key company-active-map (kbd "\C-n") 'company-select-next)
;(define-key company-active-map (kbd "\C-p") 'company-select-previous)

;;;** slime
;(setq inferior-lisp-program "/usr/bin/sbcl")
;(slime-setup '(slime-fancy slime-company))

;;;** ycmd
;(require 'ycmd)
;(add-hook 'after-init-hook 'global-ycmd-mode)

;; company-ycmd for autocompletion
;(require 'company-ycmd)
;(company-ycmd-setup)
;;;** multiple-cursors
(autoload 'multiple-cursors "multiple-cursors" "edit with multiple cursors" t)

;; keybindings
(global-set-key (kbd "C-. C-.") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-. C-<") 'mc/mark-all-like-this)

;; mc-extras
;; haven't added keybindings yet

;;;** dmenu
(global-set-key (kbd "C-s-d") 'dmenu)
;;;** pdf-tools
(pdf-tools-install)
;;;** pdf-view
(require 'pdf-view)
(setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                 ,(face-attribute 'default :background)))
;;;** which-key
(which-key-mode 1)
;;;* Settings
;;;** Functionality settings

; display lines
(global-display-line-numbers-mode)

(add-hook 'notmuch-search-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-show-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-hello-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

; auto-revert-mode (auto refresh)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

; display time
(setq display-time-24hr-format t)
(display-time-mode 1)

; disable ~files
(setq make-backup-files nil)

; enable dead keys
(require 'iso-transl)

;;;** Aesthetics

; disable menu bar and other stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; remove fringes
(fringe-mode '(0 . 0))

;; colors
(set-background-color "#292b2e")
(set-foreground-color "#b2b2b2")
(set-cursor-color "#e3dedd")

;; transparency
;;(set-frame-parameter (selected-frame) 'alpha 95)
;;(add-to-list 'default-frame-alist '(alpha 95))

;; blinking cursor
(blink-cursor-mode 1)

;; modeline
(column-number-mode 1)
(set-face-attribute 'mode-line           nil :background "#222226" :foreground "#b2b2b2" :box nil)  
;;#1A1A1B

;; font
;(add-to-list 'default-frame-alist
;               (cons 'font "Monaco:pixelsize=15"))

;;;* custom-set-variables
(setq custom-file "~/.emacs.d/custom-set-variables.el")
(load custom-file)

;;;* Bindings
;;;** Currently disabled

;; M-f for forward-to-word instead of forward-word (misc.el)
;;(require 'misc)
;;(global-set-key (kbd "M-f") 'forward-to-word)

;;;** Smaller keybindings

;; C-c q to toggle visual-line-mode
(global-set-key (kbd "C-c q") 'visual-line-mode)

;; C-c e to toggle electric-pair-mode
(global-set-key (kbd "C-c e") 'electric-pair-mode)

;; C-c d to toggle pdf-view-midnight-minor-mode
(global-set-key (kbd "C-c d") 'pdf-view-midnight-minor-mode)

;; M-p and M-n for backward-paragraph and forward-paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; M-" for mark-word (translating M-@ to swedish keyboard)
(global-set-key (kbd "M-\"") 'mark-word)

;; C-; for other-window (default: C-x o)
(global-set-key (kbd "C-;") 'other-window)

;; C-: for previous-buffer (default: C-x <left>)
(global-set-key (kbd "C-:") 'previous-buffer)

;;;** Larger keybindings (custom functions)

;; Move to window when splitting
(defun split-window-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun split-window-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-window-below-and-move)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move)

;; kill-whole-word with C-c w
(defun kill-whole-word ()
  (interactive)
  (forward-word)
  (backward-kill-word 1))
(global-set-key (kbd "C-c w") 'kill-whole-word)

;; C-w for backward-kill-word, unless there is a region selected
(defadvice kill-region (before unix-werase activate compile)
      "When called interactively with no active region, delete a single word
    backwards instead."
      (interactive
       (if mark-active (list (region-beginning) (region-end))
         (list (save-excursion (backward-word 1) (point)) (point)))))

;; M-w to copy line, unless there is a region selected
(defun kill-ring-save-line-or-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-beginning-position 2))))
  (kill-ring-save beg end))
(global-set-key (kbd "M-w") 'kill-ring-save-line-or-region)

;; C-s-x C-s-k to kill all buffers
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-s-x C-s-k") 'kill-all-buffers)

;; C-x k to kill the current buffer
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;;* Macros

;; Open Inbox
(fset 'inbox
   [?\C-c ?m ?\C-s ?i ?n ?b ?o ?x return ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b return])
(global-set-key (kbd "C-c i") 'inbox)
;; alt Open Inbox (does not require 'C-c m' to open notmuch)
;;(fset 'inbox2
;;   "\C-[xnotmuch\C-m\C-[[B\C-[[B\C-[[B\C-[[B\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-[[C\C-m")
;;(global-set-key (kbd "C-c i") 'inbox2)

;; Search Mail (not working)
;;(fset 'notmuchsearch
;;   "\C-[xnotmuch-search")
;;(define-key notmuch-search-mode-map (kbd "/") 'notmuchsearch)
;;(define-key notmuch-show-mode-map (kbd "/") 'notmuchsearch)
;;(define-key notmuch-hello-mode-map (kbd "/") 'notmuchsearch)

;;;* Outline mode in .emacs
(add-hook 'emacs-lisp-mode-hook 
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp ";;;\\*+\\|\\`")
            (make-local-variable 'outline-heading-end-regexp)
            (outline-minor-mode 1)
            ))

