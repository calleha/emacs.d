;;;* Loading packages
;;;** package.el + melpa
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;;;** org extensions
(use-package org-ref :ensure t
  :init
  (autoload 'org-ref "org-ref" "org-ref" t)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))
(use-package org-modern :ensure t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
; ai assistance disabled - requires openai token or local llm. gptel or org-ai
;(use-package gptel :ensure t)
;(use-package org-ai :ensure t
;  :config
;  (add-hook 'org-mode-hook #'org-ai-mode)
;  (org-ai-global-mode))
(use-package org-present :ensure t
  :init
  (use-package visual-fill-column :ensure t)
  :config
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (add-hook 'org-present-mode-hook (lambda ()
  				     (visual-fill-column-mode 1)
  				     (display-line-numbers-mode -1)
  				     (visual-line-mode 1)))
  (add-hook 'org-present-mode-quit-hook (lambda ()
  				     (visual-fill-column-mode -1)
  				     (display-line-numbers-mode 1)
  				     (visual-line-mode -1))))
;;;** multiple-cursors, which-key, vertico, corfu
(use-package multiple-cursors :ensure t)
(use-package which-key :ensure t
  :config
  (which-key-mode 1))
(use-package vertico :ensure t
  :config
  (vertico-mode 1))
(use-package corfu :ensure t
  :config
  (global-corfu-mode 1))
;;;** pdf-view
(use-package pdf-tools :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode))
;;;** notmuch
;(use-package notmuch
;  :init
;  (autoload 'notmuch "notmuch" "notmuch mail" t))
;;;** all the packages I forgot to add before
(use-package vterm :ensure t)
(use-package sudo-edit :ensure t)
(use-package avy :ensure t)
(use-package god-mode :ensure t)
(use-package mini-modeline :ensure t)
(use-package zygospore :ensure t)
(use-package magit :ensure t)
(use-package iedit :ensure t)
(use-package gnuplot :ensure t)
(use-package dmenu :ensure t)
;;;** emms
(use-package emms :ensure t
  :init
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)))

;;;* Settings
;;;** Functionality settings

; display lines
(global-display-line-numbers-mode)
;; exceptions
(add-hook 'notmuch-search-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-show-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-hello-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))

; wrap lines
(global-visual-line-mode 1)

; repeat mode
(repeat-mode 1)

; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

; display time and battery
(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

; disable ~files
(setq make-backup-files nil)

; enable dead keys
(require 'iso-transl)

; enable recursive minibuffers
(setq enable-recursive-minibuffers 1)

;;;*** mail
(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'ssl
      smtpmail-smtp-service 465)

;;;** Aesthetics

; disable menu bar and other stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; remove fringes
;;(fringe-mode '(0 . 0))

;; colors (redundant; handled by themes)
;;(set-background-color "#292b2e")
;;(set-foreground-color "#b2b2b2")
;;(set-cursor-color "#e3dedd")

;; transparency
;;(set-frame-parameter (selected-frame) 'alpha 95)
;;(add-to-list 'default-frame-alist '(alpha 95))

;; blinking cursor
(blink-cursor-mode 1)

;; modeline
(column-number-mode 1)

;; font
;(add-to-list 'default-frame-alist
;               (cons 'font "Monaco:pixelsize=15"))

;;;* custom-set-variables
(setq custom-file "~/.emacs.d/custom-set-variables.el")
(load custom-file)

;;;* Custom functions

;; move to window when splitting
(defun split-window-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun split-window-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; kill-whole-word
(defun kill-whole-word ()
  (interactive)
  (forward-word)
  (backward-kill-word 1))

;; kill-whole-line
(defun kill-whole-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1))

;; C-w for backward-kill-word, unless there is a region selected
(defadvice kill-region (before unix-werase activate compile)
      "When called interactively with no active region, delete a single word
    backwards instead."
      (interactive
       (if mark-active (list (region-beginning) (region-end))
         (list (save-excursion (backward-word 1) (point)) (point)))))

;; copy line, or copy region if there is a region selected
(defun kill-ring-save-line-or-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-beginning-position 2))))
  (kill-ring-save beg end))

;; kill all buffers
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; kill the current buffer
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; M-: M-" to perform emacs actions with AI
;; interactive org-ai-prompt which returns emacs-lisp expressions
(defun org-ai-elisp ()
  (interactive)
  "Prompts the user for an action within Emacs which is translated to elisp."
  (let ((prompt-string "Perform an action: "))
    (org-ai-prompt
     (read-from-minibuffer prompt-string)
     :sys-prompt "You are an expert at Emacs, and Emacs Lisp, which is used to script and extend Emacs. You can understand natural language requests for actions to take within Emacs, then translate them to Emacs Lisp that carries out those actions.

  Reply only in pure lisp expressions that can then be evaluated with \"eval-expression\". Do not include any comments or explanations. If the answer consists of multiple expressions, wrap them inside a \"progn\" form.")))

;;;* Custom keybindings

(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-x C-1") 'zygospore-toggle-delete-other-windows)

;; calling custom functions
(global-set-key (kbd "C-x C-2") 'split-window-below-and-move)
(global-set-key (kbd "C-x C-3") 'split-window-right-and-move)
(global-set-key (kbd "C-c w") 'kill-whole-word)
(global-set-key (kbd "C-c C-w") 'kill-whole-word)
(global-set-key (kbd "C-c C-l") 'kill-whole-line)
(global-set-key (kbd "M-w") 'kill-ring-save-line-or-region)
(global-set-key (kbd "C-s-x C-s-k") 'kill-all-buffers)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;(global-set-key (kbd "M-\"") 'org-ai-elisp)

;; (org-mode) The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; C-c m to compile
(global-set-key (kbd "C-c m") 'compile)

;; C-c q to toggle visual-line-mode
(global-set-key (kbd "C-c q") 'visual-line-mode)

;; C-c e to toggle electric-pair-mode (normally disabled, binding used by org-ai)
(global-set-key (kbd "C-c e") 'electric-pair-mode)

;; C-c n to toggle display-line-numbers-mode
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)

;; C-c C-b to toggle menu-bar-mode
(global-set-key (kbd "C-c C-b") 'menu-bar-mode)

;; C-c d to toggle pdf-view-midnight-minor-mode
(global-set-key (kbd "C-c d") 'pdf-view-midnight-minor-mode)

;; C-c r to toggle repeat-mode
(global-set-key (kbd "C-c r") 'repeat-mode)

;; M-p and M-n for backward-paragraph and forward-paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; C-<tab> for next-buffer (default: C-x <right>)
(global-set-key (kbd "C-<tab>") 'next-buffer)

;; s-<tab> for previous-buffer (default: C-x <left>)
(global-set-key (kbd "s-<tab>") 'previous-buffer)

;; C-<return> for vterm
(global-set-key (kbd "C-<return>") 'vterm)

;; M-<return> for eshell
(global-set-key (kbd "M-<return>") 'eshell)

;; C-s-d for dmenu
(global-set-key (kbd "C-s-d") 'dmenu)

;; keybindings for multiple-cursors
(global-set-key (kbd "C-. C-.") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-. C-<") 'mc/mark-all-like-this)

;; org-ai keybindings (disabled, C-c e used by electric-pair-mode)
;(global-set-key (kbd "C-c p") 'org-ai-prompt)
;(global-set-key (kbd "C-c e") 'org-ai-explain-code)

;; toggle mini-modeline
(global-set-key (kbd "C-c h") 'mini-modeline-mode)

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-c C-;") 'avy-goto-word-1)

;; s-M opens up notmuch from any buffer
;(global-set-key (kbd "s-M") `notmuch)

;; god-mode keybindings
(global-set-key (kbd "<escape>") #'god-local-mode)

;; emms
(global-set-key (kbd "C-c <right>") 'emms-next)
(global-set-key (kbd "C-c <left>") 'emms-previous)
(global-set-key (kbd "C-c <down>") 'emms-pause)
(global-set-key (kbd "C-c <up>") 'emms-play-directory)

;;;* Macros

;; Open Inbox
;(fset 'inbox
;   [?\C-c ?m ?\C-s ?i ?n ?b ?o ?x return ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b return])
;(global-set-key (kbd "C-c i") 'inbox)

;;;* Outline mode in init.el
(add-hook 'emacs-lisp-mode-hook 
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp ";;;\\*+\\|\\`")
            (make-local-variable 'outline-heading-end-regexp)
            (outline-minor-mode 1)
            ))

;;;* EXWM
(use-package exwm :ensure t
:config
(require 'exwm)
;obsolete (require 'exwm-config)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 4)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
(define-key exwm-mode-map (kbd "C-;") 'other-window)
(define-key exwm-mode-map (kbd "C-<tab>") 'next-buffer)
(define-key exwm-mode-map (kbd "s-<tab>") 'previous-buffer)
(define-key exwm-mode-map (kbd "C-<return>") 'vterm)
(define-key exwm-mode-map (kbd "M-<return>") 'eshell)
(define-key exwm-mode-map (kbd "C-c C-f") 'exwm-layout-toggle-fullscreen)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
	([?\C-g] . [escape])
	([?\C-j] . [return])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)

;; Open ediff control panel in a new window instead of a new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Multiple screens with xrandr
;obsolete? (require 'exwm-randr)
;obsolete (exwm-randr-enable)
(exwm-randr-mode 1)

;; Convenient editing for X windows
(use-package exwm-edit :ensure t
  :init
  (require 'exwm-edit))

;; exwm-modeline
(use-package exwm-modeline :ensure t
  :config
  (exwm-modeline-mode 1))
)
