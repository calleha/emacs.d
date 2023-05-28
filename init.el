;; Emacs config
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

;; org-ref
(autoload 'org-ref "org-ref" "org-ref" t)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;;;** org-ai
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(global-set-key (kbd "C-c p") 'org-ai-prompt)
(global-set-key (kbd "C-c e") 'org-ai-explain-code)

;;;** org-modern
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

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
;;;** zygospore (reversible C-x 1)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
;;;** mini-modeline
(global-set-key (kbd "C-c h") 'mini-modeline-mode)

;;;* Settings
;;;** Functionality settings

; display lines
(global-display-line-numbers-mode)

(add-hook 'notmuch-search-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-show-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'notmuch-hello-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))

; wrap lines
(global-visual-line-mode 1)

;; scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

; auto-revert-mode (auto refresh)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

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

;;;** Aesthetics

; disable menu bar and other stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; remove fringes
;;(fringe-mode '(0 . 0))

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

;;;** Custom keybindings

;; C-c q to toggle visual-line-mode
(global-set-key (kbd "C-c q") 'visual-line-mode)

;; C-c e to toggle electric-pair-mode (disabled, binding used by org-ai)
;;(global-set-key (kbd "C-c e") 'electric-pair-mode)

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

;; M-" for mark-word (translating M-@ to swedish keyboard, disabled)
;;(global-set-key (kbd "M-\"") 'mark-word)

;; C-; for other-window (default: C-x o)
(global-set-key (kbd "C-;") 'other-window)

;; C-<tab> for next-buffer (default: C-x <left>)
(global-set-key (kbd "C-<tab>") 'next-buffer)

;; C-<return> for vterm
(global-set-key (kbd "C-<return>") 'vterm)

;;;** Custom functions

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

;; M-: M-" to perform emacs actions with AI
;; Interactive org-ai-prompt which returns emacs-lisp expressions (M-")
(defun org-ai-elisp ()
      (interactive)
      "Prompts the user for an action within Emacs which is translated to elisp."
      (let ((prompt-string "Perform an action: "))
         (org-ai-prompt
          (read-from-minibuffer prompt-string)
          :sys-prompt "You are an expert at Emacs, and Emacs Lisp, which is used to script and extend Emacs. You can understand natural language requests for actions to take within Emacs, then translate them to Emacs Lisp that carries out those actions.

  Reply only in pure lisp expressions that can then be evaluated with \"eval-expression\". Do not include any comments or explanations. If the answer consists of multiple expressions, wrap them inside a \"progn\" form.")))
(global-set-key (kbd "M-\"") 'org-ai-elisp)

;;;* Macros

;; Open Inbox
(fset 'inbox
   [?\C-c ?m ?\C-s ?i ?n ?b ?o ?x return ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b return])
(global-set-key (kbd "C-c i") 'inbox)

;;;* Outline mode in init.el
(add-hook 'emacs-lisp-mode-hook 
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp ";;;\\*+\\|\\`")
            (make-local-variable 'outline-heading-end-regexp)
            (outline-minor-mode 1)
            ))

;;;* EXWM
(require 'exwm)
(require 'exwm-config)

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
(define-key exwm-mode-map (kbd "C-<return>") 'vterm)
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

;; Convenient editing for X windows
(require 'exwm-edit)

;; Multiple screens with xrandr
;;(require 'exwm-randr)
;;(exwm-randr-enable)
