(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq gc-cons-threshold 64000000
      load-prefer-newer t
      ;; debug-on-error t
      custom-file "~/.emacs.d/custom.el")
(load custom-file t)
(add-hook 'focus-out-hook #'garbage-collect)
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold (* 64 1000 1000))))

(setq frame-title-format "%f - %m")
;; (setq frame-title-format '((:eval (if (buffer-file-name)
;; (abbreviate-file-name (buffer-file-name)) "%b")) (:eval (if (buffer-modified-p) " •")) " - %m"))

(when (eq system-type 'darwin)
  (setq initial-frame-alist '((left . 1.0) (width . 100) (fullscreen . fullheight)))
  (add-to-list 'default-frame-alist '(font . "Monaco-13"))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list default-frame-alist '(undecorated . t))
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	default-directory "~/"
	;; insert-directory-program "gls"
	frame-title-format nil
	inhibit-splash-screen t))


;; Base
(setq-default cursor-type 'bar
	      ;; indent-tabs-mode nil
	      indicate-buffer-boundaries '((bottom . left)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      tramp-backup-directory-alist backup-directory-alist
      version-control t
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying-when-mismatch t)
(setq initial-scratch-message nil
      large-file-warning-threshold 50000000
      echo-keystrokes 0.5
      bidi-inhibit-bpa t
      window-combination-resize t
      ring-bell-function 'ignore)
(setq kill-whole-line t
      ;; tab-always-indent 'complete
      tabify-regexp "^\t* [ \t]+"
      reb-re-syntax 'string
      sentence-end-double-space nil
      backward-delete-char-untabify-method 'hungry
      save-interprogram-paste-before-kill t)
(setq switch-to-buffer-preserve-window-point t
      ;; recenter-positions '(top middle bottom)
      scroll-error-top-bottom t
      scroll-margin 5
      scroll-conservatively 3
      scroll-preserve-screen-position t
      set-mark-command-repeat-pop t)
(setq isearch-lazy-count t
      lazy-count-prefix-format "[%s/%s] "
      lazy-count-suffix-format nil
      ;; search-exit-option t
      search-whitespace-regexp ".*?"
      isearch-lazy-highlight t)
(setq
 ;; focus-follows-mouse t
 ;; mouse-autoselect-window 0.3
 ;; x-stretch-cursor t
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-diff-options "-w")
(require 'dired-x)
(setq dired-recursive-copies 'always
      dired-isearch-filenames t
      dired-dwim-target t
      dired-auto-revert-buffer t
      dired-listing-switches "-lGhv --group-directories-first") ;; -laGh1v
(add-hook 'dired-mode #'dired-omit-mode)
(require 'recentf)
(add-to-list 'recentf-exclude (expand-file-name package-user-dir))
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 50)

(require 'iso-transl)
(require 'whitespace)
(setq whitespace-line-column 90
      whitespace-style '(face tabs trailing lines-tail space-before-tab))

;;(global-superword-mode)
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
;; (add-hook 'text-mode-hook 'abbrev-mode)
;; (setq text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
(add-hook 'ibuffer-hook #'ibuffer-auto-mode)
(add-hook 'c-mode-common-hook (lambda () (c-toggle-auto-state)))

(require 'isearch-dabbrev)

;; (mouse-avoidance-mode 'banish)
;; (global-display-line-numbers-mode)
(column-number-mode)
(tool-bar-mode 0)
;; (display-time-mode)
;; (scroll-bar-mode 0)
(size-indication-mode)

(global-auto-revert-mode)
(save-place-mode)
(recentf-mode)
(winner-mode)
;; (auto-insert-mode)

(delete-selection-mode)
(electric-pair-mode)
(blink-cursor-mode -1)
(show-paren-mode)
;; (remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)


;; Packages
;; (add-to-list 'completion-styles 'flex t)
;; (add-to-list 'completion-styles 'substring t)
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-tooltip-flip-when-above t
      company-tooltip-align-annotations t
      company-dabbrev-code-ignore-case t
      company-search-regexp-function 'company-search-flex-regexp
      ;; company-transformers '(company-sort-by-occurrence)
      company-global-modes '(not org-mode)
      )
(global-company-mode)
(push 'company-ghci company-backends)
;; (add-hook 'haskell-mode-hook (lambda () (set (make-local-variable 'company-backends) (append '((company-capf company-dabbrev-code)) company-backends))))

(require 'selectrum)
(require 'selectrum-prescient)
(require 'prescient)
(selectrum-mode)
;; (add-to-list 'prescient-filter-method 'fuzzy)
(prescient-persist-mode)
(selectrum-prescient-mode)
(setq selectrum-count-style 'current/matches)
(require 'embark)
(require 'consult)
(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)
(require 'marginalia)
(marginalia-mode)
(setq marginalia-annotators '(marginalia-annotators-heavy))
(require 'company-prescient)
(company-prescient-mode)

(setq avy-background t
      avy-all-windows t
      avy-timeout-seconds 0.35)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?r ?u ?i))

(require 'expand-region)

(setq help-window-select t)
(require 'shackle)
(setq shackle-rules '((compilation-mode :select t)
		      (occur-mode :select t)
		      (help-mode :align 'below :size 0.3 :select t)))
(shackle-mode)
(defun isearch-exit-why (&rest _)
  (isearch-exit))
(advice-add 'isearch-occur :before 'isearch-exit-why)

;; (global-hl-line-mode)
(volatile-highlights-mode)
;; (idle-highlight-mode)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook #'highlight-symbol-mode)
(add-hook 'text-mode-hook #'highlight-symbol-mode)

(require 'magit)
(setq magit-diff-refine-hunk t)
;; (setq magit-display-buffer-function
;;       #'magit-display-buffer-same-window-except-diff-v1)
(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(global-diff-hl-mode +1)
;; (diff-hl-margin-mode)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(dolist (hook '(eval-expression-minibuffer-setup-hook
		lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook racket-mode-hook))
  (add-hook hook #'paredit-mode)
  )
;; (setq scheme-program-name "racket")

;; (add-hook 'c-mode-common-hook (lambda () (c-toggle-auto-state)))
;; programming competition
;; (add-hook 'c-mode-common-hook
;;               (lambda ()
;;                 (local-set-key (kbd "C-c C-j") 'compile)
;;                 (local-set-key (kbd "C-c C-k") 'eshell)
;;                 (set (make-local-variable 'compile-command)
;;                      (concat (concat "make -k "
;; buffer-file-name) " build")))))
;; (c-toggle-auto-newline)

(require 'eshell-z)
(require 'eshell-syntax-highlighting)
(eshell-syntax-highlighting-global-mode)

(defun my-inf-sml-mode-hook () "Local defaults for inferior SML mode"
       (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
       (setq      comint-scroll-show-maximum-output t)
       (setq      comint-input-autoexpand nil))
(add-hook 'inferior-sml-mode-hook #'my-inf-sml-mode-hook)
;; (add-hook 'sml-mode-hook (lambda (setq sml-indent-level 2)))

(require 'haskell-mode)
(setq haskell-process-auto-import-loaded-modules t
      haskell-interactive-popup-errors nil
      haskell-process-log t
      haskell-process-suggest-remove-import-lines t
      ;; haskell-hoogle-command "hoogle"
      haskell-doc-prettify-types nil)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'haskell-doc-mode)
(add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)

(setq org-startup-folded nil
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-hide-leading-stars t

      org-log-done 'time
      org-log-reschedule 'time
      org-adapt-indentation nil
      ;; org-startup-indented t
      org-catch-invisible-edits 'show
      org-special-ctrl-a/e t
      org-return-follows-link t

      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t

      org-directory "~/org"
      org-agenda-files '("~/org")
      org-default-notes-file (expand-file-name "notes.org" org-directory))

;; (pdf-tools-install)

(setq TeX-auto-save t
      TeX-parse-self t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

(require 'diminish)

(diminish 'eldoc-mode)
(diminish 'hi-lock-mode)
(diminish 'volatile-highlights-mode)
(diminish 'whitespace-mode)

;; (load-theme 'ample t)
(nimbus-theme)
(load-theme 'tango-2 t)

(custom-set-faces
 '(line-number ((t (:inherit default :foreground "#686a66"))))
 '(line-number-current-line ((t (:inherit default :foreground "#608079"))))
 '(hl-line ((t (:extend t :background "gray13"))))
 '(idle-highlight ((t (:inherit region :background "gray20"))))
 '(highlight-symbol-face ((t (:inherit region :background "gray20"))))
 '(whitespace-trailing ((t :background "grey15")))
 '(show-paren-match ((t :foreground "#fffe0a" :background "gray40")))
 ;; '(selectrum-primary-highlight ((t (:background "#1d9a79" :foreground "#4e11c92"))))
 ;; '(selectrum-secondary-highlight ((t (:background "#ad3632" :foreground "#c23122" :underline t))))
 ;; '(selectrum-current-candidate ((t (:weight bold :background "darkseagreen2"))))
 )
;; (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 130 :background "#1a1a1a")


;; Functions
(defun fill-line ()
  "Join the following line onto the current line."
  (interactive)
  (join-line -1))
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(defun align-to-string (beg end)
  "Align region along character CHAR from BEG to END."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))
(defun indent-buffer (arg)
  "Indent the whole buffer, delete trailing spaces, with C-u also tabify buffer"
  (interactive "p")
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace)
  (if (= arg 4)
      (tabify (point-min) (point-max))))
(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))
(defun restclient ()
  "testing buffer with restclient"
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer)))
  )
(defun narrow-dwim ()
  "Widen if currently narrowed, else narrow to function."
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen))
   (t (narrow-to-defun))))
(defun narrow-quotes ()
  "Widen if currently narrowed, else narrow inside strings."
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen))
   (t (save-excursion
	(when (er--point-inside-string-p)
	  (let ((beg (progn
		       (er--move-point-backward-out-of-string)
		       (forward-char)
		       (point)))
		(end (progn
		       (er--move-point-forward-out-of-string)
		       (backward-char)
		       (point))))
	    (narrow-to-region beg end)))))))
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
	(error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))
(defun edit-dot-emacs ()
  (interactive)
  (find-file "~/.emacs"))
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
			     (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
	(rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))
;; (defun rename-this-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))))
;;   (filename (buffer-file-name)
;;             (unless filename
;;               (error "Buffer '%s' is not visiting a file!" name))
;;             (progn
;;               (when (file-exists-p filename))))
;;   (rename-file filename new-name 1
;;                (set-visited-file-name new-name)
;;                (rename-buffer new-name)
;; 	       (set-buffer-modified-p nil)
;; 	       ))
(defun open-line-and-indent (n)
  "N lines -------|."
  (interactive "*p")
  (beginning-of-line)
  (open-line n)
  (indent-according-to-mode))
(defun open-line-below ()
  "Open a new line below, even if the point is midsentence."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun split-window-below-cycle ()
  (interactive)
  (with-selected-window (split-window-below)
    (call-interactively #'iflipb-next-buffer)))
(defun split-window-right-cycle ()
  (interactive)
  (with-selected-window (split-window-right)
    (call-interactively #'iflipb-next-buffer)))
(defvar nyan-prompt-timer nil)
(defun nyan-at-time (time)
  "Nyan after a given time."
  (interactive "sRun at time: ")
  (when (timerp nyan-prompt-timer)
    (cancel-timer nyan-prompt-timer))
  (setq nyan-prompt-timer
	(run-at-time time nil
		     (lambda () (zone-nyan-preview) (setq nyan-prompt-timer nil))))
  (message (concat "Nyan in " time)))
(defun nyan-at-quater-or-cancel ()
  "Nyan after 15 minutes, or cancel a existing timer."
  (interactive)
  (cond
   ((timerp nyan-prompt-timer)
    (cancel-timer nyan-prompt-timer)
    (setq nyan-prompt-timer nil)
    (message (concat "Canceled Nyan timer")))
   (t
    (nyan-at-time "15 min"))))
(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1))
(defun scheme-load-current-file ()
  (interactive)
  (scheme-load-file (buffer-file-name)))
(defun kill-from-indentation ()
  "kill back to indentation."
  (interactive)
  (back-to-indentation)
  (let ((kill-whole-line nil))
    (kill-line))
  )
(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(defun quick-jump-backward ()
  (interactive)
  (if (and highlight-symbol-mode highlight-symbol)
      (highlight-symbol-prev)
    (if (eq major-mode 'org-mode)
	(org-backward-heading-same-level)))
  )
(defun quick-jump-forward ()
  (interactive)
  (if (and highlight-symbol-mode highlight-symbol)
      (highlight-symbol-next)
    (if (eq major-mode 'org-mode)
	(org-forward-heading-same-level)))
  )


;; Keybindings
(require 'general)
(general-def
  ;; "C-j" C-<return>
  ;; bindable keys
  ;; C-x C-u C-x C-l C-M-/
  ;; C-!     C-$     C-^     C-(     C-=     C-}     C-:     C-|     C-.     C-`
  ;; C-#     C-%     C-&     C-)     C-{     C-;     C-"     C-,     C-?     C-~
  ;; C-= C-, M-[ M-] c-` c-;

  ;; "C-c j" 'avy-goto-char-timer
  ;; "C-c C-;" 'ace-pinyin-dwim
  ;; "s-s l" 'link-hint-open-link
  ;; "C-r" 'kill-region
  ;; "C-w" 'backward-kill-word

  "C-;" 'avy-goto-word-or-subword-1
  "C-c ;" 'avy-goto-char-timer
  "C-c C-;" 'avy-goto-char-timer
  "M-s s" 'avy-goto-line
  "M-c" 'avy-zap-to-char-dwim

  "C-0" #'delete-window
  "C-1" #'delete-other-windows
  "C-2" #'split-window-below-cycle
  "C-3" #'split-window-right-cycle
  "C-4" #'find-file-other-window
  "C-5" #'make-frame-command

  "C-x o" 'ace-window
  "M-o" 'other-window
  "C-x C-b" 'ibuffer-other-window
  "C-x C-'" 'toggle-frame-fullscreen
  "C-<tab>" 'iflipb-next-buffer
  "C-S-<tab>" 'iflipb-previous-buffer

  "M-p" 'quick-jump-backward
  "M-n" 'quick-jump-forward

  "C-c h" 'highlight-symbol
  "C-c M-h" 'highlight-symbol-query-replace

  "C-c C-o" 'occur
  "M-l" 'move-to-window-line-top-bottom

  "C-c C-i" 'indent-buffer
  "C-M-k" 'kill-from-indentation
  "M-k" 'kill-sexp
  "C-d" 'delete-forward-char
  "C-x j" 'join-line
  "M-j" 'fill-line
  "C-c s" 'cycle-spacing
  [remap just-one-space] 'cycle-spacing
  "M-_" 'align-to-string
  "C-<return>" 'open-line-below
  "C-o" 'open-line-and-indent
  "C-c e" 'er/expand-region
  "C-'" 'er/expand-region ;; also C-M-<SPC>
  "M-w" 'easy-kill

  "M-s r" 'query-replace
  "M-s M-r" 'query-replace-regexp

  "C-c M-/" 'hippie-expand

  "C-x n d" 'narrow-dwim
  "C-x n s" 'narrow-quotes
  "M-s M-l" 'downcase-dwim
  "M-s M-c" 'capitalize-dwim
  "M-s M-c" 'upcase-dwim

  "C-c C-e" 'edit-dot-emacs
  ;; "C-c r" 'rename-file
  "C-c n" 'nyan-at-time
  "C-c C-n" 'nyan-at-quater-or-cancel

  "C-S-c C-S-c" 'mc/edit-lines
  "C-S-c C-a" 'mc/edit-beginnings-of-lines
  "C-S-c C-e" 'mc/edit-ends-of-lines
  "C-<" 'mc/mark-previous-like-this
  "C->" 'mc/mark-next-like-this
  "C-c C-<" 'mc/mark-all-like-this
  "C-S-<mouse-1>" 'mc/add-cursor-on-click

  "M-y" 'browse-kill-ring

  "C-c l" 'org-store-link
  "C-c a" 'org-agenda
  "C-c c" 'org-capture
  ;; "C-c b" 'org-switchb
  ;; "s-;" 'org-refile-goto
  ;; "C-c j" 'org-refile-goto-last-stored

  ;; "C-y" 'yank-and-indent
  "C-M-<backspace>" 'backward-kill-sexp

  "C-x m" 'consult-mode-command
  "C-c f" 'consult-focus-lines
  "C-x r x" 'consult-register

  "C-c b" 'consult-bookmark
  ;; "C-x r b" 'consult-bookmark
  "C-x b" 'consult-buffer
  "C-x 4 b" 'consult-buffer-other-window
  "C-x 5 b" 'consult-buffer-other-frame

  [remap goto-line] 'consult-goto-line
  "M-s m" 'consult-mark
  "M-s k" 'consult-global-mark

  "C-c o" 'consult-outline
  "C-c i" 'consult-imenu
  "M-s e" 'consult-error
  "M-s g" 'consult-git-grep
  "M-s f" 'consult-find
  "M-s l" 'consult-line
  "C-c C-s" 'consult-line
  "M-s M-o" 'consult-multi-occur

  "M-y" 'consult-yank-pop

  "C-c r" 'region-history-other
  "C-x g" 'magit-status
  "C-c g" 'magit-dispatch

  "C-c v" 'evil-mode)
(setq help-delete nil)
(when help-delete
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\s-h] [?\C-h])
  (general-def
    "M-h" 'backward-kill-word
    "C-M-h" 'backward-kill-sexp
    "C-s-h" 'mark-defun))
(general-def help-map
  "C-i" 'info-display-manual)
(general-def dired-mode-map
  "C-c C-l" 'dired-up-directory)
(general-def isearch-mode-map
  "<tab>" 'isearch-dabbrev-expand
  "C-c C-o" 'isearch-occur
  ;; "C-o" 'isearch-occur
  ;; "C-n" 'isearch-repeat-forward
  ;; "C-p" 'isearch-repeat-backward
  "C-<return>" 'sanityinc/isearch-exit-other-end
  "M-<" 'isearch-beginning-of-buffer
  "M->" 'isearch-end-of-buffer)
(general-def company-active-map
  ;; [tab] 'company-complete
  "C-d" 'company-complete-selection
  "C-p" 'company-select-previous
  "C-n" 'company-select-next
  "C-s" 'company-filter-candidates
  "C-M-s" 'company-search-candidates)
(general-def company-search-map
  "C-p" 'company-select-previous
  "C-n" 'company-select-next)
(general-def selectrum-minibuffer-map
  "C-c C-o" 'embark-export
  "C-c C-c" 'embark-act)
(general-def diff-hl-mode-map
  "C-c d" 'diff-hl-revert-hunk
  "C-<" 'diff-hl-previous-hunk
  "C->" 'diff-hl-next-hunk)
(general-def paredit-mode-map
  "M-s" nil
  "C-c M-s" 'paredit-splice-sexp
  "M-s M-s" 'paredit-splice-sexp
  "C-c M-f" 'paredit-add-to-previous-list
  "C-c M-b" 'paredit-add-to-next-list
  "C-c M-p" 'paredit-join-with-previous-list
  "C-c M-n" 'paredit-join-with-next-list)
(general-def sml-mode-map
  [remap mark-defun] 'sml-mark-function)
(general-def scheme-mode-map
  "C-c C-l" 'scheme-load-current-file)
(general-def org-mode-map
  "C-S-n" 'org-move-subtree-down
  "C-S-p" 'org-move-subtree-up)
(general-def haskell-mode-map
  "C-c C-a" 'haskell-process-reload
  "C-c C-c" 'haskell-compile
  "C-c h" 'haskell-hoogle
  "C-c C-." 'haskell-navigate-imports)
(general-def 'c-mode-base-map
  "C-c C-j" 'compile
  "C-c C-k" 'eshell)
