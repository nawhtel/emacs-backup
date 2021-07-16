;;; -*- lexical-binding: t; -*-
(setq
 gc-cons-threshold 2000000
 package-native-compile t
 load-prefer-newer t
 ;; debug-on-error t
 comp-async-report-warnings-errors nil
 custom-file "~/.emacs.d/custom.el")
(add-to-list 'load-path "~/.emacs.d/luna")
(add-to-list 'load-path "~/.emacs.d/company-mlton" t)
(add-to-list 'load-path "~/.emacs.d/mlton-el" t)
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

(dolist (file '( package expand-region iso-transl company corfu consult bm
                 orderless embark wgrep project recentf-ext dired-x org
                 org-agenda org-tempo visual-regexp-steroids multiple-cursors
                 avy avy-zap affe whole-line-or-region shackle vterm dumb-jump
                 luna-key citre citre-config undo-propose volatile-highlights
                 ace-window magit comint browse-kill-ring exec-path-from-shell))
  (require file))
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Variables
(when (eq system-type 'gnu/linux)
  (setq xterm-query-timeout nil) ;; xterm init is used for st, causing delay
  (add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-10"))
  (menu-bar-mode 0)
  (setq frame-title-format ;; "%f - %m"
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))
          (:eval (if (buffer-modified-p) " •")) " - %m"))
  (setq citre-readtags-program "readtags"
        dumb-jump-prefer-searcher 'ag)
  )
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq
   mac-command-modifier 'meta
   mac-option-modifier 'super
   insert-directory-program "gls"
   frame-title-format nil)
  (setq default-frame-alist  ;; (undecorated . t)
        '((ns-appearance . dark) (font . "Monaco-13") (ns-transparent-titlebar . t)
          (fullscreen . fullheight) (width . 100) (left . 1.0)))
  (setq citre-readtags-program "ureadtags"
        dumb-jump-prefer-searcher 'rg)
  )

(setq-default
 ;; tab-width 4
 ;; truncate-lines t ;; if set don't forget track-eol and line-move-visual
 fill-column 80
 indent-tabs-mode nil
 cursor-type '(bar . 2)
 indicate-buffer-boundaries '((bottom . left))

 bm-buffer-persistence t
 ;; abbrev-mode t
 magit-diff-refine-hunk t)

(setq
 make-backup-files nil
 create-lockfiles nil

 echo-keystrokes 0.5
 ring-bell-function 'ignore
 initial-scratch-message nil
 large-file-warning-threshold 80000000

 resize-mini-windows t
 window-combination-resize t
 minibuffer-follows-selected-frame nil

 bidi-inhibit-bpa t
 auto-window-vscroll nil
 x-underline-at-descent-line t
 ;; truncate-partial-width-windows 40
 bidi-paragraph-direction 'left-to-right

 kill-whole-line t
 kill-ring-max 100
 mark-ring-max 100
 reb-re-syntax 'string
 global-mark-ring-max 100
 tabify-regexp "^\t* [ \t]+"
 sentence-end-double-space nil

 ;; track-eol t
 ;; line-move-visual nil
 set-mark-command-repeat-pop t
 comment-auto-fill-only-comments t
 tab-always-indent 'complete ;; or nil
 save-interprogram-paste-before-kill t
 backward-delete-char-untabify-method 'hungry

 recenter-positions '(0.2 middle bottom)
 scroll-margin 3
 scroll-conservatively 100
 scroll-error-top-bottom t
 scroll-preserve-screen-position 'in-place ;; or t

 completion-styles '(basic partial-completion orderless) ;flex ; emacs22
 completion-styles '(orderless)
 completion-category-defaults nil
 completion-category-overrides '((file (styles . (partial-completion))))
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-cycle-threshold 3
 completion-ignore-case t

 search-ring-max 50
 regexp-search-ring-max 50
 search-whitespace-regexp ".*?"
 ;; search-exit-option 'edit
 isearch-lazy-count t
 ;; isearch-wrap-pause 'no-ding
 isearch-repeat-on-direction-change t
 isearch-lazy-highlight t
 lazy-count-prefix-format "[%s/%s] "
 lazy-count-suffix-format nil
 ;; lazy-highlight-cleanup nil

 recentf-filename-handlers '(abbreviate-file-name)
 recentf-exclude
 (list (expand-file-name package-user-dir)
       (file-name-directory (directory-file-name invocation-directory)))
 recentf-max-menu-items 15
 recentf-max-saved-items 150
 savehist-additional-variables
 '(kill-ring file-name-history mark-ring search-ring regexp-search-ring)
 uniquify-buffer-name-style 'post-forward

 whitespace-line-column 90 ; 100
 whitespace-style '(face tabs trailing lines-tail space-before-tab)
 ;; electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
 ;; electric-pair-open-newline-between-pairs nil
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t
 show-paren-highlight-openparen nil

 comint-move-point-for-output t
 comint-scroll-to-bottom-on-input t
 comint-scroll-to-bottom-on-output t
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-error-screen-columns nil
 compilation-search-path '(nil "src")
 compilation-context-lines 2
 compilation-scroll-output 'first-error
 ;; next-error-highlight t
 ;; next-error-highlight-no-select t

 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-diff-options "-w"
 eval-expression-print-length nil
 eval-expression-print-level nil
 term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
 eshell-where-to-jump 'begin
 eshell-review-quick-commands nil
 eshell-smart-space-goes-to-end t
 eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))
 eshell-visual-options '(("git" "--help" "--paginate"))
 doc-view-continuous t

 dired-recursive-copies 'always
 dired-recursive-deletes 'top
 dired-listing-switches "-alGFhv --group-directories-first" ;; -G
 dired-isearch-filenames t
 dired-dwim-target t
 dired-clean-confirm-killing-deleted-buffers nil
 dired-auto-revert-buffer t
 browse-url-handlers '(("\\`file:" . browse-url-default-browser))

 avy-background t
 avy-enter-times-out nil
 avy-timeout-seconds 0.25
 avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?r ?u ?i)
 ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
 er/try-expand-list (delete 'er/mark-word er/try-expand-list)
 mc/always-run-for-all t
 mc/always-repeat-command t
 block-nav-move-skip-shallower t
 block-nav-center-after-scroll t

 symbol-overlay-displayed-window t
 symbol-overlay-idle-time 0.5
 goggles-pulse nil
 bm-highlight-style 'bm-highlight-only-fringe

 corfu-cycle t
 corfu-auto t
 corfu-auto-prefix 3
 corfu-auto-delay 0
 corfu-quit-no-match t

 yas-triggers-in-field t
 ;; yas-wrap-around-region t
 affe-regexp-function #'orderless-pattern-compiler
 affe-highlight-function #'orderless--highlight
 xref-show-definitions-function #'xref-show-definitions-completing-read
 ztree-dir-move-focus t

 shackle-rules '((compilation-mode :select nil)
                 (occur-mode :select t)
                 (Man-mode :select t)
                 (help-mode :align 'below :size 0.35 :select t))
 eyebrowse-new-workspace t
 eyebrowse-wrap-around t
 magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
 moody-mode-line-height 22
 pdf-view-midnight-colors '("#c3c4c6" . "#001111")
 ;; nyan-animate-nyancat t
 ;; ;; nyan-wavy-trail t
 ;; nyan-bar-length 23

 company-idle-delay 0.25
 company-minimum-prefix-length 3
 company-require-match 'never
 company-selection-wrap-around t
 company-tooltip-align-annotations t
 company-dabbrev-ignore-case t
 company-dabbrev-code-ignore-case t
 company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance)
 company-lighter-base ""
 company-search-regexp-function 'company-search-flex-regexp
 ;; company-global-modes '(not org-mode)

 scheme-program-name "racket"
 haskell-process-auto-import-loaded-modules t
 haskell-interactive-popup-errors nil
 haskell-process-log t
 haskell-process-suggest-remove-import-lines t
 haskell-doc-prettify-types nil
 dante-tap-type-time 0.1
 sml-indent-level 2
 sml-indent-args 2
 sml-electric-pipe-mode nil
 company-mlton-modes '(sml-mode inferior-sml-mode)
 ;; company-mlton-basis-file "~/.emacs.d/company-mlton/sml-basis-lib.basis"

 deft-extensions '("adoc" "markdown" "md" "txt" "org")
 deft-default-extension "org"
 deft-recursive t
 deft-directory "~/notes"

 org-startup-with-inline-images t
 org-log-done 'time
 org-hide-leading-stars t
 ;; org-log-reschedule 'time
 org-adapt-indentation nil
 org-special-ctrl-a/e t
 org-return-follows-link t
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-src-preserve-indentation t
 org-directory "~/org"
 org-confirm-babel-evaluate nil
 org-agenda-files '("~/org")
 org-default-notes-file (expand-file-name "notes.org" org-directory)
 ;; org-export-with-smart-quotes t
 ;; org-export-backends '(beamer html latex md)

 org-src-block-faces  '(("sml" (:inherit default)))
 org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))
 org-refile-use-outline-path t
 org-outline-path-complete-in-steps nil)

(load custom-file t)

(dolist (cmd '(consult-ripgrep consult-buffer affe-grep affe-find sanityinc/affe-grep-at-point))
  (add-to-list 'consult-config `(,cmd :preview-key ,(kbd "M-."))))

(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun   'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'compilation-error-regexp-alist 'mlton)
(add-to-list 'compilation-error-regexp-alist-alist
             '(mlton
               "^[[:space:]]*\\(\\(?:\\(Error\\)\\|\\(Warning\\)\\|\\(\\(?:\\(?:defn\\|spec\\) at\\)\\|\\(?:escape \\(?:from\\|to\\)\\)\\|\\(?:scoped at\\)\\)\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\.\\([0-9]+\\)\\)?\\.?\\)$"
               5 (6 . 8) (7 . 9) (3 . 4) 1))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t) (C . t) (asymptote . t) (emacs-lisp . t)
   (haskell . t) (latex . t) (makefile . t) (ocaml . t)
   (perl . t) (python . t) (shell . t) (sql . t) (stan . t)))

(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(advice-add #'isearch-occur :before (lambda (&rest _) (isearch-exit)))

;;; Hooks
(add-hook 'focus-out-hook #'garbage-collect)
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 2000000)))

(add-hook 'prog-mode-hook #'superword-mode)
(dolist (hook '(haskell-mode-hook c++-mode-hook elm-mode-hook))
  (add-hook hook #'subword-mode))
(add-hook 'text-mode-hook #'visual-line-mode)

(dolist (hook '(prog-mode-hook conf-mode-hook))
  ;; (add-hook hook #'company-mode)
  (add-hook hook #'whitespace-mode)
  (add-hook hook #'rainbow-delimiters-mode)
  (add-hook hook #'symbol-overlay-mode))
(dolist (hook '(eval-expression-minibuffer-setup-hook
                lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook
                racket-mode-hook racket-repl-mode-hook))
  (add-hook hook #'paredit-everywhere-mode))

(add-hook 'after-init-hook #'bm-repository-load)
(add-hook 'kill-buffer-hook #'bm-buffer-save)
(add-hook 'find-file-hooks #'bm-buffer-restore)

(add-hook 'after-save-hook #'gen-tags-after-save-hook)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(dolist (hook '(shell-mode-hook term-mode-hook vterm-mode-hook))
  (add-hook hook #'compilation-shell-minor-mode))

(add-hook 'ibuffer-hook #'ibuffer-auto-mode)
(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-hook 'org-mode-hook #'org-autolist-mode)

;; (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)
;; (add-hook 'c-mode-common-hook 'c-prog-competition-build)
;; (add-hook 'python-mode-hook 'anaconda-mode)

(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'haskell-doc-mode)
(add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'haskell-mode-hook #'dante-mode)
(add-hook 'dante-mode-hook (lambda () (set (make-local-variable 'company-backends) (append '((dante-company company-dabbrev-code)) (remove 'dante-company company-backends)))))
;; (add-hook 'haskell-mode-hook (lambda () (set (make-local-variable 'company-backends) (append '((company-capf company-dabbrev-code)) company-backends))))
;; (push 'company-ghci company-backends)

(add-hook 'inferior-sml-mode-hook #'my-inf-sml-mode-hook)
(add-hook 'sml-mode-hook #'my-sml-mode-hook)
(add-hook 'sml-mode-hook
          (lambda () (add-hook 'post-self-insert-hook
                               'more-normal-sml-indentation-post-self-insert-function nil t)))
;; (add-hook 'sml-mode-hook #'def-use-mode)
;; (add-hook 'def-use-mode-hook (lambda () (symbol-overlay-mode -1)))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)

(skewer-setup)

;;; Modes
(recentf-mode)
(winner-mode)
(savehist-mode)
(save-place-mode)
(column-number-mode)
(global-so-long-mode)

(repeat-mode)
(show-paren-mode)
(electric-pair-mode)
(global-hl-line-mode)
(delete-selection-mode)
;; (whole-line-or-region-global-mode)

(shackle-mode)
(eyebrowse-mode)
(yas-global-mode)
(pdf-tools-install)
(vimish-fold-global-mode)
;; (goggles-mode)
(volatile-highlights-mode)

(vertico-mode)
(marginalia-mode)
(corfu-global-mode)

(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(5 . nil)) ;; nil
;; (set-fringe-style '(nil . 0))

(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(minions-mode)
;; (nyan-mode)

;; (load-theme 'ample t t)
(load-theme 'nimbus t t)
(enable-theme 'nimbus)
(when (eq system-type 'gnu/linux)
  (load-theme 'tango-2 t)
  (enable-theme 'tango-2))

;;; Faces
(custom-set-faces
 '(default ((t (:background "gray13" :foreground "#cecec4")))) ;; #cdcdb3
 '(highlight ((t (:background "#303030"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf"))))
 '(region ((t (:background "#313131" :distant-foreground nil))))
 '(modeline ((t (:background "#2e3436" :foreground "#eeeeec"))))
 '(modeline-inactive ((t (:background "#111111" :foreground "#cccddd"))))
 '(fringe ((t (:background nil))))
 '(shadow ((t (:foreground "#858585" :background nil))))
 '(hl-line ((t (:extend t :background "#1f1f1f")))) ;; #23232c ;; #gray13 #202020
 ;; '(hl-line ((t (:extend t :background "#34363e"
 ;;                        ;; :box (:line-width -2 :color "#3f414a" :style nil)))))
 '(whitespace-trailing ((t :background "gray15"))) ; :underline t
 '(trailing-whitespace ((t :background "gray16" :underline nil)))
 '(show-paren-match ((t (:foreground nil :inverse-video t))))
 '(show-paren-mismatch ((t (:italic t :inherit nil))))
 '(line-number ((t (:inherit default :foreground "#686a66"))))
 '(line-number-current-line ((t (:inherit default :foreground "#608079"))))

 '(symbol-overlay-default-face ((t (:underline t))))
 '(vertico-current ((t (:extent t :background "#333333"))))
 '(eyebrowse-mode-line-inactive ((t (:foreground "#608079"))))
 '(easy-kill-origin ((t (:inherit secondary-selection :inverse t))))
 '(vhl/default-face ((t (:inherit secondary-selection :extend nil))))

 '(rainbow-delimiters-depth-1-face ((t (:foreground "grey60")))) ;grey55
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#93a8c6"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#b0b1a3"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#97b098"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#aebed8"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#b0b0b3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#90a890"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#a2b6da"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9cb6ad"))))
 '(def-use-def-face ((t (:inherit symbol-overlay-face-1))))
 '(def-use-use-face ((t (:inherit symbol-overlay-face-2))))
 '(def-use-mark-face ((t (:inherit symbol-overlay-face-3))))
 '(def-use-view-face ((t (:inherit symbol-overlay-face-4))))
 '(def-use-unused-def-face ((t (:inherit symbol-overlay-face-5)))))

;;; Editing functions
(defun align-to-string (beg end)
  "Align region along character CHAR from BEG to END."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))
(defun fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(defun indent-buffer (&optional arg)
  "Indent the whole buffer, delete trailing spaces, with C-u also tabify buffer"
  (interactive "P")
  (indent-region (point-min) (point-max) nil)
  (delete-trailing-whitespace)
  (if arg
      (tabify (point-min) (point-max))))
(defun kill-same-indent (&optional arg)
  "kill whole lines downward until a line with the same indentation level or higher."
  (interactive "p")
  (back-to-indentation)
  (setq first-col (current-column))
  (kill-whole-line)
  (while (progn
           (back-to-indentation)
           (> (current-column) first-col))
    (beginning-of-line)
    (append-next-kill)
    (kill-line))
  (beginning-of-line)
  (if (and arg (> arg 1))
      (kill-same-indent (- arg 1))))
(defun kill-region-or-sexp-whole-region (num)
  "Kill region or last sexp using whole-line-region"
  (interactive "p")
  (if (whole-line-or-region-use-region-p)
      (funcall 'kill-region (region-beginning) (region-end) 'region)
    ;; (defun delete-or-kill-sexp (beg end)
    ;;   (if (> (region-bytes beg end) 10)
    ;;       (kill-region beg end nil)
    ;;     (delete-region beg end nil)))
    (let ((f (if (> gc-cons-threshold 2000000) 'delete-region 'kill-region)))
      (whole-line-or-region-filter-with-yank-handler
       (funcall f
                (save-excursion (backward-sexp num) (point))
                (point)
                )))))
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))
(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))
(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))
(defun open-line-above (n)
  "N lines -------|."
  (interactive "*p")
  (beginning-of-line)
  (open-line n)
  (indent-according-to-mode))
(defun open-line-below (n)
  "Open a new line below, even if the point is midsentence."
  (interactive "*p")
  (end-of-line)
  ;; (if (= n 1)
  ;; (indent-new-comment-line)
  (open-line n)
  (forward-line 1)
  ;; )
  (indent-according-to-mode))
(defun comment-line-extra (n)
  "comment line with new comment when line is empty."
  (interactive "p")
  (if (not (thing-at-point 'line))
      (call-interactively 'open-line))
  (if (or (use-region-p) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
      (call-interactively 'comment-dwim)
    (comment-line n)))
(defun kill-back-to-indentation ()
  "Kill back to indentation."
  (interactive)
  (back-to-indentation)
  (let ((kill-whole-line nil))
    (kill-line)))
(defun yank-new-line ()
  "Yank on a new line."
  (interactive)
  (if (not (thing-at-point 'line))
      (call-interactively 'open-line-below))
  (yank)
  (call-interactively 'open-line-below))
(defun temp-subword-backward-delete-word (arg)
  "Backawrd kill subword"
  (interactive "p")
  (let ((superword-mode nil))
    (delete-region
     (point)
     (progn
       (backward-word arg) (point)))))
(defun temp-subword-backward-word (arg)
  "Backward one subword."
  (interactive "p")
  (let ((superword-mode nil))
    (backward-word arg)))
(defun temp-subword-forward-word (arg)
  "Backward one subword."
  (interactive "p")
  (let ((superword-mode nil))
    (forward-word arg)))
(defun end-of-delim ()
  (interactive)
  (ignore-error user-error (call-interactively 'backward-up-list))
  (paredit-forward))
(defun sk/other-pdf-next ()
  "Turns the next page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (pdf-view-next-page)
  (other-window 1))
(defun sk/other-pdf-previous ()
  "Turns the previous page in adjoining PDF file"
  (interactive)
  (other-window 1)
  (pdf-view-previous-page)
  (other-window 1))
(defun sk/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 3)))
(defun quick-jump-backward ()
  (interactive)
  (cond ((and symbol-overlay-mode symbol-overlay-temp-symbol)
         (symbol-overlay-jump-prev))
        ((bm-bookmark-at (point))
         (bm-previous))
        (t
         (forward-line (- (window-half-height))))))
(defun quick-jump-forward ()
  (interactive)
  (cond ((and symbol-overlay-mode symbol-overlay-temp-symbol)
         (symbol-overlay-jump-next))
        ((bm-bookmark-at (point))
         (bm-next))
        (t
         (forward-line (window-half-height)))))
(defun sanityinc/affe-grep-at-point (&optional dir initial)
  (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                  (symbol-name s))))
  (affe-grep dir initial))
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
(defun vimish-fold-toggle-dwim ()
  "Toggle fold at point if it exists, else call vimish-fold-avy."
  (interactive)
  (if (-any? #'vimish-fold--vimish-overlay-p (overlays-at (point)))
      (call-interactively 'vimish-fold-toggle)
    (progn
      (if (not (region-active-p))
          (call-interactively 'vimish-fold-avy)
        (call-interactively 'vimish-fold)))))

;; Utils functions
(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda (arg)
     (interactive "P")
     ,@commands))
(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.
FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-current-buffer)))
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

(defun makef-compile ()
  (interactive)
  (let ((default-directory (locate-dominating-file "." "Makefile")))
    (compile "make")))
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  :lighter " [S]"
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
(defun region-bytes (start end)
  "Return the number of bytes used by the region."
  (interactive "r")
  (- (bufferpos-to-filepos end 'exact)
     (bufferpos-to-filepos start 'exact)))
(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))
(defun restclient ()
  "testing buffer with restclient"
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))
(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1))
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  ;; (other-window)
  (follow-mode))
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode))

;; Misc functions
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; if we got this far just use the default so we don't miss
         ;; any upstream changes
         (keyboard-quit))))

(defun gen-tags ()
  (interactive)
  (let ((tagf
         (if (citre-tags-file-path)
             (concat " -f " (citre-tags-file-path))
           nil
           )
         ))
    (shell-command (concat "uctags --kinds-all='*' --fields='*' --extras='*' -R" tagf))
    ;; (shell-command (concat "uctags --languages=" langs " --kinds-all='*' --fields='*' --extras='*' -R"))
    ;; ctags --languages=c,c++,... --kinds-all='*' --fields=+KESflnt --extras=+fr -R
    ))
(defun ctags-entries (&optional tag)
  "Return a list of all tag entries, or just the entries for TAG."
  (let ((default-directory
          (or (locate-dominating-file
               default-directory ctags-dominating-file)
              default-directory)))
    (with-temp-buffer
      (call-process ctags-program-name nil t nil "-Rx")
      (setf (point) (point-min))
      (cl-loop with regexp =
               (if tag
                   (format "^%s" (regexp-opt (list tag) 'symbols))
                 "^\\S-+")
               while (re-search-forward regexp nil t)
               collect (ctags--extract-tag-info)))))
(defun gen-tags-after-save-hook ()
  (when (and citre-mode (citre-tags-file-path)) (gen-tags)))

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (format "... / %d"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))
    (overlay-put ov 'face `(:inherit shadow))))
(defun ttn-hs-hide-level-1 ()
  (when (hs-looking-at-block-start-p)
    (hs-hide-level 1))
  (forward-sexp 1))
(defun my-inf-sml-mode-hook ()
  "Local defaults for inferior SML mode"
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  ;; (setq comint-buffer-maximum-size 400)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-input-autoexpand nil))
(defun my-sml-mode-hook ()
  "Push company-mlton with dabbrev into company-backends."
  (require 'company-mlton)
  (make-local-variable 'company-backends)
  (push '(company-mlton-keyword company-mlton-basis :with company-dabbrev-code) company-backends)
  (company-mlton-basis-autodetect)
  ;; (company-mlton-basis-load company-mlton-basis-file)
  )
(defun more-normal-sml-indentation-post-self-insert-function ()
  "Reindent the current line after a keyword and a space are inserted."
  (when (and (= (char-before) 32)
             (not executing-kbd-macro)
             (not noninteractive)
             (looking-back (concat sml-keywords-regexp "[ ]")))
    (call-interactively 'indent-for-tab-command)))
(put 'more-normal-sml-indentation-post-self-insert-function 'priority 100) ;; does this do anything?
(defun sml-prog-proc-send-region-by-string (begin end)
  (interactive "r")
  (let ((proc (sml-prog-proc-proc))
        (code (buffer-substring begin end)))
    (sml-prog-proc-send-string proc code)))
(defun c-prog-competition-build ()
  (set (make-local-variable 'compile-command)
       (concat (concat "make -k "
                       buffer-file-name) " build")))
(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
           2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(defun dired-yank-file-name-other-buffer ()
  "Yank current file name to the other buffer."
  (interactive)
  (dired-copy-filename-as-kill)
  (other-window -1)
  (yank-new-line)
  (other-window -1)
  (call-interactively 'dired-flag-file-deletion))
(defun dired-view-file-other-window ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (progn
        (view-file-other-window file)
        (other-window -1)))))
(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(defvar nyan-prompt-timer nil)
(defun nyan-at-time (time)
  "Nyan after set time."
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

(defvar quick-bind-availaible-keys '("C-j" "C-t" "C-RET"))
(defvar binding-ring (make-ring (length quick-bind-availaible-keys)))
(dolist (elem quick-bind-availaible-keys) (ring-insert binding-ring elem))
(defun quick-bind (arg)
  (interactive "P")
  (let ((quick-expr (read--expression "Expr: "))
        (quick-keybind (ring-ref binding-ring -1)))
    (ring-insert binding-ring quick-keybind)
    (local-set-key (kbd quick-keybind) quick-expr)
    )
  ;; (if arg
  ;; )
  )

(defun emacs-welcome()
  "Setup usual buffers."
  (find-file "~/org/todo.org")
  (split-window-below)
  (find-file "~/buffer/")
  (eyebrowse-create-window-config)
  (other-window 1))

;;; Keybindings
(luna-def-key
 "C-0" (bind  (delete-window) (balance-windows))
 "C-1" #'delete-other-windows
 "M-2" #'split-window-below-focus ;; or cycle
 "M-3" #'split-window-right-focus ;; of cycle
 "M-4" #'find-file-other-window
 "M-5" #'make-frame-command
 "M-o" #'other-window
 "C-x o" #'ace-window
 "C-<tab>" #'mode-line-other-buffer
 ;; "C-<tab>" #'iflipb-next-buffer
 ;; "C-S-<tab>" #'iflipb-previous-buffer
 "C-x k" #'kill-current-buffer
 "s-k" #'kill-buffer
 "C-z" #'delete-frame
 "C-x C-'" #'toggle-frame-fullscreen ;;#'writeroom-mode

 [remap keyboard-quit] #'keyboard-quit-context+

 "C-w" #'kill-region-or-sexp-whole-region
 "M-w" #'easy-kill
 "M-m" #'kill-back-to-indentation
 "M-k" #'kill-same-indent
 "C-M-<backspace>" #'avy-zap-up-to-char-dwim
 "C-d" #'delete-forward-char

 "M-s ;" #'avy-goto-line
 "M-Z" #'avy-zap-to-char-dwim
 "C-c ;" #'avy-goto-char-timer
 "C-c C-;" #'avy-goto-symbol-1
 "C-;" #'avy-goto-word-or-subword-1

 "C-c C-/" #'undo-propose
 "C-x C-u" #'undo-only
 "C-c s" #'fixup-whitespace
 [remap just-one-space] #'cycle-spacing
 "C-x j" #'join-line
 "M-j" (bind (join-line -1))

 "C-c C-i" 'indent-buffer
 "s-a" 'align
 "C-s-a" 'align-regexp
 "M-_" 'align-to-string
 "M-s M-f" #'fill-paragraph
 "M-s M-q" #'fill-region
 [remap fill-paragraph] #'fill-or-unfill
 "M-s t" #'tab-to-tab-stop

 "M-s M-l" #'downcase-dwim
 "M-s M-c" #'capitalize-dwim
 "M-s M-u" #'upcase-dwim

 "C-x C-;" #'comment-dwim
 "M-;" #'comment-line-extra

 "C-)" #'paredit-forward-slurp-sexp
 "C-}" #'paredit-forward-barf-sexp
 "M-(" #'paredit-wrap-round
 "M-s M-s" #'paredit-splice-sexp

 ;; "C-<return>" #'open-line-below
 ;; "M-<return>" #'open-line-above
 ;; "C-o" #'open-line-below
 ;; "C-o" #'open-line-above

 "C-M-e" #'end-of-delim
 "M-s a" #'ack
 "C-M-'" 'affe-grep
 "M-s M-'" 'affe-find
 "C-c C-'" 'sanityinc/affe-grep-at-point
 "C-c C-p" #'swiper
 "C-c C-o" #'occur
 "M-g o" 'dumb-jump-go-other-window
 "M-g j" 'dumb-jump-go
 "M-g i" 'dumb-jump-go-prompt

 "C-x C-." #'pop-global-mark
 "M-c" #'goto-last-change ;; or reverse
 "M-p" #'quick-jump-backward
 "M-n" #'quick-jump-forward
 ;; "M-l" #'move-to-window-line-top-bottom ;; or M-r

 "C-c e" #'er/expand-region
 "M-h" #'er/expand-region

 "C-c q" #'vr/replace
 "C-c C-q" #'vr/query-replace
 "M-s r" #'query-replace
 "M-s M-r" #'query-replace-regexp

 "C-c m" 'vr/mc-mark
 "C-S-c C-;" #'ace-mc-add-multiple-cursors
 "C-S-c C-M-;" #'ace-mc-add-single-cursor
 "C-S-c C-S-c" #'mc/edit-lines
 "C-S-c C-a" #'mc/edit-beginnings-of-lines
 "C-S-c C-e" #'mc/edit-ends-of-lines
 "C-<" #'mc/mark-previous-like-this
 "C->" #'mc/mark-next-like-this
 "C-S-<mouse-1>" #'mc/add-cursor-on-click
 "C-c C-<" #'mc/mark-all-like-this

 "C-x n d" (bind (cond ((buffer-narrowed-p) (widen)) (t (narrow-to-defun))))
 "C-x n s" #'narrow-quotes
 "C-c f" 'vimish-fold-toggle-dwim
 "C-c C-f" 'vimish-fold-delete

 "M-/" #'dabbrev-completion
 "C-M-/" #'hippie-expand
 "M-s M-/" #'dabbrev-expand

 "C-x c j" #'citre-jump
 "C-x c J" #'citre-jump-back
 "C-M-;" #'citre-ace-peek

 "C-c M-o" (bind (find-file "~/org/buffer.org"))
 "M-s M-d" #'delete-this-file

 "C-c w" #'webjump
 "C-x p S" 'project-search
 "C-x p D" 'project-display-buffer
 "C-c v" #'vterm

 "M-s M-e" #'evil-mode

 "M-s i" 'toggle-input-method
 "s-f" #'auto-fill-mode
 "s-t" #'toggle-truncate-lines
 "M-s s" 'sticky-buffer-mode
 "M-s C-f" #'follow-mode

 "C-c C-n" #'bm-toggle
 "C-@" #'bm-show-all

 "C-c h" #'symbol-overlay-put
 "C-c M-h" #'symbol-overlay-query-replace

 "M-s z" #'ztree-dir
 "M-s M-z" #'ztree-diff

 "C-c r" #'region-history-other
 "C-x g" #'magit-status
 "C-c g" #'magit-dispatch
 "C-c M-g" #'magit-file-dispatch
 "C-c C-g" #'magit-blame

 "M-s n" #'nyan-at-time
 "M-s M-n" #'nyan-at-quater-or-cancel

 "C-c d" 'deft
 "C-x M-d" 'deft-find-file

 "C-c l" #'org-store-link
 "C-c a" #'org-agenda
 "C-c c" #'org-capture
 "M-s b" #'burly-open-bookmark
 "M-s M-b" #'burly-bookmark-frames

 "C-x f" #'consult-recent-file
 "C-x m" #'consult-mode-command
 ;; "C-x r x" #'consult-register
 "C-c b" #'consult-bookmark
 "C-x b" #'consult-buffer
 "C-x C-b" #'consult-buffer
 "C-c C-b" #'ibuffer
 "s-b" #'frog-jump-buffer                ; or isearchb-activate
 "C-x 4 b" #'consult-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame

 ;; "M-'" #'consult-line
 "C-c o" #'consult-outline
 "C-c i" #'consult-project-imenu
 "C-'" #'consult-line
 "C-c C-s" #'consult-line
 [remap goto-line] #'consult-goto-line
 "M-s m" #'consult-mark
 "M-s k" #'consult-global-mark
 "M-s g" #'consult-git-grep
 "M-s e" #'consult-error
 "M-s M-g" #'consult-grep
 "M-s f" #'consult-find
 "M-s l" #'consult-line
 ;; "M-y" #'consult-yank-pop
 "M-s M-o" #'consult-multi-occur

 :keymaps 'help-map
 "C-i" #'info-display-manual
 :keymaps 'indent-rigidly-map
 "C-f" #'indent-rigidly-right
 "M-f" #'indent-rigidly-right-to-tab-stop
 "C-b" #'indent-rigidly-left
 "M-b" #'indent-rigidly-left-to-tab-stop
 :keymaps 'superword-mode-map
 "C-M-f" #'temp-subword-forward-word
 "C-M-b" #'temp-subword-backward-word
 "C-<backspace>" #'temp-subword-backward-delete-word
 :keymaps 'paredit-everywhere-mode-map
 "C-k" 'paredit-kill
 "C-c M-f" #'paredit-add-to-previous-list
 "C-c M-b" #'paredit-add-to-next-list
 "C-c M-p" #'paredit-join-with-previous-list
 "C-c M-n" #'paredit-join-with-next-list
 "C-x C-;" #'paredit-comment-dwim
 "M-r" nil ;; paredit-raise-sexp
 "M-s" nil ;; paredit-splice-sexp
 "M-d" #'paredit-forward-kill-word ;; paredit-forward-kill-word
 "M-DEL" nil  ;; paredit-backward-kill-word
 "M-]" nil
 "C-(" #'paredit-backward-slurp-sexp
 "C-{" #'paredit-backward-barf-sexp
 "C-M-n" #'paredit-forward
 "C-M-p" #'paredit-backward
 "C-M-d" #'paredit-forward-down
 :keymaps 'isearch-mode-map
 "TAB" #'isearch-dabbrev-expand
 "M-/" #'isearch-dabbrev-expand
 [remap isearch-delete-char]  #'isearch-del-char
 "C-c C-o" #'isearch-occur
 "C-n" #'isearch-repeat-forward
 "C-p" #'isearch-repeat-backward
 "M-w" #'isearch-yank-word-or-char
 "C-w" #'isearch-forward-symbol-at-point
 "C-RET" #'sanityinc/isearch-exit-other-end
 "M-<" #'isearch-beginning-of-buffer
 "M->" #'isearch-end-of-buffer
 :keymaps 'company-active-map
 "TAB" (bind (when (null (yas-expand)) (company-select-next)))
 "C-d" #'company-complete-selection
 "M-d" #'company-show-doc-buffer
 "M-." #'company-show-location
 "M-/" #'company-other-backend
 ;; "C-p" 'company-select-previous
 ;; "C-n" 'company-select-next
 ;; "M-p" 'previous-line
 ;; "M-n" 'next-line
 "C-s" #'company-filter-candidates
 "C-M-s" #'company-search-candidates
 :keymaps 'company-search-map
 "C-p" #'company-select-previous
 "C-n" #'company-select-next
 :keymaps 'minibuffer-local-map
 "C-'" (bind (kill-same-indent) (insert "~/"))
 "C-c C-o" #'embark-export
 "C-c C-c" #'embark-act
 :keymaps 'yas-minor-mode-map
 "C-c y" #'company-yasnippet
 :keymaps 'c-mode-base-map
 ;; "C-c C-j" 'eshell
 "C-c C-k" #'compile
 :keymaps 'comint-mode-map
 "C-l" #'comint-clear-buffer
 :keymaps 'sml-mode-map
 "C-c C-a" #'sml-run
 "C-<return>" #'sml-electric-pipe
 [remap mark-defun] #'sml-mark-function
 "C-c C-r" #'sml-prog-proc-send-region-by-string
 :keymaps 'scheme-mode-map
 "C-c C-l" #'scheme-load-file
 :keymaps 'haskell-mode-map
 "C-c C-s" nil
 "C-c M-s" #'haskell-mode-toggle-scc-at-point
 "C-c C-a" #'haskell-process-reload
 ;; "C-c C-c" #'haskell-compile
 "C-c C-h" #'haskell-hoogle
 "C-c C-." #'haskell-navigate-imports
 :keymaps 'dired-mode-map
 "C-c C-l" #'dired-up-directory
 "C-w" #'wdired-change-to-wdired-mode
 "r" #'dired-do-rename
 "y" #'dired-yank-file-name-other-buffer
 "v" #'dired-view-file-other-window
 "V" #'dired-view-file                   ; override (dired-do-run-mail)
 "K" #'dired-k
 :keymaps 'magit-status-mode-map
 "C-c p" #'magit-patch
 :keymaps 'eshell-mode-map
 "C-z" (bind (eshell-life-is-too-much) (delete-frame))
 :keymaps 'esh-autosuggest-active-map
 "C-e" #'company-complete-selection
 :keymaps 'org-mode-map
 "C-'" nil
 "C-c t" #'org-done
 "s-;" #'org-refile-goto
 "C-s-;" #'org-refile-goto-last-stored
 "C-c C-i" nil
 "C-c M-i" #'org-ctrl-c-tab
 "C-S-n" #'org-move-subtree-down
 "C-S-p" #'org-move-subtree-up
 :keymaps 'elpher-mode-map
 "p" #'elpher-prev-link
 "n" #'elpher-next-link
 )
(browse-kill-ring-default-keybindings)
(emacs-welcome)
