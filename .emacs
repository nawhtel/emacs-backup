;; TODO: ways to display and open important / frequent files
;; setq in use-package :init , adjust gc values , set up flymake, go-mode, gnu newsreader, 

;; General
;; (setq gc-cons-percentage 0.2)
;; (setq gc-cons-threshold (* 2 1000 1000))
(setq load-prefer-newer t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/save/"))
      delete-by-moving-to-trash t
      version-control t
      kept-new-versions 7
      delete-old-versions t
      backup-by-copying-when-mismatch t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :custom
  (use-package-enable-imenu-support t)
  :config
  ;; (setq use-package-always-ensure t) ;;on new machine
  (setq use-package-verbose t))

(use-package amx
  :bind ("M-x" . amx))

(use-package dired
  :hook (dired-mode . dired-omit-mode)  ; Use C-x M-o to show omitted files.
  :bind (:map dired-mode-map
	      ("C-c c-l" . dired-up-directory))
  :config
  ;; (use-package diredful
  ;;   :config (diredful-mode))
  ;; Notes: WDired can be enabled with C-x C-q and compiled with C-c C-c
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-recursive-copies 'always
        dired-listing-switches "-laGh1v --group-directories-first")
  (use-package dired-x))

(use-package exec-path-from-shell :defer t)
;; prepare for mac: vars, rc, font(xft?), x window difference, super key and command key relation, xdg dir / freedesktop difference (e.g. trash can), bsd difference, cross-platform specifics and check, special considerations for laptop, mac filesystem hierarchy, csh difference, mac package management (brew or ports or nix or no management at all) specifics

;; ex:
;; (use-package reveal-in-osx-finder
;;   :bind ("C-c f" . reveal-in-osx-finder))

(use-package ivy
  :demand t
  :init
  (setq enable-recursive-minibuffers t)
  :bind (([remap list-buffers] . ivy-switch-buffer) ;;C-c C-k to kill buffer, occur for ibuffer
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
	 ("C-i" . ivy-done)
	 ("C-<return>" . ivy-done)
         ("C-l" . ivy-backward-kill-word))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
	;; ivy-wrap t
	ivy-height 12
	ivy-use-virtual-buffers t
	ivy-use-selectable-prompt t
	ivy-initial-inputs-alist nil
	ivy-format-function 'ivy-format-function-arrow)
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus) t)
  ;;  (add-to-list 'ivy-re-builders-alist '(amx . ivy--regex-fuzzy) t) ;; ido like behavior
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbrev
	ivy-virtual-abbreviate 'full
	ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package counsel
  :after ivy
  :demand t
  :bind (("M-y"     . counsel-yank-pop)
         ("C-@" . counsel-mark-ring)
         ("C-*"     . counsel-org-agenda-headlines)
         ("C-x f" . counsel-recentf)
         ("C-c i" . counsel-semantic-or-imenu)
         ("C-c e v"  . counsel-set-variable)
         ("C-c e f" . counsel-find-library)
         ("C-c e l" . counsel-load-library)
         ("M-s f" . counsel-file-jump)
         ("M-s g" . counsel-rg)
         ("M-s j" . counsel-dired-jump)
         ([remap bookmark-jump] . counsel-bookmark)
         ([remap find-file] . counsel-find-file)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable))
  :config
  (setq counsel-find-file-ignore-regexp "\\`\\."
	counsel-dired-listing-switches "-laGh1v --group-directories-first"
        counsel-linux-app-format-function
        'counsel-linux-app-format-function-name-only)
  (add-to-list 'ivy-sort-matches-functions-alist
	       '(counsel-find-file . ivy--sort-files-by-date) t))

(use-package flx)

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "4:30am"))

(use-package recentf
  :config
  (setq recentf-max-menu-items 30
	recentf-max-saved-items 100)
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (recentf-mode t))

(use-package saveplace
  :config
  (save-place-mode t))

;; use-package save-visited-files
;;   :config (turn-on-save-visited-files-mode))
;; desktop-save

;; Completion
(use-package company
  :hook (after-init . global-company-mode)
  :init
  (add-to-list 'completion-styles 'initials t)
  :bind (:map company-active-map
	      ([tab] . company-complete)
	      ("M-i" . company-complete-selection)
	      ("C-d" . company-complete-selection)
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)
	      ("C-s" . company-filter-candidates)
	      ("C-M-s" . company-search-candidates)
	      :map company-search-map
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next))
  :config
  (setq company-show-numbers t
	;; company-idle-delay 0.3
	company-selection-wrap-around t
	company-dabbrev-minimum-length 3
	company-dabbrev-ignore-case t
	company-dabbrev-code-ignore-case t
	;; company-dabbrev-char-regexp "[[:word:]_:@.-]+"
	company-transformers '(company-sort-by-occurrence)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode))
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-hook 'c++-mode-hook
	    (lambda () (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.1.0"))))

(use-package bash-completion :defer t)
;; (bash-completion-setup))

;; (use-package fish-completion
;;   :after (:any shell eshell)
;;   :config
;;   (setq fish-completion-fallback-on-bash-p t)
;;   (global-fish-completion-mode))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind (:map yas-minor-mode-map
	      ("C-c y" . company-yasnippet))
  :config
  ;; (setq yas-prompt-functions '(yas-completing-prompt))
  ;; (setq yas-triggers-in-field t)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package yasnippet-snippets :after yasnippet)

;; Editing
(setq-default indent-tabs-mode nil)
(setq kill-whole-line t
      ;; tab-always-indent 'complete
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      kill-ring-max 80)

(delete-selection-mode)
(electric-pair-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'subword-mode)
;;(global-superword-mode)
(require 'iso-transl)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

(defun align-to-string (beg end)
  "Align region along character CHAR from BEG to END."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))

(defun clear-line ()
  "Clear the line, but don't delete it."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (indent-according-to-mode))

(defun fill-line ()
  "Join the following line onto the current line."
  (interactive)
  (join-line -1))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun narrow-dwim ()
  "Widen if currently narrowed, else narrow to function."
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen))
   (t (narrow-to-defun))))

(defun open-buffer-file ()
  "Open buffer file."
  (interactive)
  (find-file "~/org/buffer.org"))

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

(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1))

;; bindable keys
;; C-x C-u C-x C-l C-M-/
;; C-!     C-$     C-^     C-(     C-=     C-}     C-:     C-|     C-.     C-`
;; C-#     C-%     C-&     C-)     C-{     C-;     C-"     C-,     C-?     C-~     
;; C-= C-, M-[ M-] c-` c-;

(bind-keys
 ("C-o" . open-line-and-indent)
 ("M-j" . fill-line)
 ("M-r" . isearch-backward)
 ("C-r" . backward-kill-word)
 ("M-p" . backward-paragraph)
 ("M-n" . forward-paragraph)
 ("M-l" . move-to-window-line-top-bottom)
 ("C-d" . delete-forward-char)
 ("M-o" . other-window)
 ("M-=" . align-to-string)
 ("C-x j" . join-line)
 ("C-x s" . save-all)
 ("C-c C-i" . indent-buffer)
 ("C-x n d" . narrow-dwim)
 ("C-c h" . region-history-other)
 ("C-S-k" . clear-line)
 ("<C-return>" . open-line-below)
 ;;("<S-return>")
 ([f5] . revert-buffer)
 ([remap just-one-space] . cycle-spacing)
 ([remap capitalize-word] . capitalize-dwim)
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 :map help-map
 ("C-i" . info-display-manual)
 )

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package expand-region
  :bind ("C-c C-e" . er/expand-region))

(use-package evil
  :bind ("C-c v" . evil-mode))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package flymake
  :bind (:map flymake-mode-map
              ("C-;" . flymake-goto-next-error)
              ("C-:" . flymake-goto-prev-error)))

(use-package flyspell
  ;; :hook ((text-mode . flyspell-mode))
  ;;	 (prog-mode . flyspell-prog-mode))
  :bind (:map flyspell-mode-map
              ([f6] . cycle-ispell-languages))
  :config
  ;;  (setq flyspell-issue-message-flag nil)
  ;; (define-key flyspell-mode-map (kbd "C-M-i") nil)
  ;; (setq flyspell-auto-correct-binding (kbd "C-M-;")))
  (defvar lang-ring)
  (let ((langs '("francais" "american")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
  (defun cycle-ispell-languages ()
    "Cycle through dictionary."
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang))))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

(use-package goto-last-change
  :bind ("C-?" . goto-last-change))

;; (use-package hideshow
;;   :bind (:map hs-minor-mode-map
;;               ("C-c C-t" . hs-toggle-hiding)
;;               ("C-c @" . counsel-mark-ring)
;;               ("C-c C-M-h" . hs-hide-all)
;;               ("C-c C-M-l" . hs-hide-level)
;;               ("C-c C-M-s" . hs-show-all)))

;; (use-package reveal
;;   :diminish
;;   :hook (hs-minor-mode . reveal-mode))

(use-package ibuffer
  :bind (("M-s b" . ibuffer)
         ("C-c C-b" . ibuffer))
  :config
  (add-hook 'ibuffer-hook 'ibuffer-auto-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; experiment with this snippet
(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list)
              :map lisp-mode-map
              ("<return>" . paredit-newline)
              :map emacs-lisp-mode-map
              ("<return>" . paredit-newline))
  :config
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(use-package pyim
  :demand t
  :bind (("C-c C-p" . toggle-input-method) ;与 pyim-probe-dynamic-english 配合
	 ("C-c C-SPC" . toggle-input-method))
  :config
  ;; (pyim-isearch-mode 1)
  ;; (add-hook 'after-init-hook
  ;; '(lambda () (pyim-restart-1 t)))
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim"
	pyim-page-tooltip 'posframe))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package string-edit
  :bind ("C-c C-'" . string-edit-at-point))

;; Jumping
(setq switch-to-buffer-preserve-window-point t
      ;; recenter-positions '(middle bottom top)
      set-mark-command-repeat-pop t
      isearch-allow-scroll t
      auto-window-vscroll nil
      scroll-step 0
      scroll-error-top-bottom t
      scroll-conservatively 10000
      scroll-preserve-screen-position t)

(windmove-default-keybindings)

(use-package avy
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-'"   . avy-goto-char-timer)
         ("M-s s" . avy-goto-line))
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  (setq avy-timeout-seconds 0.4)
  (setq avy-styles-alist '((ivy-avy . pre) (avy-goto-line . pre))))

(use-package avy-zap
  :after avy
  :bind (("C-c C-z" . avy-zap-up-to-char-dwim)
         ("M-z" . avy-zap-to-char-dwim)))

;; (use-package ace-mc
;;     :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
;; 	   ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package ace-pinyin :defer t)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  ;;(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package dumb-jump
  :bind (("M-s g" . dumb-jump-go-other-window)
         ("M-s j" . dumb-jump-go)
         ("M-s q" . dumb-jump-quick-look)
         ("M-s p" . dumb-jump-back))
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  (setq dumb-jump-selector 'ivy))

(use-package imenu-anywhere)

(use-package link-hint
  :after avy
  :bind ("C-c l" . link-hint-open-link)
  :config
  (setq link-hint-avy-style 'pre))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-c C-s" . swiper-all)
         :map swiper-map
         ("C-l" . swiper-recenter-top-bottom)))

;; Display
(setq-default line-spacing 1
              cursor-type 'bar)
(setq global-hl-line-sticky-flag t
      help-window-select t
      display-time-default-load-average 1
      window-combination-resize t
      echo-keystrokes 0.25)

;;(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(column-number-mode)
(display-time-mode)
;;(global-hl-line-mode 0)
;;(whitespace-mode)
(mouse-avoidance-mode 'banish)
(show-paren-mode)
(remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)
;;(add-hook 'focus-out-hook 'balance-windows)

;; (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")) (:eval (if (buffer-modified-p) " •")) " - %m"))

(use-package ample-theme :defer t)

(use-package beacon
  :config
  (setq beacon-size 20)
  (setq beacon-lighter "")
  (beacon-mode))

(use-package eyebrowse
  :config
  ;;(setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode 1))

(use-package nimbus-theme
  :config
  (nimbus-theme))

;; (use-package page-break-lines
;;   :config
;;   (global-page-break-lines-mode))

(use-package powerline
  :after nimbus-theme
  :config
  (set-face-background 'mode-line "#5080b0") ;; a bit darker than steelblue
  (set-face-foreground 'powerline-active2 "coral") ;; lighter than tomato
  (set-face-background 'powerline-active2 "grey18")
  (setq powerline-default-separator 'arrow)
  (defun powerline-default-theme-alt ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face0 (if active 'powerline-active0 'powerline-inactive0))
			    (face1 (if active 'powerline-active1 'powerline-inactive1))
			    (face2 (if active 'powerline-active2 'powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw "%*" face0 'l)
				       (when powerline-display-buffer-size
					 (powerline-buffer-size face0 'l))
				       (when powerline-display-mule-info
					 (powerline-raw mode-line-mule-info face0 'l))
				       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				       (powerline-raw "%02l:%2c" face0)
				       (when (and (boundp 'which-func-mode) which-func-mode)
					 (powerline-raw which-func-format face0 'l))
				       (powerline-raw " " face0)
				       (funcall separator-left face0 face1)
				       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					 (powerline-raw erc-modified-channels-object face1 'l))
				       (powerline-major-mode face1 'l)
				       (powerline-process face1)
				       (powerline-minor-modes face1 'l)
				       (powerline-narrow face1 'l)
				       (powerline-raw " " face1)
				       (funcall separator-left face1 face2)
				       (powerline-vc face2 'r)
				       (when (bound-and-true-p nyan-mode)
					 (powerline-raw (list (nyan-create)) face2 'l))))
			    (rhs (list (powerline-raw global-mode-string face2 'r)
				       (funcall separator-right face2 face1)
				       (unless window-system
					 (powerline-raw (char-to-string #xe0a1) face1 'l))
				       (funcall separator-right face1 face0)
				       (powerline-raw " " face0)
				       (powerline-raw "%6p" face0 'r)
				       (when powerline-display-hud
					 (powerline-hud face0 face2))
				       (powerline-fill face0 0)
				       )))
		       (concat (powerline-render lhs)
			       (powerline-fill face2 (powerline-width rhs))
			       (powerline-render rhs)))))))
  (powerline-default-theme-alt))

(use-package popwin
  :config
  (setq popwin:popup-window-height 20)
  (popwin-mode 1))
;;try customizing display-buffer if having trouble

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode))

(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map
	      ("C-h" . which-key-C-h-dispatch))
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode))

;; Major Mode
(use-package compile
  :defer 5
  :config
  (setq compilation-scroll-output 'first-error
	compilation-ask-about-save nil
	compilation-always-kill t))

;; (use-package clojure-mode
;;   :defer t)

;; (use-package cider
;;   :after clojure-mode
;;   :config
;;   (setq cider-use-overlays nil))

(use-package cc-mode
  :bind (:map c-mode-base-map
	      ("C-c C-j" . compile) ;; Programming Competition
	      ("C-c C-k" . eshell))
  :config
  ;; (add-hook 'c-mode-common-hook
  ;; 	      (lambda ()
  ;; 		(set (make-local-variable 'compile-command)
  ;; 		     (concat (concat "make -k "
  ;; 				     buffer-file-name) " build")))))
  (c-toggle-auto-newline)
  ;;nand2tetris
  (add-to-list 'auto-mode-alist '("\\.hdl\\'"  . c-mode))
  (add-to-list 'auto-mode-alist '("\\.jack\\'" . java-mode))
  (add-to-list 'auto-mode-alist '("\\.over\\'" . json-mode))
  )

(use-package discover-my-major
  :bind (:map help-map
	      ("C-m" . discover-my-major) ;;view-order-manuals
	      ("C-M-m" . discover-my-mode)))

;; (use-package go-mode
;;   :bind (("C-c C-a" . go-test-current-project)
;;          ("C-c C-m" . go-test-current-file)
;;          ("C-c ." . go-test-current-test)
;;          ("C-c C-b" . go-run)
;;          ("C-h C-f" . godoc-at-point))
;;   (add-hook 'before-save-hook 'gofmt-before-save nil t)
;;   (set (make-local-variable 'company-backends) '(company-go))
;;   (go-eldoc-setup)
;;   (use-package go-imports
;;     (setq gofmt-command goimports)
;;     ))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  ;;(add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

(use-package haskell-mode
  :after company
  :bind (:map haskell-mode-map
	      ("C-#" . haskell-process-reload)
              ("C-c h" . haskell-hoogle)
	      ("C-c C-," . haskell-navigate-imports)
	      ("C-c C-." . haskell-mode-format-imports)
	      ("C-c C-u" . my-haskell-insert-undefined))
  :config
  ;;distribution specific
  (setq haskell-process-args-ghci
	'("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
	'("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
	'("--ghci-options=-ferror-spans -fshow-loaded-modules"
	  "--no-build" "--no-load"))
  (setq haskell-process-args-cabal-new-repl
	'("--ghci-options=-ferror-spans -fshow-loaded-modules"))

  (defun my-haskell-insert-undefined ()
    (interactive) (insert "undefined"))
  (setq haskell-process-log t
        haskell-process-auto-import-loaded-modules t
	haskell-process-suggest-remove-import-lines t
	haskell-interactive-popup-errors nil
	haskell-hoogle-command "hoogle")
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook (lambda () (set (make-local-variable 'company-backends) (append '((company-capf company-dabbrev-code)) company-backends)))))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command "pandoc"))

(use-package semantic
  :hook ((c-mode . semantic-mode)
	 (c++-mode . semantic-mode)
	 (semantic-mode . semantic-idle-summary-mode))
  :config
  (require 'semantic/bovine/gcc))

(use-package srefactor
  :after semantic)

;; Plugout
(use-package ediff
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("C-c d" . diff-hl-revert-hunk)
              ("C-<"   . diff-hl-previous-hunk)
              ("C->"   . diff-hl-next-hunk))
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
	 :map magit-status-mode-map
	 ("C-c f" . magit-format-patch))
  :config
  ;; (setq magit-display-buffer-function
  ;;       #'magit-display-buffer-same-window-except-diff-v1)
  (setq-default magit-diff-refine-hunk t))

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

(use-package org
  :init
  ;; (defun org-done ()
  ;;   "Change task status to DONE and archive it."
  ;;   (interactive)
  ;;   (org-todo 'done)
  ;;   (org-archive-subtree))

  ;; (defun org-refile-goto ()
  ;;   "Use org-refile to conveniently choose and go to a heading."
  ;;   (interactive)
  ;;   (let ((current-prefix-arg '(4))) (call-interactively 'org-refile))
  ;;   )

  ;; try this snippet
  (defvar org-capture-templates
    '(("t" "My TODO task format." entry
       (file+headline "todo.org" "Todo List")
       "* TODO %?\nSCHEDULED: %t")
      ("n" "My note format." entry
       (file "notes.org")
       "* %?")))
  :hook (org-mode . org-indent-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("C-S-n" . org-move-subtree-down)
         ("C-S-p" . org-move-subtree-up)
         ;; ("C-c t" . org-done)
         ;; ("s-;" . org-refile-goto)
         ("C-c j" . org-refile-goto-last-stored))
  :config
  (setq org-startup-folded nil
	org-startup-with-inline-images t
	org-startup-indented t
	org-startup-with-latex-preview t)
  (setq org-agenda-restore-windows-after-quit t
        ;; org-agenda-window-setup 'only-window
        ;; org-agenda-include-diary nil
        ;; org-agenda-todo-ignore-scheduled t
        ;; org-agenda-todo-ignore-deadlines t
        )
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t)
  (setq org-log-done 'time
        org-log-reschedule 'time
        ;; org-log-into-drawer t
        org-special-ctrl-a 'reversed
        org-catch-invisible-edits 'show
        org-return-follows-link t
        org-imenu-depth 3)
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        ;; org-src-tab-acts-natively t
        ;; org-image-actual-width nil
        )
  (setq org-directory "~/org"
        org-agenda-files '("~/org")
        org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (add-hook 'org-agenda-after-show-hook 'org-show-entry)
  (setq org-refile-use-outline-path t)
  ;; (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-interface 'outline-path-interface)
  (org-clock-persistence-insinuate))


(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

(use-package eshell-z :after eshell)
(use-package eshell-up :after eshell)

(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))

(use-package pomidor
  :bind ("C-c p" . pomidor))

;; to organize
;; (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)))
;; (global-set-key [f1] 'projectile-run-eshell)
;; (setq magit-repo-dirs
;;       (mapcar
;;        (lambda (dir)
;;          (substring dir 0 -1))
;;        (cl-remove-if-not
;;         (lambda (project)
;;           (unless (file-remote-p project)
;;             (file-directory-p (concat project "/.git/"))))
;;         (projectile-relevant-known-projects))))))
;; (use-package find-file-in-project
;;   :commands find-file-in-project)
;; :delight '(:eval (concat " " (projectile-project-name))))
;; (setq auto-revert-verbose nil)
;; :commands dired-jump)
;;     :commands (org-projectile-location-for-project)
;;     (setq org-projectile-projects-file org-projectile-file)
;;     (push (org-projectile-project-todo-entry :empty-lines 1)
;;           (org-projectile-per-project)
;;           (setq org-projectile-per-project-filepath org-projectile-file))))
;; (dired (projectile-project-root)))))
;;   (setq projectile-mode-line '(:eval (format " Pro[%s]" (projectile-project-name)))
;; (use-package projectile
;;   (projectile-global-mode 1)
;;   (setq projectile-enable-caching nil))
;; :config
;; (setq projectile-completion-system 'ivy))
;; 	    :defer 1
;; 	    :hook (prog-mode . projectile-mode)
;; 	    :config
;; 	    (setq projectile-completion-system 'helm)
;; 	    )    :bind* ("C-c TAB" . projectile-find-other-file)
;;   (projectile-global-mode 1)
;;   (setq projectile-enable-caching nil))
;; 	  :bind-keymap ("C-c p" . projectile-command-map)
;; 	  (setq projectile-mode-line '(:eval
;;                                        (if
;; 					   (file-remote-p default-directory)
;; 					   " Projectile"
;; 					 (format " [%hs]"
;; 						 (projectile-project-name)))))
;; 	  ;; :hook (prog-mode . turn-on-diff-hl-mode)
;; 	  (setq projectile-project-root-files-bottom-up
;; 		'(".git" ".projectile"))
;; 	  (setq projectile-completion-system 'ivy)
;; 	  (setq projectile-indexing-method 'alien)
;; 	  (setq projectile-enable-caching nil)
;; 	  (setq projectile-switch-project-action
;; 		(lambda ()
;; 		  (dired (projectile-project-root)))))
;; 	:hook (prog-mode . projectile-mode)
;; 	:bind ("<f6>" . projectile-compile-project)

;; Misc
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(defun emacs-welcome()
  "Display Emacs welcome screen."
  (interactive)
  (find-file "~/org/notes.org")
  (split-window-right)
  (find-file "~/org/todo.org")
  (split-window-below)
  (open-buffer-file)
  (other-window 1)
  )

(emacs-welcome)
