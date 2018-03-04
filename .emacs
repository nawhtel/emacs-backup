;;Backup & Auto-Save
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
(setq delete-old-versions t
      kept-new-versions 7
      kept-old-versions 3
      version-control t
      backup-by-copying t)
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/")))

;;Packages & OS
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

;;Company C Headers
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c++-mode-hook
	  '(lambda () (add-to-list 'company-c-headers-path-system
				   '"/usr/include/c++/7.3.0")))

;;Editing
(add-hook 'text-mode-hook 'visual-line-mode)
(global-subword-mode t)
(electric-pair-mode t)

(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "C-c C-w") 'compare-windows)
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "M-o") 'other-window)

;;(global-set-key (kbd "C-S-h") help-map)
;;(global-set-key (kbd "C-h") 'backspace)
;;(global-set-key (kbd "s-h") 'delete-backward-char)
;;(global-set-key (kbd "C-s-h") 'backward-kill-word)

;;Files
(setq delete-by-moving-to-trash t)

;;Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

;;Flyspell
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

(defvar lang-ring)
(let ((langs '("francais" "american")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  "Cycle through dictionary."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(global-set-key [f6] 'cycle-ispell-languages)

;;GUI
(tool-bar-mode 0)
;;(scroll-bar-mode 0)
(load-theme 'ample t)
(show-paren-mode t)
(setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")) (:eval (if (buffer-modified-p) " •")) " - %m"))

;;Haskell Mode
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines t)

;;distribution specific
;; (setq haskell-process-args-ghci
;;       '("-ferror-spans" "-fshow-loaded-modules"))
;; (setq haskell-process-args-cabal-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
;; (setq haskell-process-args-stack-ghci
;;       '("--ghci-options=-ferror-spans -fshow-loaded-modules"
;; 	"--no-build" "--no-load"))
;; (setq haskell-process-args-cabal-new-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 (append '((company-capf company-dabbrev-code)) company-backends))))

;;Helm
(require 'helm)
(require 'helm-config)
(helm-mode t)
(helm-descbinds-mode)
(helm-autoresize-mode)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x o") 'helm-occur)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;Org Mode
(require 'org)
(setq org-startup-folded nil)
(setq org-startup-with-inline-images t)
(setq org-startup-indented t)
(setq org-startup-with-latex-preview t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done 'time)
;;(setq org-directory "~/Org")
;;(setq org-agenda-files (list "~/Org/Notes.org"))


;;Programming Competition
;; (defvar c-mode-base-map)
;; (add-hook 'c-mode-common-hook
;;           (lambda () (define-key c-mode-base-map
;; 		       (kbd "C-c C-j") 'compile)))

;; (add-hook 'c-mode-common-hook
;;           (lambda () (define-key c-mode-base-map
;; 		       (kbd "C-c C-k") 'eshell)))

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'compile-command)
;; 		 (concat (concat "make -k "
;; 				 buffer-file-name) " build"))))

;;Miscellaneous
(require 'iso-transl)
(setq visible-bell t)
