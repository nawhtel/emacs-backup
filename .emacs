;;Backup & Auto-Save
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;Package System
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.07)
(setq company-minimum-prefix-length 2)

;;Company Clang
(add-hook 'c++-mode-hook
	  '(lambda () (setq company-clang-arguments
			    (list "-std=c++11"
				  "-stdlib=libstdc++"))))

;;Company C Headers
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (setq company-c-headers-path-system
		   (list "/usr/include/" "/usr/local/include/"
			 "/usr/include/c++/5.3.0/"))))

;;Editing
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-z") 'undo)

;;Electric-Pair Mode
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

;;FlyCheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-standard-library "libstdc++")))
(setq flycheck-display-errors-delay 0.2)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;;(add-hook 'c++-mode-hook
;;    (lambda ()
;;        (flyspell-prog-mode)))
;;    ‘flyspell-issue-message-flag'

;;GUI
(tool-bar-mode 0)
;;(menu-bar-mode 0)

;;Haskell Mode
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;;Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c o") 'helm-occur)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(add-to-list 'helm-sources-using-default-as-input
	     'helm-source-man-pages)

;;Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;Racket Mode
(add-hook 'racket-mode-hook
          '(lambda ()
	     (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;;Miscellaneous
(setq visible-bell t)
