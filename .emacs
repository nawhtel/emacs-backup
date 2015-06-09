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
(setq company-idle-delay 0.09)
(setq company-minimum-prefix-length 2)

;;Company Clang
(add-hook 'c++-mode-hook
	  '(lambda () (setq company-clang-arguments
			    (list "-std=c++14"
				  "-stdlib=libstdc++"))))

;;Company C Headers
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c++-mode-hook
	  '(lambda ()
	    (setq company-c-headers-path-system
		  (list "/usr/include/c++/5.1.0/"
			"/usr/include/" "/usr/local/include/"))))

;;Electric-Pair Mode
(electric-pair-mode 1)

;;Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-language-standard "c++14")))
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-standard-library "libstdc++")))

;;GUI
(tool-bar-mode -1)
;(menu-bar-mode -1)
;(load-theme 'wombat t)

;;GLSL Mode
(autoload 'glsl-mode "glsl-mode" "GL Shading Language editing mode" t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;Haskell Mode
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;;Helm
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(add-to-list 'helm-sources-using-default-as-input
	     'helm-source-man-pages)

;;Racket Mode
(add-hook 'racket-mode-hook
          '(lambda ()
	     (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;;Miscellaneous
(setq visible-bell t)
