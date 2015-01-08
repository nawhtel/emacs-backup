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

;;Company Mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.09)
(setq company-minimum-prefix-length 2)

;;Company C Headers
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c++-mode-hook
	  '(lambda ()
	    (setq company-c-headers-path-system
		  (list "/usr/include/c++/4.9.2/"
			"/usr/include/" "/usr/local/include/"))))

;;Company Clang
(add-hook 'c++-mode-hook
	  '(lambda () (setq company-clang-arguments
			   (list "-std=c++14"
				 "-stdlib=libstdc++"))))

;;Electric-Pair Mode
(electric-pair-mode 1)

;;Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-language-standard "c++14")))
(add-hook 'c++-mode-hook
	  '(lambda () (setq flycheck-clang-standard-library "libstdc++")))
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;;Haskell Mode
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;;Lua Mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;Racket Mode
(add-hook 'racket-mode-hook
          '(lambda ()
	     (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;;QML Mode
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;Miscellaneous
(setq ring-bell-function 'ignore)
