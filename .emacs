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

;;Auto-Complete Mode
(require 'auto-complete)
(ac-config-default)
(setq ac-delay 0.07)
(setq ac-auto-show-menu t)
(setq ac-disable-faces nil)
(setq ac-ignore-case t)
(setq ac-use-fuzzy t)

;;Auto-Complete-C-Headers
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories (list "." "/usr/include")))
(add-hook 'c-mode-hook 'my:ac-c-headers-init)
(defun my:ac-c++-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories (list "." "/usr/include/c++/4.9.2" "/usr/include" "/usr/include/qt")))
(add-hook 'c++-mode-hook 'my:ac-c++-headers-init)

;;Auto-Complete-Clang-Async
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (setq ac-clang-cflags (append '("-std=c++14") ac-clang-cflags))
  (ac-clang-launch-completion-process))
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)

;;Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-args
			   (append '("-std=c++14") flycheck-clang-args))))

;;YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;;Electric-Pair Mode
(electric-pair-mode 1)

;;Haskell Mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;Racket Mode
(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;;QML Mode
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;Miscellaneous
(setq ring-bell-function 'ignore)
