; basics

(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq column-number-mode t)
(setq scroll-step 1)
(setq scroll-margin 7)
(setq scroll-conservatively 10000)
(setq redisplay-dont-pause t)
(show-paren-mode 1)


; auto install
; evil go-mode cider clojure-mode python-mode
; M-x package-list-package
; M-x package-menu-execute
; list the packages you want installed
(require 'package)
(setq package-list '(
		     evil go-mode cider clojure-mode python-mode yasnippet
		     rainbow-delimiters highlight-symbol hl-sexp
		    ))
; list the repositories containing them
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


; installed package configurations
(require 'evil)
(evil-mode 1)

(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook #'hl-sexp-mode)

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'find-file-hook #'highlight-symbol-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
