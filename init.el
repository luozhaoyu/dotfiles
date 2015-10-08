; basics

(global-hl-line-mode 1)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq column-number-mode t)
(setq scroll-step 1)
(setq scroll-margin 7)

; installed
; evil go-mode cider clojure-mode python-mode
; M-x package-list-package
; M-x package-menu-execute

(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
            package-archives )
(package-initialize)
(require 'evil)
(evil-mode 1)

(require 'yasnippet)
(yas-global-mode 1)

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
