; basics


(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 80)

(setq scroll-step 1)

(setq standard-indent 4)
(setq-default indent-tabs-mode nil)

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
