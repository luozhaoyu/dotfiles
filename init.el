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
		     go-autocomplete go-eldoc flycheck
		     epc jedi jedi-core
		     neotree
		    ))
; list the repositories containing them
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             )
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             )

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

(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
(add-hook 'clojure-mode-hook #'hl-sexp-mode)

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.25)
(add-hook 'find-file-hook #'highlight-symbol-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:inverse-video t)))))

(require 'flycheck)
(global-flycheck-mode)

; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

; go-mode
(defun go-mode-setup ()
    (setq compile-command "golint && go build -v && go test -race -v && go vet")
    (define-key (current-local-map) "\C-c\C-c" 'compile)
    (go-eldoc-setup)
    (setq gofmt-command "goimports")
    (evil-define-key 'normal go-mode-map (kbd "gD") 'godef-jump)

    (add-to-list 'load-path (expand-file-name (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake")))
    ;(require 'flymake)
    ;(require 'go-flymake)
    ;(require 'go-flycheck)

    (require 'auto-complete)
    (require 'go-autocomplete)
    (require 'auto-complete-config)
    (ac-config-default)

    (require 'go-eldoc) ;; Don't need to require, if you install by package.el
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (add-hook 'before-save-hook 'gofmt-before-save)

    ; go oracle
    (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

    ; go lint
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (require 'golint))
(add-hook 'go-mode-hook 'go-mode-setup)

(defun my-go-mode-hook ()
    (whitespace-mode -1) ; don't highlight hard tabs
      (setq
       tab-width 2         ; display tabs as two-spaces
       indent-tabs-mode 1  ; use hard tabs to indent
       fill-column 100))   ; set a reasonable fill width
(add-hook 'go-mode-hook 'my-go-mode-hook)

; python-mode
(defun python-mode-setup ()
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.
    ; http://tkf.github.io/emacs-jedi/latest/#install
    (jedi:setup)
    (setq jedi:complete-on-dot t))
    (evil-define-key 'normal python-mode-map (kbd "gD") 'jedi:goto-definition)
(add-hook 'python-mode-hook 'python-mode-setup)


; https://www.emacswiki.org/emacs/NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(add-hook 'neotree-mode-hook
(lambda ()
  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;(setq stack-trace-on-error t)
