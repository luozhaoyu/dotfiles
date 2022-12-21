;;; init.el --- Initialization file for Emacs
;;; Commentary:
;; Emacs reference card: https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf
;; C-h b (M-x describe-bindings) will show all bindings
;; C-h v (M-x describe-variable)
;; C-h f (M-x describe-function)
;; M-: eval-expression
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; M-x elisp-format-buffer


;;; Code:


(add-hook 'find-file-hook (lambda () 
			    (linum-mode 1)))
(setq column-number-mode t)
(setq scroll-step 1)
(setq scroll-margin 7)
(setq scroll-conservatively 10000)
(setq redisplay-dont-pause t)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html#g_t_0025_002dConstructs
(setq-default mode-line-buffer-identification '("%I %f")) ; display file size and full path
(show-paren-mode 1)


(defun load-directory (dir) 
  "Load config files from arbitrary DIR."
  (let ((load-it (lambda (f) 
		   (load-file (concat (file-name-as-directory dir) f))))) 
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/customize/")


;;; auto install
;; evil go-mode cider clojure-mode python-mode
;; M-x package-list-package
;; M-x package-menu-execute
;; list the packages you want installed
;; refresh package list if package becomes outdated
;; (package-refresh-contents)

(require 'package)
(setq package-list '(elisp-format evil go-mode cider clojure-mode python-mode yasnippet
				  yasnippet-snippets rainbow-delimiters highlight-symbol git-link
				  ;; hl-sexp
				  codesearch go-autocomplete go-eldoc flycheck epc jedi jedi-core
				  elpy neotree rust-mode yaml-mode tide))
;; list the repositories containing them
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
					;(add-to-list 'package-archives
					;             '("marmalade" . "http://marmalade-repo.org/packages/")
					;             )

;; activate all the packages (in particular autoloads)
(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))
;; install the missing packages
(dolist (package package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


;; installed package configurations
(require 'evil)
(evil-mode 1)
(evil-ex-define-cmd 
 "k"
 'kill-buffer)
;; need to customize evil-symbol-word-search to enable symbol search rather than word


;; for speeding up and down
(require 'cl)
(setq my-scroll-counter 0)
(setq up-down-gear 1)			; the smaller, the faster
(setq left-right-gear 2)
(setq my-last-scroll 0)
(setq my-scroll-interval 0.3)

(defun hjkl-timer() 
  "Every press will invoke this."
  (let ((now (float-time))) 
    (if (and (eq last-command this-command) 
	     (< (- now my-last-scroll) my-scroll-interval)) 
	(incf my-scroll-counter) 
      (setq my-scroll-counter 0)) 
    (setq my-last-scroll now)))

(defun speed-up() 
  "Every press will move cursor by my-scroll-counter." 
  (interactive) 
  (hjkl-timer) 
  (previous-line (+ 1 (/ my-scroll-counter up-down-gear))))
(defun speed-down() 
  "Every press will move cursor by my-scroll-counter." 
  (interactive) 
  (hjkl-timer) 
  (next-line (+ 1 (/ my-scroll-counter up-down-gear))))
(defun speed-left() 
  "Every press will move cursor by my-scroll-counter." 
  (interactive) 
  (hjkl-timer) 
  (left-char (+ 1 (/ my-scroll-counter left-right-gear))))
(defun speed-right() 
  "Every press will move cursor by my-scroll-counter." 
  (interactive) 
  (hjkl-timer) 
  (right-char (+ 1 (/ my-scroll-counter left-right-gear))))
(define-key evil-normal-state-map (kbd "k") 'speed-up)
(define-key evil-normal-state-map (kbd "j") 'speed-down)
(define-key evil-normal-state-map (kbd "h") 'speed-left)
(define-key evil-normal-state-map (kbd "l") 'speed-right)


(define-key evil-normal-state-map (kbd "gl") 'git-link)


(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

					;(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
					;(add-hook 'clojure-mode-hook #'hl-sexp-mode)

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

;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; go-mode
(defun go-mode-setup () 
  "Setup Golang." 
  (setq compile-command "golint && godep go build -v && godep go test -race -v && go vet") 
  (define-key (current-local-map) "\C-c\C-c" 'compile) 
  (go-eldoc-setup) 
  (setq gofmt-command "goimports") 
  (evil-define-key 'normal go-mode-map (kbd "gd") 'godef-jump) 
  (add-to-list 'load-path (expand-file-name (concat (getenv "GOPATH")
						    "/src/github.com/dougm/goflymake")))
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

  ;; go lint
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs")) 
  (require 'golint))
(add-hook 'go-mode-hook 'go-mode-setup)

(defun my-go-mode-hook () 
  "Personal go configs."
  (whitespace-mode -1)			; don't highlight hard tabs
  (setq tab-width 2			; display tabs as two-spaces
	indent-tabs-mode 1		; use hard tabs to indent
	fill-column 100))		; set a reasonable fill width
(add-hook 'go-mode-hook 'my-go-mode-hook)


;; rust-mode
(require 'rust-mode)
(setq rust-format-on-save t)
					;(with-eval-after-load 'rust-mode
					;  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; M-x package-install RET jedi RET
;; M-x jedi:install-server RET
;; Then open Python file
;; refer: http://tkf.github.io/emacs-jedi/latest/#install
(defun python-mode-setup () 
  "Setup Python mode."
  (jedi:setup) 
  (setq jedi:complete-on-dot t))
(evil-define-key 'normal python-mode-map (kbd "gd") 'jedi:goto-definition)
(add-hook 'python-mode-hook 'python-mode-setup)

(elpy-enable)

;; TODO: need a smart way to not auto format existing code
;; (add-hook 'python-mode-hook 'blacken-mode)
(evil-ex-define-cmd 
 "bb"
 'blacken-buffer)

;; codesearch https://github.com/abingham/emacs-codesearch#commands
(require 'codesearch)
(define-key evil-normal-state-map (kbd "gs") 'listing-codesearch-search)

;; https://www.emacswiki.org/emacs/NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(add-hook 'neotree-mode-hook (lambda () 
			       (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter) 
			       (define-key evil-normal-state-local-map (kbd "SPC")
				 'neotree-quick-look) 
			       (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide) 
			       (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter) 
			       (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh) 
			       (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line) 
			       (define-key evil-normal-state-local-map (kbd "p")
				 'neotree-previous-line) 
			       (define-key evil-normal-state-local-map (kbd "A")
				 'neotree-stretch-toggle) 
			       (define-key evil-normal-state-local-map (kbd "H")
				 'neotree-hidden-file-toggle)))
(define-key evil-normal-state-map (kbd "|") 'neotree-find)


;; tabs
(define-key evil-normal-state-map "t" 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map "T" 'tab-bar-switch-to-prev-tab)
(define-key evil-normal-state-map (kbd "C-t") 'tab-new)


;;(setq stack-trace-on-error t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-symbol-word-search t)
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(rust-mode rainbow-delimiters python-mode neotree jedi highlight-symbol go-eldoc go-autocomplete flycheck evil elpy codesearch cider))
 '(tide-allow-popup-select '(code-fix refactor)))
;;; init.el ends here
