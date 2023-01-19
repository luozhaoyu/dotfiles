;;; typescript.el --- Support for typescript
;;; Commentary:
;; https://github.com/ananthakumaran/tide


;;; Code:


(defun setup-tide-mode () 
  "Setup Typescript mode." 
  (interactive) 
  (tide-setup) 
  (flycheck-mode +1) 
  (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
  (eldoc-mode +1) 
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

  ;; setup key binding
  ;; https://github.com/ananthakumaran/tide/blob/96bfc5da11a9b83b32368c38e933a405270652de/tide.el#L2257-L2258
  ;; https://github.com/emacs-evil/evil-collection#key-translation
  (evil-define-key 'normal tide-mode-map (kbd "gd") 'tide-jump-to-definition (kbd "gD")
    'tide-jump-back (kbd "gs") 'tide-references)

  ;; enable normal state mode (not only special mode) can go to reference by RET
  ;; https://github.com/ananthakumaran/tide/blob/master/tide.el#L1921
  (evil-define-key 'normal tide-references-mode-map (kbd "RET") #'tide-goto-line-reference)

  ;; disallow typescript mode to change indent behavior: use default behavior for tabs
  ;; https://github.com/emacs-typescript/typescript.el/blob/d79551c67ff5f2bd5f651eb411cdc66ceeb787e3/typescript-mode.el#L3061
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto_002dIndentation.html
  (setq-local indent-line-function 'tab-to-tab-stop)

  ;; disallow auto indent when typing some chars
  ;; https://github.com/emacs-typescript/typescript.el/blob/v0.4/typescript-mode.el#L2951-L2952
  (setq-local electric-indent-chars nil))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)


(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;; typescript.el ends here
