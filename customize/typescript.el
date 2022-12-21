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
    'tide-jump-back))


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)


(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;; typescript.el ends here
