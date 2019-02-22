;;; mypeloton.el --- settings to be compatible with company's existing style
;;; Commentary:
;;; These settings should do override, and can be commented out easily

;;; Code:

(defun peloton-sh-mode-setup ()
  "All existing coding conventions."
  (setq-default indent-tabs-mode nil)
  )
(add-hook 'sh-mode-hook 'peloton-sh-mode-setup)
(provide 'mypeloton)
;;; mypeloton.el ends here
