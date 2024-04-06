;;; package --- ---
;;; Commentary:
;; mostly company

;;; Code:
(require-package 'company)
(add-hook 'prog-mode 'company-mode)
(add-hook 'lisp-mode-hook (lambda () (company-mode nil)))
(global-set-key (kbd "C-<tab>") 'company-complete)
(provide 'custom-completion)
;;; custom-completion.el ends here
