;;;;;; custom-help.el --- Better help mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'helpful)
(require 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; https://github.com/Wilfred/helpful/issues/236
;; start info-lookup in init instead of lazy loading
(require 'info-look)
(info-lookup-setup-mode 'symbol 'emacs-lisp-mode)

(provide 'custom-help)
;;; init-mmm.el ends here
