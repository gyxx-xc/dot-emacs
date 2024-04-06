;;; custom-wenyan.el --- Wenyan lang support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;(load "/home/gyxx/myProject/elisp/wenyan-mode/wenyan-mode.el")
(require 'wenyan-mode)

(define-key wenyan-mode-map (kbd "\\") (kbd "、"))
(define-key wenyan-mode-map (kbd ".") (kbd "。"))
(define-key wenyan-mode-map (kbd "[") (kbd "「"))
(define-key wenyan-mode-map (kbd "]") (kbd "」"))
(define-key wenyan-mode-map (kbd "{") (kbd "『"))
(define-key wenyan-mode-map (kbd "}") (kbd "』"))
(define-key wenyan-mode-map (kbd "<") (kbd "《"))
(define-key wenyan-mode-map (kbd ">") (kbd "》"))

(add-hook 'wenyan-mode-hook (lambda ()
                              (rainbow-delimiters-mode -1)))

(provide 'custom-wenyan)
;;; custom-wenyan.el ends here
