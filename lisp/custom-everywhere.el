;;; custom-everywhere.el --- Configure emacs every where -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (eq system-type 'windows)
    (message "windows not support emacs every where")
  (progn
    (require-package 'emacs-everywhere)
    (require 'emacs-everywhere)
    (define-key emacs-everywhere-mode-map (kbd "C-c C-c") 'emacs-everywhere-finish)
    ))

(provide 'custom-everywhere)
;;; custom-everywhere.el ends here
