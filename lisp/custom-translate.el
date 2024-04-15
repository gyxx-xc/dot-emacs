;;; custom-translate.el --- Configure translate for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; hack gts-implements.el 162, remove evil
(require-package 'go-translate)
(require 'go-translate)

(setq gts-translate-list '(("en" "zh")))

(setq gts-default-translator
      (gts-translator
       :picker (gts-noprompt-picker)
       :engines (list (gts-google-rpc-engine))
       :render (gts-posframe-pop-render)))

(global-set-key (kbd "C-x C-t") 'gts-do-translate)

(provide 'custom-translate)
;;; custom-translate.el ends here
