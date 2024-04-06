;;; custom-eaf --- the eaf settings.
;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)

(require 'eaf-2048)
(require 'eaf-browser)
(require 'eaf-camera)
(require 'eaf-demo)
;; (require 'eaf-file-browser)
(require 'eaf-file-manager)
(require 'eaf-file-sender)
(require 'eaf-image-viewer)
(require 'eaf-map)
(require 'eaf-markdown-previewer)
(require 'eaf-markmap)
(require 'eaf-mindmap)
(require 'eaf-music-player)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-org-previewer)
(require 'eaf-pdf-viewer)
(require 'eaf-rss-reader)
(require 'eaf-system-monitor)
;; (require 'eaf-terminal)
;; (require 'eaf-vue-demo)
;; (require 'eaf-vue-tailwindcss)

(setq eaf-pdf-dark-mode nil)

(setq eaf-pdf-notify-file-changed nil)

(provide 'custom-eaf)
;;; custom-eaf.el ends here
