;;; mystic-modeline.el --- modeline config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'doom-modeline)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-hud t)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-check-icon nil)
(setq doom-modeline-check-simple-format t)
(setq doom-modeline-enable-word-count t)

(provide 'mystic-modeline)
;;; mystic-modeline.el ends here
