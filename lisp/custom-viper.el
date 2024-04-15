;;; custom-viper.el --- viper config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require-package 'evil)
(require-package 'key-chord)
(require 'evil)
(require 'key-chord)

(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.15)

(key-chord-define-global ",," 'turn-on-evil-mode)

(defun mystic/quit-evil (num)
  "IMPORTANT: this function has been rewrited in custom-viper.el.

It should not become a function, thought it *look like* control
the insert state by NUM.  When NUM is 1, the insert state is on.
Over write it can make the insert state being totally Emacsversion."
  (if (eq num 1) (turn-off-evil-mode)))
(advice-add 'evil-insert-state :override #'mystic/quit-evil)

(key-chord-mode 1)

(provide 'custom-viper)
;;; custom-viper.el ends here
