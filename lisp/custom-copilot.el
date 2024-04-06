;;; custom-copilot.el --- copilot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))


;; don't use copilot in readonly mode
(add-hook 'prog-mode-hook (lambda () (unless buffer-read-only (copilot-mode 1))))

(setq copilot-enable-predicates (list 'copilot--buffer-changed))
(define-key copilot-mode-map (kbd "C-c <tab>") 'copilot-accept-completion)
(define-key copilot-mode-map (kbd "C-c f") 'repeatable-copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-c n") 'copilot-next-completion)

(defun repeat-command (command)
  "Repeat COMMAND."
  (require 'repeat)
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           #'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))
(defun repeatable-copilot-accept-completion-by-word ()
  "repeatable-copilot-accept-completion-by-word"
  (interactive)
  (repeat-command 'copilot-accept-completion-by-word))

(setq copilot-indent-offset-warning-disable t)

(provide 'custom-copilot)
;;; custom-copilot.el ends here
