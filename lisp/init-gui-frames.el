;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Stop C-z from minimizing windows
(global-set-key (kbd "C-z") nil)


;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; Window size and features

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

;; use the window action key to resize and drag
(let ((no-title '(undecorated . t)))
  (add-to-list 'default-frame-alist no-title)
  (add-to-list 'initial-frame-alist no-title))

(let ((no-border '(internal-border-width . 50)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq-default line-spacing 0.11)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))



(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))

         ;; the value of the alpha acts differently in int and float
         ;; the float is 0.0 to 1.0
         ;; the int is 0 to 100
         ;; it will broken if you accidentally get a float...
         (oldalpha (if (floatp oldalpha) (truncate (* oldalpha 100)) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


;; Change global font size easily
(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(require-package 'pulsar)
(require 'pulsar)

(pulsar-global-mode 1)

(advice-add 'switch-window :after 'pulsar-pulse-line)

(when (maybe-require-package 'key-chord)
  (key-chord-mode 1)
  (key-chord-define-global "``" 'pulsar-pulse-line))


(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(require-package 'disable-mouse)

(require-package 'hl-todo)
(require 'hl-todo)
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#99cc99")
        ("FIXME"  . "#ff3300")
        ("DEBUG"  . "#cc66cc")
        ("GOTCHA" . "#f2777a")
        ("STUB"   . "#66cccc")
        ("MIKU"   . "#39c5bb")
        ("LTY"    . "#66ccff"))) ;; what is lty???

(require-package 'minimap)
(require 'minimap)

(require-package 'posframe)
(require 'posframe)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
