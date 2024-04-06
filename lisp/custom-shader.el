;;; package --- settings for openGl shader
;;; Commentary:

;;; Code:
(define-derived-mode shader-language-mode c-mode "shader language"
  "Shader langue mode is a major for openGL shading language.")

(add-to-list 'auto-mode-alist '("\\.vs\\'" . shader-language-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . shader-language-mode))

(add-hook 'shader-language-mode-hook (lambda () (flymake-mode-off)))
(provide 'custom-shader)
;;; custom-shader.el ends here
