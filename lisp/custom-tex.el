;;; custom-tex.el --- tex(latex) config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require-package 'auctex)
(require-package 'auctex-latexmk)
(require-package 'cdlatex)
(require 'auctex)
(require 'cdlatex)
(require 'auctex-latexmk)
(turn-on-cdlatex)
;;(auctex-latexmk-setup)

;; use the AUCTeX mode instead of the origin mode
;; seems stupid,,,
(add-hook 'latex-mode-hook 'LaTeX-mode)

(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook (lambda () (flymake-mode -2)))

;; compile when save
(defun compile-and-save-latex ()
  (interactive)
  (save-buffer)
  (if (/= 0 (call-process "latexmk"
                          nil (get-buffer-create "*latex-compile*") nil
                          "-pdf" "--synctex=1" (buffer-file-name)))
      (progn
        (error "latex-compile-failed")
        )
    ))


(defun tex-to-pdf-path (p)
  (file-name-with-extension (file-name-sans-extension p) "pdf"))
(defun change-to-pdf-view ()
  (interactive)
  (let ((open-buffer (file-name-nondirectory (tex-to-pdf-path buffer-file-name))))
    (if (get-buffer open-buffer)
        (switch-to-buffer-other-window open-buffer)
      (find-file-other-window (tex-to-pdf-path buffer-file-name)) )
    ))
(defun set-down-to-pdf ()
  (interactive)
  (split-window-below)
  (change-to-pdf-view))
(defun set-right-to-pdf ()
  (interactive)
  (split-window-right)
  (change-to-pdf-view))
(defun set-this-to-pdf ()
  (interactive)
  (let ((open-buffer (file-name-nondirectory (tex-to-pdf-path buffer-file-name))))
    (if (get-buffer open-buffer)
        (switch-to-buffer open-buffer)
      (find-file (tex-to-pdf-path buffer-file-name)) )
    ))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") 'compile-and-save-latex)
            (local-set-key (kbd "C-c C-v 1") 'set-this-to-pdf)
            (local-set-key (kbd "C-c C-v 2") 'set-right-to-pdf)
            (local-set-key (kbd "<tab>") 'cdlatex-tab)))

;; from https://zhuanlan.zhihu.com/p/501582548
(load "auctex.el" nil t t)
;; 下面这行有时会导致bug，若出现问题请注掉
;; (load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; from https://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs
(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
(add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
(add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))

(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))) ; Automatically activate TeX-fold-mode.
(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; RefTeX
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
                                        ;     (setq reftex-default-bibliography '("UNCOMMENT LINE AND INSERT PATH TO YOUR BIBLIOGRAPHY HERE")); So that RefTeX in Org-mode knows bibliography
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see https://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))

;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
                                        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))


(provide 'custom-tex)
;;; custom-tex.el ends here
