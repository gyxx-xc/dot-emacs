;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'desktop+)
(require 'desktop+)

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(defvar mystic/last-desktop-filename ".last-desktop.el")

(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 30)
(setq desktop-path (list user-emacs-directory))
(if (file-exists-p (expand-file-name mystic/last-desktop-filename desktop+-base-dir))
    (load (expand-file-name mystic/last-desktop-filename desktop+-base-dir)))
(desktop-save-mode 1)

(global-set-key (kbd "C-x C-d") 'desktop+-load-or-create)


;; borrow from https://github.com/jimeh/.emacs.d/blob/master/modules/workspaces/siren-desktop.el
(defun siren-desktop+--list-filter-item (path)
  (let ((basename (file-name-nondirectory path))
        (is-dir (car (file-attributes path))))
    (if (and is-dir (not (member basename '("." ".."))))
        (file-name-nondirectory path))))
(defun siren-desktop+-list ()
  "Return a list of available desktops"
  (remove nil (mapcar 'siren-desktop+--list-filter-item
                      (directory-files desktop+-base-dir t))))
(defun siren-desktop+-current-desktop ()
  (when (and (boundp 'desktop-dirname) desktop-dirname)
    (let ((dir (directory-file-name desktop-dirname))
          (base-dir (expand-file-name desktop+-base-dir)))
      (when (string-prefix-p base-dir dir)
        (file-name-nondirectory dir)))))
(defun siren-desktop+-list-interactive ()
  (let ((current (siren-desktop+-current-desktop))
        (desktops (siren-desktop+-list)))
    (if current
        (append (list current)
                (delete current desktops))
      desktops)))
(defun desktop+-load-or-create (name)
  "Load or create a desktop session by name."
  (interactive
   (list
    (completing-read "Desktop name: " (siren-desktop+-list-interactive))))
  (if (member name (siren-desktop+-list))
      (if (not (string= name (siren-desktop+-current-desktop)))
          (desktop+-load name))
    (desktop+-create name)))

(defun mystic/empty-desktop (IGNORE)
  (desktop-clear))
(advice-add 'desktop+-create :after 'mystic/empty-desktop)

(defun mystic/set-defult-desktop (dir)
  (setq desktop-path (list dir))
  )
;; (advice-add 'desktop-change-dir :after 'mystic/set-defult-desktop)

(defun mystic/store-defult-desktop (return restart)
  (write-region (format "(setq desktop-path (list \"%s\"))\n"
                        (desktop+--dirname (siren-desktop+-current-desktop)))
                nil (expand-file-name mystic/last-desktop-filename desktop+-base-dir))
  )
(advice-add 'kill-emacs :before 'mystic/store-defult-desktop)



(defun sanityinc/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

(defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)


;; Restore histories and registers after saving

(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(require-package 'session)
(setq session-save-file (locate-user-emacs-file ".session"))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))


;; TODO: modify desktop-buffer-mode-handlers to restore shell buffers

(defun mystic/eaf-mode-hook ()
  (setq desktop-save-buffer #'mystic/eaf-save-buffer))
(defun mystic/eaf-save-buffer (dirname)
  "Save eaf-pdf-viewer buffer."
  (list :url (eaf-get-path-or-url)
        :app-name eaf--buffer-app-name
        :args eaf--buffer-args)
  )
(defun mystic/eaf-restore-buffer (file-name buffer-name misc)
  "Restore eaf-pdf-viewer buffer."
  (eaf-open (plist-get misc :url))
  (rename-buffer buffer-name)
  )

;; (add-hook 'eaf-mode-hook 'mystic/eaf-mode-hook)
;; (add-to-list 'desktop-buffer-mode-handlers '(eaf-mode . mystic/eaf-restore-buffer))


;;(save-place-mode 1)

(provide 'init-sessions)
;;; init-sessions.el ends here
