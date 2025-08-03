;;; test-helper.el --- Test helper for denote-projectile-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides stubs for external dependencies to allow tests to run without
;; requiring actual packages to be installed.

;;; Code:

;; Provide the features that will be required
(provide 'denote)
(provide 'projectile)

;; Stub for denote functions
(defvar denote-directory "~/org/denote/"
  "Default directory for notes.")

(defun denote-sluggify-title (title)
  "Convert TITLE to a slug format."
  (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title)))

;; Stub for projectile functions
(defun projectile-project-name ()
  "Return a test project name."
  "test-project")

;; Stub for Evil if needed
(unless (fboundp 'evil-define-key)
  (defmacro evil-define-key (state keymap &rest bindings)
    "Stub for evil-define-key."
    `(progn
       ,@(cl-loop for (key fn) on bindings by #'cddr
                  collect `(define-key ,keymap ,key ,fn)))))

(unless (fboundp 'evil-local-set-key)
  (defun evil-local-set-key (state key def)
    "Stub for evil-local-set-key."
    (local-set-key key def)))

;; Stub for Doom popup functions
(unless (fboundp '+popup-buffer)
  (defun +popup-buffer (buffer &optional alist)
    "Stub for Doom's popup-buffer."
    (pop-to-buffer buffer)))

(unless (fboundp '+popup/close)
  (defun +popup/close ()
    "Stub for Doom's popup close."
    (quit-window)))

(provide 'test-helper)

;;; test-helper.el ends here

