;;; denote-projectile-notes.el --- Project-specific notes for Denote using Projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2025 vlnn

;; Author: Volodymyr Anokhin <vlnn-github@proton.me>
;; URL: https://github.com/vlnn/denote-projectile-notes
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (denote "2.0.0") (projectile "2.7.0"))
;; Keywords: convenience, files, notes, project

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Denote with Projectile to provide seamless
;; project-specific note management.
;;
;; Features:
;; - Organize notes by project in dedicated subdirectories
;; - Auto-generated overview of all project notes
;; - Quick navigation between notes
;; - Support for multiple notes per project
;; - Integration with projectile project detection
;;
;; Usage:
;; - M-x denote-projectile-notes (or bind to a key)
;; - Select "View overview" to see all notes
;; - Press RET on any note to open it
;; - Create new notes directly with denote-projectile-notes-new
;;
;; Configuration:
;; (use-package denote-projectile-notes
;;   :after (denote projectile)
;;   :bind (:map projectile-command-map
;;          ("n" . denote-projectile-notes)
;;          ("N" . denote-projectile-notes-new)))

;;; Code:

(require 'denote)
(require 'projectile)
(require 'cl-lib)

;;; Custom Variables

(defgroup denote-projectile-notes nil
  "Project-specific notes for Denote."
  :group 'denote
  :prefix "denote-projectile-notes-")

(defcustom denote-projectile-notes-subdirectory "projects/"
  "Subdirectory within `denote-directory' for project notes."
  :type 'string
  :group 'denote-projectile-notes)

(defcustom denote-projectile-notes-overview-auto-update t
  "Whether to automatically update overview when saving project notes."
  :type 'boolean
  :group 'denote-projectile-notes)

(defcustom denote-projectile-notes-overview-filename-suffix "-overview"
  "Suffix for overview files."
  :type 'string
  :group 'denote-projectile-notes)

(defcustom denote-projectile-notes-popup-side 'right
  "Side where popup window appears."
  :type '(choice (const left)
          (const right)
          (const top)
          (const bottom))
  :group 'denote-projectile-notes)

(defcustom denote-projectile-notes-popup-size 0.4
  "Size of the popup window (fraction of frame)."
  :type 'number
  :group 'denote-projectile-notes)

;;; Variables

(defvar-local denote-projectile-notes--opened-from-overview nil
  "Whether current note was opened from overview.")

(defvar-local denote-projectile-notes--project-name nil
  "Project name associated with current buffer.")

;;; Core Functions

(defun denote-projectile-notes--directory (project-name)
  "Get the directory path for PROJECT-NAME notes."
  (expand-file-name
   (concat denote-projectile-notes-subdirectory
           (denote-sluggify-title project-name))
   denote-directory))

(defun denote-projectile-notes--ensure-directory (project-name)
  "Ensure the directory for PROJECT-NAME exists."
  (let ((dir (denote-projectile-notes--directory project-name)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun denote-projectile-notes--list-notes (project-name)
  "List all note files for PROJECT-NAME."
  (let ((dir (denote-projectile-notes--directory project-name)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.org$"))))

(defun denote-projectile-notes--generate-filename (_project-name note-title)
  "Generate filename for a project note with PROJECT-NAME and NOTE-TITLE."
  (format "%s--%s__project.org"
          (format-time-string "%Y%m%dT%H%M%S")
          (denote-sluggify-title note-title)))

(defun denote-projectile-notes--create-frontmatter (project-name note-title)
  "Create frontmatter for a project note."
  (format "#+title: %s - %s\n#+filetags: :project:%s:\n#+date: %s\n\n"
          project-name
          note-title
          (denote-sluggify-title project-name)
          (format-time-string "%Y-%m-%d")))

;;; Overview Functions

(defun denote-projectile-notes--overview-filepath (project-name)
  "Get the overview note filepath for PROJECT-NAME."
  (expand-file-name
   (format "%s%s%s.org"
           denote-projectile-notes-subdirectory
           (denote-sluggify-title project-name)
           denote-projectile-notes-overview-filename-suffix)
   denote-directory))

(defun denote-projectile-notes--extract-note-title (filepath)
  "Extract the note title from FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath nil 0 200)
    (if (re-search-forward "^#\\+title: .*? - \\(.+\\)$" nil t)
        (match-string 1)
      (file-name-base filepath))))

(defun denote-projectile-notes--extract-note-content (filepath)
  "Extract content from note at FILEPATH, excluding frontmatter."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (while (looking-at "^#\\+")
      (forward-line))
    (skip-chars-forward "\n")
    (buffer-substring-no-properties (point) (point-max))))

(defun denote-projectile-notes--get-note-metadata (filepath)
  "Extract metadata from note at FILEPATH."
  (let ((mtime (file-attribute-modification-time (file-attributes filepath))))
    (list :title (denote-projectile-notes--extract-note-title filepath)
          :date (format-time-string "%Y-%m-%d %H:%M" mtime)
          :filepath filepath
          :mtime mtime)))

(defun denote-projectile-notes--sort-notes-by-date (notes)
  "Sort NOTES by modification time, newest first."
  (sort notes (lambda (a b)
                (time-less-p (plist-get b :mtime)
                             (plist-get a :mtime)))))

(defun denote-projectile-notes--demote-org-headers (content)
  "Demote all org headers in CONTENT by one level."
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " content))

(defun denote-projectile-notes--format-note-section (metadata content)
  "Format a note section with METADATA and CONTENT."
  (format "* %s [%s]\n:PROPERTIES:\n:SOURCE: [[file:%s][Open]]\n:NOTE_ID: %s\n:END:\n\n%s\n"
          (plist-get metadata :title)
          (plist-get metadata :date)
          (plist-get metadata :filepath)
          (file-name-nondirectory (plist-get metadata :filepath))
          (denote-projectile-notes--demote-org-headers (string-trim content))))

(defun denote-projectile-notes--generate-overview-content (project-name)
  "Generate overview content for PROJECT-NAME."
  (let* ((notes (denote-projectile-notes--list-notes project-name))
         (metadata-list (mapcar #'denote-projectile-notes--get-note-metadata notes))
         (sorted-metadata (denote-projectile-notes--sort-notes-by-date metadata-list)))
    (concat
     (format "#+title: %s - Overview\n#+filetags: :project:%s:overview:\n#+date: %s\n\n"
             project-name
             (denote-sluggify-title project-name)
             (format-time-string "%Y-%m-%d"))
     (format "This is an auto-generated overview of all notes for project: %s\n"
             project-name)
     (format "Last updated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M"))
     (mapconcat
      (lambda (metadata)
        (let ((content (denote-projectile-notes--extract-note-content
                        (plist-get metadata :filepath))))
          (denote-projectile-notes--format-note-section metadata content)))
      sorted-metadata
      "\n"))))

(defun denote-projectile-notes--update-overview (project-name)
  "Update or create overview for PROJECT-NAME."
  (let ((filepath (denote-projectile-notes--overview-filepath project-name))
        (content (denote-projectile-notes--generate-overview-content project-name)))
    (with-temp-file filepath
      (insert content))
    filepath))

;;; Navigation Functions

(defun denote-projectile-notes--find-note-id-at-point ()
  "Find the NOTE_ID property in the current section."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^\\* ")
      (re-search-backward "^\\* " nil t))
    (let ((section-start (point)))
      (forward-line 1)
      (let ((section-end (if (re-search-forward "^\\* " nil t)
                             (match-beginning 0)
                           (point-max))))
        (goto-char section-start)
        (when (re-search-forward "^:NOTE_ID: \\(.+\\)$" section-end t)
          (match-string 1))))))

;;; Interactive Functions

(defun denote-projectile-notes--format-note-for-selection (filepath _project-name)
  "Format FILEPATH for selection display."
  (let* ((title (denote-projectile-notes--extract-note-title filepath))
         (mtime (file-attribute-modification-time (file-attributes filepath)))
         (date (format-time-string "%Y-%m-%d" mtime)))
    (format "%-40s  [%s]" title date)))

(defun denote-projectile-notes--select-action (existing-notes project-name)
  "Prompt for action with EXISTING-NOTES."
  (let ((choices (if existing-notes
                     (append '(("📊 View overview" . overview)
                               ("+ Create new note" . new))
                             (mapcar (lambda (note)
                                       (cons (denote-projectile-notes--format-note-for-selection
                                              note project-name)
                                             note))
                                     existing-notes))
                   '(("+ Create new note" . new)))))
    (cdr (assoc (completing-read "Select note: " choices nil t) choices))))

;;;###autoload
(defun denote-projectile-notes ()
  "Open or create a project note for current project."
  (interactive)
  (if-let ((project-name (projectile-project-name)))
      (let* ((existing-notes (denote-projectile-notes--list-notes project-name))
             (choice (denote-projectile-notes--select-action existing-notes project-name)))
        (cond
         ((eq choice 'new)
          (denote-projectile-notes-new))
         ((eq choice 'overview)
          (denote-projectile-notes-overview))
         ((stringp choice)
          (denote-projectile-notes--open-note choice))
         (t (message "Cancelled"))))
    (message "Not in a projectile project")))

;;;###autoload
(defun denote-projectile-notes-new ()
  "Create a new project note directly."
  (interactive)
  (if-let ((project-name (projectile-project-name)))
      (let* ((note-title (read-string "Note title: "))
             (dir (denote-projectile-notes--ensure-directory project-name))
             (filename (denote-projectile-notes--generate-filename project-name note-title))
             (filepath (expand-file-name filename dir))
             (content (denote-projectile-notes--create-frontmatter project-name note-title)))
        (with-temp-file filepath
          (insert content))
        (denote-projectile-notes--open-note filepath t))
    (message "Not in a projectile project")))

;;;###autoload
(defun denote-projectile-notes-overview ()
  "Open overview for current project."
  (interactive)
  (if-let ((project-name (projectile-project-name)))
      (let ((filepath (denote-projectile-notes--update-overview project-name)))
        (denote-projectile-notes--open-overview filepath project-name))
    (message "Not in a projectile project")))

;;; Buffer Management

(defun denote-projectile-notes--open-note (filepath &optional new-note _note-title)
  "Open note at FILEPATH."
  (let ((buffer (find-file-noselect filepath)))
    (with-current-buffer buffer
      (when new-note
        (goto-char (point-max)))
      (denote-projectile-notes-mode 1))
    (denote-projectile-notes--display-popup buffer)))

(defun denote-projectile-notes--open-overview (filepath project-name)
  "Open overview at FILEPATH for PROJECT-NAME."
  (let ((buffer (find-file-noselect filepath)))
    (with-current-buffer buffer
      (denote-projectile-notes-overview-mode)
      (setq-local denote-projectile-notes--project-name project-name))
    (denote-projectile-notes--display-popup buffer)))

(defun denote-projectile-notes--display-popup (buffer)
  "Display BUFFER in popup window."
  (if (fboundp '+popup-buffer)
      ;; Doom Emacs style
      (let ((window (+popup-buffer buffer
                                   `((side . ,denote-projectile-notes-popup-side)
                                     (size . ,denote-projectile-notes-popup-size)))))
        (when window (select-window window)))
    ;; Fallback to standard display
    (pop-to-buffer buffer)))

;;; Minor Mode

(defvar denote-projectile-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal map
        "q" #'denote-projectile-notes-close
        (kbd "<escape>") #'denote-projectile-notes-close))
    map)
  "Keymap for `denote-projectile-notes-mode'.")

(define-minor-mode denote-projectile-notes-mode
  "Minor mode for project notes."
  :lighter " DPN"
  :keymap denote-projectile-notes-mode-map
  (when denote-projectile-notes-mode
    (when (string-match "/projects/\\([^/]+\\)/" (buffer-file-name))
      (setq-local denote-projectile-notes--project-name
                  (denote-projectile-notes--unslugify
                   (match-string 1 (buffer-file-name)))))))

(defun denote-projectile-notes-close ()
  "Close current note, returning to overview if applicable."
  (interactive)
  (save-buffer)
  (if denote-projectile-notes--opened-from-overview
      (denote-projectile-notes-overview)
    (if (fboundp '+popup/close)
        (+popup/close)
      (quit-window))))

;;; Overview Mode

(defvar denote-projectile-notes-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'denote-projectile-notes-open-at-point)
    (define-key map (kbd "g") #'denote-projectile-notes-refresh-overview)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal map
        (kbd "RET") #'denote-projectile-notes-open-at-point
        "g" #'denote-projectile-notes-refresh-overview
        "q" #'denote-projectile-notes-close
        (kbd "<escape>") #'denote-projectile-notes-close))
    map)
  "Keymap for `denote-projectile-notes-overview-mode'.")

(define-derived-mode denote-projectile-notes-overview-mode org-mode "DPN-Overview"
  "Major mode for project note overviews."
  (read-only-mode 1))

(defun denote-projectile-notes-open-at-point ()
  "Open the note at point."
  (interactive)
  (when-let* ((note-id (denote-projectile-notes--find-note-id-at-point))
              (project-name denote-projectile-notes--project-name)
              (filepath (expand-file-name note-id
                                          (denote-projectile-notes--directory project-name))))
    (if (file-exists-p filepath)
        (let ((buffer (find-file-noselect filepath)))
          (with-current-buffer buffer
            (denote-projectile-notes-mode 1)
            (setq-local denote-projectile-notes--opened-from-overview t)
            (setq-local denote-projectile-notes--project-name project-name))
          (denote-projectile-notes--display-popup buffer))
      (message "Note file not found: %s" note-id))))

(defun denote-projectile-notes-refresh-overview ()
  "Refresh the current overview."
  (interactive)
  (when denote-projectile-notes--project-name
    (let ((inhibit-read-only t)
          (point (point)))
      (erase-buffer)
      (insert (denote-projectile-notes--generate-overview-content
               denote-projectile-notes--project-name))
      (goto-char (min point (point-max)))
      (message "Overview refreshed"))))

;;; Auto-update Hook

(defun denote-projectile-notes--maybe-update-overview ()
  "Update overview if current buffer is a project note."
  (when (and denote-projectile-notes-overview-auto-update
             (buffer-file-name)
             (string-match "/projects/\\([^/]+\\)/" (buffer-file-name))
             (not (string-match "-overview\\.org$" (buffer-file-name))))
    (let* ((project-slug (match-string 1 (buffer-file-name)))
           (project-name (denote-projectile-notes--unslugify project-slug))
           (overview-path (denote-projectile-notes--overview-filepath project-name)))
      (denote-projectile-notes--update-overview project-name)
      (when-let ((overview-buffer (find-buffer-visiting overview-path)))
        (with-current-buffer overview-buffer
          (let ((inhibit-read-only t))
            (revert-buffer t t t)))))))

(add-hook 'after-save-hook #'denote-projectile-notes--maybe-update-overview)

;;; Utility Functions

(defun denote-projectile-notes--unslugify (slug)
  "Convert SLUG back to normal text."
  (replace-regexp-in-string "-" " " slug))

;;; Provide

(provide 'denote-projectile-notes)

;;; denote-projectile-notes.el ends here
