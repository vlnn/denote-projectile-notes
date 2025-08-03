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

(defgroup denote-projectile-notes nil
  "Project-specific notes for Denote."
  :group 'denote
  :prefix "denote-projectile-")

;; Helper functions for project note paths
(defun denote-project-notes-directory (project-name)
  "Get the directory path for PROJECT-NAME notes."
  (expand-file-name (format "projects/%s" (denote-sluggify-title project-name))
                    denote-directory))

(defun denote-ensure-project-directory (project-name)
  "Ensure the directory for PROJECT-NAME exists."
  (let ((dir (denote-project-notes-directory project-name)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; Note creation functions
(defun denote-generate-project-note-filename (_project-name note-title)
  "Generate filename for a project note with PROJECT-NAME and NOTE-TITLE."
  (format "%s--%s__project.org"
          (format-time-string "%Y%m%dT%H%M%S")
          (denote-sluggify-title note-title)))

(defun denote-create-project-note-frontmatter (project-name note-title)
  "Create frontmatter for a project note."
  (format "#+startup: hidedrawers\n#+title: %s - %s\n#+filetags: :project:%s:\n#+date: %s\n\n"
          project-name
          note-title
          (denote-sluggify-title project-name)
          (format-time-string "%Y-%m-%d")))


;; Note discovery functions
(defun denote-list-project-notes (project-name)
  "List all note files for PROJECT-NAME."
  (let ((dir (denote-project-notes-directory project-name)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.org$"))))

(defun denote-extract-note-title (filepath)
  "Extract the note title from FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath nil 0 200)
    (if (re-search-forward "^#\\+title: .*? - \\(.+\\)$" nil t)
        (match-string 1)
      (file-name-base filepath))))

(defun denote-format-note-for-selection (filepath _project-name)
  "Format FILEPATH for selection display."
  (let* ((title (denote-extract-note-title filepath))
         (mtime (file-attribute-modification-time (file-attributes filepath)))
         (date (format-time-string "%Y-%m-%d" mtime)))
    (format "%-40s  [%s]" title date)))

;; Selection interface
(defun denote-select-project-note (project-name)
  "Select a note from PROJECT-NAME's notes."
  (let ((notes (denote-list-project-notes project-name)))
    (cond
     ((null notes) nil)
     ((= (length notes) 1) (car notes))
     (t (denote-prompt-for-note-selection notes project-name)))))

(defun denote-prompt-for-note-selection (notes project-name)
  "Prompt user to select from NOTES list."
  (let* ((choices (mapcar (lambda (note)
                            (cons (denote-format-note-for-selection note project-name)
                                  note))
                          notes))
         (selection (completing-read
                     (format "Select note for %s: " project-name)
                     choices nil t)))
    (cdr (assoc selection choices))))

;; New note creation
(defun denote-create-new-project-note (project-name)
  "Create a new note for PROJECT-NAME."
  (let* ((note-title (read-string "Note title: "))
         (dir (denote-ensure-project-directory project-name))
         (filename (denote-generate-project-note-filename project-name note-title))
         (filepath (expand-file-name filename dir)))
    (cons filepath note-title)))

;; Keybinding setup with proper guards
(defun denote-setup-base-keybindings ()
  "Setup base keybindings that work in all contexts."
  (local-set-key (kbd "q") #'denote-smart-close-popup)
  (local-set-key (kbd "<escape>") #'denote-smart-close-popup))

(defun denote-setup-evil-keybindings ()
  "Setup Evil-specific keybindings when Evil is available."
  (when (and (featurep 'evil)
             (fboundp 'evil-local-set-key)
             (bound-and-true-p evil-mode))
    (evil-local-set-key 'normal (kbd "q") #'denote-smart-close-popup)
    (evil-local-set-key 'normal (kbd "<escape>") #'denote-smart-close-popup)))

(defun denote-setup-popup-keybindings ()
  "Setup keybindings for popup buffer."
  (denote-setup-base-keybindings)
  (denote-setup-evil-keybindings))

(defun denote-setup-overview-base-keys ()
  "Setup base overview keybindings."
  (local-set-key (kbd "g") #'denote-refresh-overview)
  (local-set-key (kbd "RET") #'denote-open-note-at-point))

(defun denote-setup-overview-evil-keys ()
  "Setup Evil overview keybindings when available."
  (when (and (featurep 'evil)
             (fboundp 'evil-local-set-key)
             (bound-and-true-p evil-mode))
    (evil-local-set-key 'normal (kbd "g") #'denote-refresh-overview)
    (evil-local-set-key 'normal (kbd "RET") #'denote-open-note-at-point)))

(defun denote-setup-overview-keybindings ()
  "Setup all overview keybindings."
  (denote-setup-base-keybindings)
  (denote-setup-evil-keybindings)
  (denote-setup-overview-base-keys)
  (denote-setup-overview-evil-keys))

;; Popup management
(defun denote-smart-close-popup ()
  "Close popup or return to overview based on how it was opened."
  (interactive)
  (save-buffer)
  (if (bound-and-true-p denote-opened-from-overview)
      (denote-return-to-overview)
    (if (fboundp '+popup/close)
        (+popup/close)
      (quit-window))))

(defun denote-return-to-overview ()
  "Return to the overview that opened this note."
  (let ((project-name (bound-and-true-p denote-overview-project-name)))
    (if (fboundp '+popup/close)
        (+popup/close)
      (quit-window))
    (when project-name
      (when-let ((overview-path (denote-project-overview-filepath project-name)))
        (when-let ((existing-buffer (find-buffer-visiting overview-path)))
          (with-current-buffer existing-buffer
            (revert-buffer t t t))))
      (denote-open-project-overview project-name))))

(defun denote-save-and-close-popup ()
  "Save buffer and close popup."
  (interactive)
  (save-buffer)
  (if (fboundp '+popup/close)
      (+popup/close)
    (quit-window)))

(defun denote-open-project-popup (buffer)
  "Open BUFFER in a popup window and focus it."
  (if (fboundp '+popup-buffer)
      (let ((popup-window (+popup-buffer buffer '((side . right) (size . 0.4)))))
        (when popup-window
          (select-window popup-window)))
    (pop-to-buffer buffer)))

(defun denote-setup-project-note-buffer (buffer project-name note-title)
  "Setup BUFFER for a project note."
  (with-current-buffer buffer
    (when (= (point-max) 1)
      (insert (denote-create-project-note-frontmatter project-name note-title)))
    (denote-setup-popup-keybindings)
    (setq-local denote-overview-project-name project-name)))

;; Overview note functions
(defun denote-project-overview-filepath (project-name)
  "Get the overview note filepath for PROJECT-NAME."
  (expand-file-name
   (format "projects/%s-overview.org" (denote-sluggify-title project-name))
   denote-directory))

(defun denote-extract-note-content (filepath)
  "Extract content from note at FILEPATH, excluding frontmatter."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    ;; Skip frontmatter
    (while (looking-at "^#\\+")
      (forward-line))
    (skip-chars-forward "\n")
    (buffer-substring-no-properties (point) (point-max))))

(defun denote-get-note-metadata (filepath)
  "Extract metadata from note at FILEPATH."
  (let ((mtime (file-attribute-modification-time (file-attributes filepath))))
    (list :title (denote-extract-note-title filepath)
          :date (format-time-string "%Y-%m-%d %H:%M" mtime)
          :filepath filepath
          :mtime mtime)))

(defun denote-sort-notes-by-date (notes)
  "Sort NOTES by modification time, newest first."
  (sort notes (lambda (a b)
                (time-less-p (plist-get b :mtime)
                             (plist-get a :mtime)))))

(defun denote-demote-org-headers (content)
  "Demote all org headers in CONTENT by one level."
  (replace-regexp-in-string
   "^\\(\\*+\\) "
   "*\\1 "
   content))

(defun denote-format-note-section (metadata content)
  "Format a note section with METADATA and CONTENT."
  (format "* %s [%s]\n:PROPERTIES:\n:SOURCE: [[file:%s][Open]]\n:NOTE_ID: %s\n:END:\n\n%s\n"
          (plist-get metadata :title)
          (plist-get metadata :date)
          (plist-get metadata :filepath)
          (file-name-nondirectory (plist-get metadata :filepath))
          (denote-demote-org-headers (string-trim content))))

(defun denote-generate-overview-content (project-name)
  "Generate overview content for PROJECT-NAME."
  (let* ((notes (denote-list-project-notes project-name))
         (metadata-list (mapcar #'denote-get-note-metadata notes))
         (sorted-metadata (denote-sort-notes-by-date metadata-list))
         (header (denote-create-overview-header project-name))
         (sections (denote-create-overview-sections sorted-metadata)))
    (concat header sections)))

(defun denote-create-overview-header (project-name)
  "Create header for PROJECT-NAME overview."
  (format "#+title: %s - Overview\n#+filetags: :project:%s:overview:\n#+date: %s\n\nThis is an auto-generated overview of all notes for project: %s\nLast updated: %s\n\n"
          project-name
          (denote-sluggify-title project-name)
          (format-time-string "%Y-%m-%d")
          project-name
          (format-time-string "%Y-%m-%d %H:%M")))

(defun denote-create-overview-sections (metadata-list)
  "Create sections from METADATA-LIST."
  (mapconcat
   (lambda (metadata)
     (let ((content (denote-extract-note-content (plist-get metadata :filepath))))
       (denote-format-note-section metadata content)))
   metadata-list
   "\n"))

(defun denote-update-project-overview (project-name)
  "Update or create overview for PROJECT-NAME."
  (let ((filepath (denote-project-overview-filepath project-name))
        (content (denote-generate-overview-content project-name)))
    (with-temp-file filepath
      (insert content))
    filepath))

;; Main interactive functions
(defun denote-project-notes ()
  "Open or create a project note for current project."
  (interactive)
  (if-let ((project-name (and (fboundp 'projectile-project-name)
                              (projectile-project-name))))
      (denote-handle-project-note-selection project-name)
    (message "Not in a projectile project")))

(defun denote-handle-project-note-selection (project-name)
  "Handle note selection or creation for PROJECT-NAME."
  (let* ((existing-notes (denote-list-project-notes project-name))
         (choice (denote-prompt-note-action existing-notes)))
    (cond
     ((eq choice 'new)
      (denote-open-new-project-note project-name))
     ((eq choice 'overview)
      (denote-open-project-overview project-name))
     ((stringp choice)
      (denote-open-existing-project-note choice))
     (t (message "Cancelled")))))

(defun denote-prompt-note-action (existing-notes)
  "Prompt for action with EXISTING-NOTES."
  (if existing-notes
      (let ((choices (append '(("📊 View overview" . overview)
                               ("+ Create new note" . new))
                             (mapcar (lambda (note)
                                       (cons (denote-format-note-for-selection note nil)
                                             note))
                                     existing-notes))))
        (cdr (assoc (completing-read "Select note: " choices nil t) choices)))
    'new))

(defun denote-open-new-project-note (project-name)
  "Create and open a new note for PROJECT-NAME."
  (let* ((result (denote-create-new-project-note project-name))
         (filepath (car result))
         (note-title (cdr result))
         (buffer (find-file-noselect filepath)))
    (denote-setup-project-note-buffer buffer project-name note-title)
    (denote-open-project-popup buffer)))

(defun denote-open-existing-project-note (filepath)
  "Open existing note at FILEPATH."
  (let ((buffer (find-file-noselect filepath)))
    (with-current-buffer buffer
      (denote-setup-popup-keybindings)
      (unless (bound-and-true-p denote-overview-project-name)
        (when (string-match "/projects/\\([^/]+\\)/" filepath)
          (setq-local denote-overview-project-name
                      (denote-unslugify-project-name (match-string 1 filepath))))))
    (denote-open-project-popup buffer)))

(defun denote-open-project-overview (project-name)
  "Open or create overview for PROJECT-NAME."
  (let* ((filepath (denote-update-project-overview project-name))
         (buffer (find-file-noselect filepath)))
    (with-current-buffer buffer
      (denote-setup-overview-keybindings)
      (read-only-mode 1)
      (setq-local denote-overview-project-name project-name))
    (denote-open-project-popup buffer)))

;; Navigation functions for overview
(defun denote-find-note-id-at-point ()
  "Find the NOTE_ID property in the current section."
  (save-excursion
    (let ((_original-point (point)))
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
            (match-string 1)))))))

(defun denote-open-note-at-point ()
  "Open the note file corresponding to the current section."
  (interactive)
  (when-let* ((note-id (denote-find-note-id-at-point))
              (project-name (bound-and-true-p denote-overview-project-name))
              (filepath (denote-find-note-filepath project-name note-id)))
    (if (file-exists-p filepath)
        (let ((buffer (find-file-noselect filepath)))
          (with-current-buffer buffer
            (denote-setup-popup-keybindings)
            (setq-local denote-opened-from-overview t)
            (setq-local denote-overview-project-name project-name))
          (denote-open-project-popup buffer))
      (message "Note file not found: %s" note-id))))

(defun denote-find-note-filepath (project-name note-id)
  "Find full filepath for NOTE-ID in PROJECT-NAME."
  (expand-file-name note-id (denote-project-notes-directory project-name)))

(defun denote-refresh-overview ()
  "Refresh the current overview buffer."
  (interactive)
  (when (bound-and-true-p denote-overview-project-name)
    (let ((inhibit-read-only t)
          (point (point)))
      (erase-buffer)
      (insert (denote-generate-overview-content denote-overview-project-name))
      (goto-char (min point (point-max)))
      (message "Overview refreshed"))))

;; Hook to update overview when saving project notes
(defun denote-maybe-update-overview ()
  "Update overview if current buffer is a project note."
  (when (and (buffer-file-name)
             (string-match "/projects/\\([^/]+\\)/" (buffer-file-name))
             (not (string-match "-overview\\.org$" (buffer-file-name))))
    (let* ((project-slug (match-string 1 (buffer-file-name)))
           (project-name (denote-unslugify-project-name project-slug))
           (overview-path (denote-project-overview-filepath project-name)))
      (denote-update-project-overview project-name)
      (when-let ((overview-buffer (find-buffer-visiting overview-path)))
        (with-current-buffer overview-buffer
          (let ((inhibit-read-only t)
                (point (point)))
            (revert-buffer t t t)
            (goto-char (min point (point-max)))))))))

(defun denote-unslugify-project-name (slug)
  "Convert SLUG back to project name."
  (replace-regexp-in-string "-" " " slug))

(defun denote-create-project-note ()
  "Create a new project note directly without menu."
  (interactive)
  (if-let ((project-name (and (fboundp 'projectile-project-name)
                              (projectile-project-name))))
      (denote-open-new-project-note project-name)
    (message "Not in a projectile project")))

;; Setup hook
(add-hook 'after-save-hook #'denote-maybe-update-overview)

(provide 'denote-projectile-notes)
;;; denote-projectile-notes.el ends here
