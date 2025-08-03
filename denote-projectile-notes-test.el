;;; denote-projectile-notes-test.el --- Tests for denote-projectile-notes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Volodymyr Anokhin

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test suite for denote-projectile-notes package.

;;; Code:

(require 'ert)
(require 'denote-projectile-notes)

;;; Test Helpers

(defvar denote-projectile-notes-test-dir nil
  "Temporary directory for tests.")

(defmacro denote-projectile-notes-test-with-temp-dir (&rest body)
  "Execute BODY with a temporary test directory."
  `(let* ((denote-projectile-notes-test-dir (make-temp-file "dpn-test-" t))
          (denote-directory denote-projectile-notes-test-dir)
          (default-directory denote-projectile-notes-test-dir))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p denote-projectile-notes-test-dir)
         (delete-directory denote-projectile-notes-test-dir t)))))

(defun denote-projectile-notes-test-create-note (project-name title content)
  "Create a test note for PROJECT-NAME with TITLE and CONTENT."
  (let* ((dir (denote-projectile-notes--ensure-directory project-name))
         (filename (denote-projectile-notes--generate-filename project-name title))
         (filepath (expand-file-name filename dir)))
    (with-temp-file filepath
      (insert (denote-projectile-notes--create-frontmatter project-name title))
      (insert content))
    filepath))

;;; Core Function Tests

(ert-deftest denote-projectile-notes-test-directory ()
  "Test directory path generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "My Test Project"))
     (should (string-match-p "projects/my-test-project$"
                             (denote-projectile-notes--directory project-name))))))

(ert-deftest denote-projectile-notes-test-ensure-directory ()
  "Test directory creation."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((project-name "Test Project")
          (dir (denote-projectile-notes--ensure-directory project-name)))
     (should (file-directory-p dir))
     (should (string-match-p "test-project$" dir)))))

(ert-deftest denote-projectile-notes-test-generate-filename ()
  "Test filename generation."
  (let ((filename (denote-projectile-notes--generate-filename "Project" "My Note")))
    (should (string-match-p "^[0-9T]+--my-note__project\\.org$" filename))))

(ert-deftest denote-projectile-notes-test-create-frontmatter ()
  "Test frontmatter creation."
  (let ((frontmatter (denote-projectile-notes--create-frontmatter "Test Project" "Note Title")))
    (should (string-match-p "^#\\+title: Test Project - Note Title" frontmatter))
    (should (string-match-p "#\\+filetags: :project:test-project:" frontmatter))
    (should (string-match-p "#\\+date: [0-9-]+" frontmatter))))

;;; Note Management Tests

(ert-deftest denote-projectile-notes-test-list-notes ()
  "Test listing project notes."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     ;; Initially no notes
     (should (null (denote-projectile-notes--list-notes project-name)))

     ;; Create some notes
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "Content 2")

     ;; Should have 2 notes now
     (let ((notes (denote-projectile-notes--list-notes project-name)))
       (should (= 2 (length notes)))
       (should (cl-every (lambda (f) (string-match-p "\\.org$" f)) notes))))))

(ert-deftest denote-projectile-notes-test-extract-note-title ()
  "Test extracting note title."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Test Note" "Some content")))
     (should (equal "Test Note"
                    (denote-projectile-notes--extract-note-title filepath))))))

(ert-deftest denote-projectile-notes-test-extract-note-content ()
  "Test extracting note content."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((content "This is the note content\n\nWith multiple lines")
          (filepath (denote-projectile-notes-test-create-note
                     "Project" "Note" content)))
     (should (string-match-p "This is the note content"
                             (denote-projectile-notes--extract-note-content filepath))))))

;;; Overview Tests

(ert-deftest denote-projectile-notes-test-overview-filepath ()
  "Test overview filepath generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((filepath (denote-projectile-notes--overview-filepath "Test Project")))
     (should (string-match-p "projects/test-project-overview\\.org$" filepath)))))

(ert-deftest denote-projectile-notes-test-demote-org-headers ()
  "Test org header demotion."
  (should (equal "** Heading"
                 (denote-projectile-notes--demote-org-headers "* Heading")))
  (should (equal "*** Subheading"
                 (denote-projectile-notes--demote-org-headers "** Subheading")))
  (should (equal "Regular text"
                 (denote-projectile-notes--demote-org-headers "Regular text"))))

(ert-deftest denote-projectile-notes-test-generate-overview ()
  "Test overview generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     ;; Create test notes
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "* Header\nContent 2")

     ;; Generate overview
     (let ((overview (denote-projectile-notes--generate-overview-content project-name)))
       (should (string-match-p "Test Project - Overview" overview))
       (should (string-match-p "Note 1" overview))
       (should (string-match-p "Note 2" overview))
       (should (string-match-p "\\*\\* Header" overview)) ; Demoted header
       (should (string-match-p ":NOTE_ID:" overview))))))

;;; Metadata Tests

(ert-deftest denote-projectile-notes-test-get-note-metadata ()
  "Test metadata extraction."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Test Note" "Content"))
          (metadata (denote-projectile-notes--get-note-metadata filepath)))
     (should (equal "Test Note" (plist-get metadata :title)))
     (should (string-match-p "[0-9-]+ [0-9:]+" (plist-get metadata :date)))
     (should (equal filepath (plist-get metadata :filepath)))
     (should (plist-get metadata :mtime)))))

(ert-deftest denote-projectile-notes-test-sort-notes-by-date ()
  "Test note sorting by date."
  (let* ((now (current-time))
         (earlier (time-subtract now 3600))
         (notes (list (list :mtime earlier :title "Old")
                      (list :mtime now :title "New"))))
    (let ((sorted (denote-projectile-notes--sort-notes-by-date notes)))
      (should (equal "New" (plist-get (car sorted) :title)))
      (should (equal "Old" (plist-get (cadr sorted) :title))))))

;;; Navigation Tests

(ert-deftest denote-projectile-notes-test-find-note-id-at-point ()
  "Test finding note ID at point."
  (with-temp-buffer
    (insert "* Note Title [2024-01-01]\n")
    (insert ":PROPERTIES:\n")
    (insert ":SOURCE: [[file:/path/to/note.org][Open]]\n")
    (insert ":NOTE_ID: 20240101T120000--my-note__project.org\n")
    (insert ":END:\n\n")
    (insert "Note content here\n")
    (goto-char (point-min))
    (forward-line 5) ; Move into content
    (should (equal "20240101T120000--my-note__project.org"
                   (denote-projectile-notes--find-note-id-at-point)))))

;;; Utility Tests

(ert-deftest denote-projectile-notes-test-unslugify ()
  "Test slug conversion."
  (should (equal "test project"
                 (denote-projectile-notes--unslugify "test-project")))
  (should (equal "multi word title"
                 (denote-projectile-notes--unslugify "multi-word-title"))))

(provide 'denote-projectile-notes-test)

;;; denote-projectile-notes-test.el ends here
