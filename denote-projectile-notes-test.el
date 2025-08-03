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
  (let* ((dir (denote-ensure-project-directory project-name))
         (filename (denote-generate-project-note-filename project-name title))
         (filepath (expand-file-name filename dir)))
    (with-temp-file filepath
      (insert (denote-create-project-note-frontmatter project-name title))
      (insert content))
    filepath))

;;; Core Function Tests

(ert-deftest denote-projectile-notes-test-directory ()
  "Test directory path generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "My Test Project"))
     (should (string-match-p "projects/my-test-project$"
                             (denote-project-notes-directory project-name))))))

(ert-deftest denote-projectile-notes-test-ensure-directory ()
  "Test directory creation."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((project-name "Test Project")
          (dir (denote-ensure-project-directory project-name)))
     (should (file-directory-p dir))
     (should (string-match-p "test-project$" dir)))))

(ert-deftest denote-projectile-notes-test-generate-filename ()
  "Test filename generation."
  (let ((filename (denote-generate-project-note-filename "Project" "My Note")))
    (should (string-match-p "^[0-9T]+--my-note__project\\.org$" filename))))

(ert-deftest denote-projectile-notes-test-create-frontmatter ()
  "Test frontmatter creation."
  (let ((frontmatter (denote-create-project-note-frontmatter "Test Project" "Note Title")))
    (should (string-match-p "^#\\+startup: hidedrawers" frontmatter))
    (should (string-match-p "^#\\+title: Test Project - Note Title" frontmatter))
    (should (string-match-p "^#\\+filetags: :project:test-project:" frontmatter))
    (should (string-match-p "^#\\+date: [0-9-]+" frontmatter))))

;;; Note Management Tests

(ert-deftest denote-projectile-notes-test-list-notes ()
  "Test listing project notes."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (should (null (denote-list-project-notes project-name)))

     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "Content 2")

     (let ((notes (denote-list-project-notes project-name)))
       (should (= 2 (length notes)))
       (should (cl-every (lambda (f) (string-match-p "\\.org$" f)) notes))))))

(ert-deftest denote-projectile-notes-test-extract-note-title ()
  "Test extracting note title."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Test Note" "Some content")))
     (should (equal "Test Note"
                    (denote-extract-note-title filepath))))))

(ert-deftest denote-projectile-notes-test-extract-note-title-fallback ()
  "Test extracting note title falls back to filename."
  (denote-projectile-notes-test-with-temp-dir
   (let ((filepath (expand-file-name "test-note.org" denote-directory)))
     (with-temp-file filepath
       (insert "No title header here"))
     (should (equal "test-note"
                    (denote-extract-note-title filepath))))))

(ert-deftest denote-projectile-notes-test-extract-note-content ()
  "Test extracting note content."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((content "This is the note content\n\nWith multiple lines")
          (filepath (denote-projectile-notes-test-create-note
                     "Project" "Note" content)))
     (should (string-match-p "This is the note content"
                             (denote-extract-note-content filepath))))))

(ert-deftest denote-projectile-notes-test-format-note-selection ()
  "Test note formatting for selection."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Test Note" "Content"))
          (formatted (denote-format-note-for-selection filepath "Project")))
     (should (string-match-p "Test Note" formatted))
     (should (string-match-p "\\[20[0-9-]+\\]" formatted)))))

;;; Overview Tests

(ert-deftest denote-projectile-notes-test-overview-filepath ()
  "Test overview filepath generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((filepath (denote-project-overview-filepath "Test Project")))
     (should (string-match-p "projects/test-project-overview\\.org$" filepath)))))

(ert-deftest denote-projectile-notes-test-demote-org-headers ()
  "Test org header demotion."
  (should (equal "** Heading"
                 (denote-demote-org-headers "* Heading")))
  (should (equal "*** Subheading"
                 (denote-demote-org-headers "** Subheading")))
  (should (equal "**** Deep heading"
                 (denote-demote-org-headers "*** Deep heading")))
  (should (equal "Regular text"
                 (denote-demote-org-headers "Regular text"))))

(ert-deftest denote-projectile-notes-test-generate-overview ()
  "Test overview generation."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "* Header\nContent 2")

     (let ((overview (denote-generate-overview-content project-name)))
       (should (string-match-p "Test Project - Overview" overview))
       (should (string-match-p "Note 1" overview))
       (should (string-match-p "Note 2" overview))
       (should (string-match-p "\\*\\* Header" overview))
       (should (string-match-p ":NOTE_ID:" overview))))))

(ert-deftest denote-projectile-notes-test-create-overview-header ()
  "Test overview header creation."
  (let ((header (denote-create-overview-header "Test Project")))
    (should (string-match-p "^#\\+title: Test Project - Overview" header))
    (should (string-match-p "#\\+filetags: :project:test-project:overview:" header))
    (should (string-match-p "auto-generated overview" header))))

(ert-deftest denote-projectile-notes-test-update-overview ()
  "Test overview update."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content")

     (let ((filepath (denote-update-project-overview project-name)))
       (should (file-exists-p filepath))
       (with-temp-buffer
         (insert-file-contents filepath)
         (should (string-match-p "Test Project - Overview" (buffer-string)))
         (should (string-match-p "Note 1" (buffer-string))))))))

;;; Metadata Tests

(ert-deftest denote-projectile-notes-test-get-note-metadata ()
  "Test metadata extraction."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Test Note" "Content"))
          (metadata (denote-get-note-metadata filepath)))
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
    (let ((sorted (denote-sort-notes-by-date notes)))
      (should (equal "New" (plist-get (car sorted) :title)))
      (should (equal "Old" (plist-get (cadr sorted) :title))))))

(ert-deftest denote-projectile-notes-test-format-note-section ()
  "Test note section formatting."
  (let* ((metadata (list :title "Test Note"
                         :date "2024-01-01 12:00"
                         :filepath "/path/to/note.org"))
         (content "Some content")
         (section (denote-format-note-section metadata content)))
    (should (string-match-p "\\* Test Note \\[2024-01-01 12:00\\]" section))
    (should (string-match-p ":SOURCE: \\[\\[file:/path/to/note.org\\]" section))
    (should (string-match-p ":NOTE_ID: note.org" section))
    (should (string-match-p "Some content" section))))

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
    (forward-line 5)
    (should (equal "20240101T120000--my-note__project.org"
                   (denote-find-note-id-at-point)))))

(ert-deftest denote-projectile-notes-test-find-note-id-no-match ()
  "Test finding note ID when none exists."
  (with-temp-buffer
    (insert "Just some text without properties")
    (goto-char (point-min))
    (should (null (denote-find-note-id-at-point)))))

(ert-deftest denote-projectile-notes-test-find-note-filepath ()
  "Test finding note filepath."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((project-name "Test Project")
          (note-id "test-note.org")
          (expected-path (expand-file-name
                          note-id
                          (denote-project-notes-directory project-name)))
          (actual-path (denote-find-note-filepath project-name note-id)))
     (should (equal expected-path actual-path)))))

;;; Utility Tests

(ert-deftest denote-projectile-notes-test-unslugify ()
  "Test slug conversion."
  (should (equal "test project"
                 (denote-unslugify-project-name "test-project")))
  (should (equal "multi word title"
                 (denote-unslugify-project-name "multi-word-title")))
  (should (equal "single"
                 (denote-unslugify-project-name "single"))))

;;; Integration Tests

(ert-deftest denote-projectile-notes-test-create-new-project-note ()
  "Test creating new project note."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((project-name "Test Project"))
     ;; Mock read-string to avoid interactive input
     (cl-letf (((symbol-function 'read-string)
                (lambda (prompt) "Test Note Title")))
       (let ((result (denote-create-new-project-note project-name)))
         (should (consp result))
         (should (stringp (car result)))  ; filepath
         (should (stringp (cdr result)))  ; title
         (should (equal "Test Note Title" (cdr result)))
         (should (string-match-p "test-note-title__project\\.org$" (car result)))
         ;; Directory should exist even if file doesn't
         (should (file-directory-p (file-name-directory (car result)))))))))

(ert-deftest denote-projectile-notes-test-select-project-note-single ()
  "Test selecting project note when only one exists."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (let ((filepath (denote-projectile-notes-test-create-note
                      project-name "Single Note" "Content")))
       (should (equal filepath (denote-select-project-note project-name)))))))

(ert-deftest denote-projectile-notes-test-select-project-note-none ()
  "Test selecting project note when none exist."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Empty Project"))
     (should (null (denote-select-project-note project-name))))))

;;; Additional valuable tests to add

;; Core workflow tests
(ert-deftest denote-projectile-notes-test-prompt-note-action-with-notes ()
  "Test note action prompting when notes exist."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "Content 2")

     (let ((notes (denote-list-project-notes project-name)))
       ;; Mock completing-read to return "📊 View overview"
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (prompt choices &rest _)
                    "📊 View overview")))
         (should (eq 'overview (denote-prompt-note-action notes))))

       ;; Mock completing-read to return "+ Create new note"
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (prompt choices &rest _)
                    "+ Create new note")))
         (should (eq 'new (denote-prompt-note-action notes))))))))

(ert-deftest denote-projectile-notes-test-prompt-note-action-no-notes ()
  "Test note action prompting when no notes exist."
  (should (eq 'new (denote-prompt-note-action nil))))

(ert-deftest denote-projectile-notes-test-create-overview-sections ()
  "Test overview sections creation."
  (let* ((metadata1 (list :title "Note 1" :date "2024-01-01" :filepath "/path1.org"))
         (metadata2 (list :title "Note 2" :date "2024-01-02" :filepath "/path2.org"))
         (metadata-list (list metadata1 metadata2)))
    ;; Mock denote-extract-note-content
    (cl-letf (((symbol-function 'denote-extract-note-content)
               (lambda (filepath)
                 (if (string= filepath "/path1.org")
                     "Content 1"
                   "Content 2"))))
      (let ((sections (denote-create-overview-sections metadata-list)))
        (should (string-match-p "Note 1" sections))
        (should (string-match-p "Note 2" sections))
        (should (string-match-p "Content 1" sections))
        (should (string-match-p "Content 2" sections))))))

(ert-deftest denote-projectile-notes-test-handle-project-note-selection ()
  "Test main project note selection workflow."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project")
         (test-called-with nil))  ; Declare variable outside cl-letf
     ;; Test with no existing notes - should default to 'new
     (cl-letf (((symbol-function 'denote-open-new-project-note)
                (lambda (name) (setf test-called-with name))))
       (denote-handle-project-note-selection project-name)
       (should (equal project-name test-called-with))))))

(ert-deftest denote-projectile-notes-test-maybe-update-overview ()
  "Test automatic overview update on save."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     ;; Create a project note
     (let ((filepath (denote-projectile-notes-test-create-note
                      project-name "Test Note" "Content")))
       ;; Mock buffer-file-name to return our test file
       (cl-letf (((symbol-function 'buffer-file-name) (lambda () filepath)))
         ;; Track if update was called
         (let (update-called)
           (cl-letf (((symbol-function 'denote-update-project-overview)
                      (lambda (name) (setf update-called name))))
             (denote-maybe-update-overview)
             (should (equal "test project" update-called)))))))))

(ert-deftest denote-projectile-notes-test-setup-project-note-buffer ()
  "Test project note buffer setup."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project")
         (note-title "Test Note"))
     (with-temp-buffer
       (denote-setup-project-note-buffer (current-buffer) project-name note-title)
       ;; Should have inserted frontmatter
       (should (> (point-max) 1))
       (should (string-match-p "Test Project - Test Note" (buffer-string)))
       ;; Should have set local variable
       (should (equal project-name denote-overview-project-name))))))

(ert-deftest denote-projectile-notes-test-prompt-for-note-selection ()
  "Test prompting for note selection with multiple notes."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     (let ((note1 (denote-projectile-notes-test-create-note
                   project-name "Note 1" "Content 1"))
           (note2 (denote-projectile-notes-test-create-note
                   project-name "Note 2" "Content 2")))
       (let ((notes (list note1 note2)))
         ;; Mock completing-read to select first note
         (cl-letf (((symbol-function 'completing-read)
                    (lambda (prompt choices &rest _)
                      (caar choices))))  ; Select first choice
           (let ((selected (denote-prompt-for-note-selection notes project-name)))
             (should (member selected notes)))))))))

;; Edge case tests
(ert-deftest denote-projectile-notes-test-list-notes-excludes-overview ()
  "Test that listing notes excludes overview files."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     ;; Create regular notes
     (denote-projectile-notes-test-create-note project-name "Note 1" "Content 1")
     (denote-projectile-notes-test-create-note project-name "Note 2" "Content 2")

     ;; Create an overview file
     (let ((overview-path (denote-project-overview-filepath project-name)))
       (with-temp-file overview-path
         (insert "Overview content")))

     ;; List notes should exclude the overview
     (let ((notes (denote-list-project-notes project-name)))
       (should (= 2 (length notes)))  ; Only the 2 regular notes
       (should (cl-every (lambda (f) (not (string-match-p "-overview\\.org$" f))) notes))))))

(ert-deftest denote-projectile-notes-test-overview-no-recursion ()
  "Test that overview generation doesn't include previous overviews."
  (denote-projectile-notes-test-with-temp-dir
   (let ((project-name "Test Project"))
     ;; Create a note
     (denote-projectile-notes-test-create-note project-name "Real Note" "Real content")

     ;; Generate first overview
     (let ((overview1-content (denote-generate-overview-content project-name)))
       ;; Create the overview file
       (let ((overview-path (denote-project-overview-filepath project-name)))
         (with-temp-file overview-path
           (insert overview1-content)))

       ;; Generate second overview (should not include first overview)
       (let ((overview2-content (denote-generate-overview-content project-name)))
         ;; Should only contain "Real Note", not nested overview content
         (should (string-match-p "Real Note" overview2-content))
         ;; Should not contain recursive "Overview" sections
         (should (= 1 (cl-count ?* overview2-content :test #'char-equal))))))))

(ert-deftest denote-projectile-notes-test-empty-content ()
  "Test handling empty note content."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((filepath (denote-projectile-notes-test-create-note
                     "Project" "Empty Note" ""))
          (content (denote-extract-note-content filepath)))
     (should (equal "" (string-trim content))))))

(ert-deftest denote-projectile-notes-test-demote-headers-edge-cases ()
  "Test header demotion with edge cases."
  (should (equal "Text without headers"
                 (denote-demote-org-headers "Text without headers")))
  (should (equal "****** Very deep heading"
                 (denote-demote-org-headers "***** Very deep heading")))
  (should (equal "Not a header * in middle"
                 (denote-demote-org-headers "Not a header * in middle"))))

(ert-deftest denote-projectile-notes-test-find-note-filepath-edge-cases ()
  "Test note filepath construction with edge cases."
  (denote-projectile-notes-test-with-temp-dir
   (let* ((project-name "Project/With\\Slashes")
          (note-id "weird-note-id.org")
          (expected-path (expand-file-name
                          note-id
                          (denote-project-notes-directory project-name)))
          (actual-path (denote-find-note-filepath project-name note-id)))
     (should (equal expected-path actual-path)))))

(provide 'denote-projectile-notes-test)

;;; denote-projectile-notes-test.el ends here
