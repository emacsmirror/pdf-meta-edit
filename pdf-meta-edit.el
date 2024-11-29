;;; pdf-meta-edit.el --- Editing PDFs' metadata      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Keywords: files, data

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

;; This package lets users conveniently modify PDF metadata, labels, and
;; bookmarks within Emacs.  Emacs wrapper and convenience functions for changing
;; package metadata using `pdftk'.  See https://unix.stackexchange.com/a/72457
;; for more information on the CLI commands involved.

;;; Code:

;;; Options

;;; Variables
(defvar-local pdf-meta-edit-pdf-file nil
  "The absolute file path of the current `pdf-meta-edit-mode' buffer.")

;;; Functions
(defun pdf-meta-edit--buffer-name (pdf-file)
  "Return buffer name corresponding to PDF-FILE.
Returns a string that represents the buffer name used to edit the
metadata of PDF-FILE."
  (format "*pdf-meta-edit: %s*" (file-name-base pdf-file)))

;;; Keymap
(defvar-keymap pdf-meta-edit-mode-map
  :doc "Mode map for `pdf-meta-edit-mode'."
  "C-c C-c" #'pdf-meta-edit-commit
  "C-c C-b" #'pdf-meta-edit-bookmark-section
  "C-c C-l" #'pdf-meta-edit-label-section
  "M-n" #'pdf-meta-forward-subsection
  "M-p" #'pdf-meta-backward-subsection)

;;; Major mode
;;;; Font-locking
(defvar pdf-meta-edit-mode-font-lock-keywords
  `((,(rx (or (seq bol "InfoBegin")
              (seq bol "InfoKey:")
              (seq bol "InfoValue:")))
     . font-lock-doc-face)
    (,(rx (or (seq bol "PdfID" num ":")
              (seq bol "NumberOfPages:")))
     . font-lock-comment-delimiter-face)
    (,(rx (or (seq bol "BookmarkBegin")
              (seq bol "BookmarkTitle:")
              (seq bol "BookmarkLevel:")
              (seq bol "BookmarkPageNumber:")))
     . font-lock-keyword-face)
    (,(rx (or (seq bol "PageMediaBegin")
              (seq bol "PageMediaNumber:")
              (seq bol "PageMediaRotation:")
              (seq bol "PageMediaRect:")
              (seq bol "PageMediaDimensions:")))
     . font-lock-type-face)
    (,(rx (or (seq bol "PageLabelBegin")
              (seq bol "PageLabelNewIndex:")
              (seq bol "PageLabelStart:")
              (seq bol "PageLabelNumStyle:")))
     . font-lock-builtin-face))
  "Syntax highlighting for `pdf-meta-edit-mode'.")

;;;; Definition
(define-derived-mode pdf-meta-edit-mode fundamental-mode "Metadata"
  "Major mode for altering and viewing PDF metadata."
  :interactive t
  (setq font-lock-defaults (list pdf-meta-edit-mode-font-lock-keywords))
  (add-hook 'pdf-meta-edit-mode-hook #'font-lock-mode)
  (use-local-map pdf-meta-edit-mode-map))

;;; Commands
;;;; Entry-point
;;;###autoload
(defun pdf-meta-edit-modify (pdf-file)
  "Modify PDF-FILE metadata."
  (interactive (list (buffer-file-name)))
  (unless (string= "pdf" (file-name-extension pdf-file))
    (user-error "File is not a PDF!"))
  (unless (executable-find "pdftk")
    (error "System executable `pdftk' not found.  Please install executable on filesystem to proceed"))
  (let* ((metadata-buf-name (pdf-meta-edit--buffer-name pdf-file))
         (metadata-dump-command (format "pdftk \"%s\" dump_data" pdf-file))
         (pdf-buffer (find-buffer-visiting pdf-file)))
    ;; First we save the buffer visiting PDF-FILE in case it exists
    (when pdf-buffer
      (with-current-buffer pdf-buffer (save-buffer)))
    (unless (get-buffer metadata-buf-name)
      (with-current-buffer (get-buffer-create metadata-buf-name)
        (insert (shell-command-to-string metadata-dump-command))
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (pdf-meta-edit-mode)
        (setq-local pdf-meta-edit-pdf-file (expand-file-name pdf-file))))
    (pop-to-buffer metadata-buf-name)
    (message (substitute-command-keys "Press \\[pdf-meta-edit-commit] when finished editing PDF metadata. To see other keybinds, press \\[describe-mode]"))))

;;;; Subsection insertion
(defun pdf-meta-edit-commit ()
  "Save metadata information in buffer to pdf file."
  (interactive nil pdf-meta-edit-mode)
  (let* ((pdf-name (file-name-base pdf-meta-edit-pdf-file))
         (metadata-buf-name (pdf-meta-edit--buffer-name pdf-meta-edit-pdf-file))
         (temp-metadata-file (concat "/tmp/pdf-meta-edit--" pdf-name))
         (temp-pdf (make-temp-file "/tmp/pdf-meta-edit--temp-pdf"))
         (metadata-update-command
          (concat "pdftk \"" pdf-meta-edit-pdf-file "\" update_info \"" temp-metadata-file "\" output \"" temp-pdf "\"")))
    (with-current-buffer metadata-buf-name
      (widen)
      (write-region (point-min) (point-max) temp-metadata-file))
    (shell-command metadata-update-command "*pdf-meta-edit: CLI output*")
    (kill-buffer metadata-buf-name)
    ;; We must replace the pdf with temp-pdf because `pdftk' does not allow
    ;; having the output file be the input file
    (rename-file temp-pdf pdf-meta-edit-pdf-file t)
    (message "Updated metadata!")))

(defun pdf-meta-edit-bookmark-section ()
  "Insert bookmark metadata section."
  (interactive nil pdf-meta-edit-mode)
  (save-excursion
    (insert "\nBookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: "))
  (move-end-of-line 2))

(defun pdf-meta-edit-label-section ()
  "Insert bookmark metadata section."
  (interactive nil pdf-meta-edit-mode)
  (let* ((possible-styles
          '("DecimalArabicNumerals"
            "LowercaseRomanNumerals"
            "UppercaseRomanNumerals"
            "UppercaseLetters"
            "LowercaseLetters"
            "NoNumber"))
         (style
          (completing-read "Label style: " possible-styles nil t)))
    (save-excursion
      (insert "\n"
              "PageLabelBegin\n"
              "PageLabelNewIndex: 1\n"
              "PageLabelStart: 1\n"
              "PageLabelNumStyle: " style))
    (move-end-of-line 3)))

;;;; Movement
(defun pdf-meta-forward-subsection (&optional arg)
  "Move to the next ARG metadata subsection.
For instance, if the \"metadata subsection\" at point is a bookmark
subsection (a line beginning with \"Bookmark\"), then move to the next
line beginning with \"BookmarkBegin\"."
  (interactive "p" pdf-meta-edit-mode)
  (if (save-excursion
        (end-of-line)
        (search-forward-regexp
         (rx line-start
             (group (+ alpha)) ; Match the prefix
             (or "Begin" (seq word-boundary (+ alpha) ":")))
         nil :noerror (or arg 1)))
      (progn
        (goto-char (match-beginning 0))
        (beginning-of-line))
    (message "No next subsection")))

(defun pdf-meta-edit-backward-subsection (&optional arg)
  "Move to the previous ARG metadata subsection.
For instance, if the \"metadata subsection\" at point is a bookmark
subsection (a line beginning with \"Bookmark\"), then move to the last
line beginning with \"BookmarkBegin\"."
  (interactive "p" pdf-meta-edit-mode)
  (if (save-excursion
        (beginning-of-line)
        (search-backward-regexp
         (rx line-start
             (group (+ alpha))
             (or "Begin" (seq word-boundary (+ alpha) ":")))
         nil :noerror (or arg 1)))
      (progn
        (goto-char (match-beginning 0))
        (beginning-of-line))
    (message "No previous subsection")))

;;; Provide
(provide 'pdf-meta-edit)
;;; pdf-meta-edit.el ends here
