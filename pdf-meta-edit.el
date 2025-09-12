;;; pdf-meta-edit.el --- Edit PDF metadata via pdftk -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://github.com/krisbalintona/pdf-meta-edit
;; Keywords: files, data
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.3") (compat "29.1"))

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
;; bookmarks within Emacs.  Metadata is retrieved from PDFs using the pdftk CLI
;; tool.  Therefore, a dependency for this package is a pdftk binary.  This
;; package has only been tested in Linux.
;;
;; USAGE:
;;
;; Open a PDF file (in any major mode) and call `pdf-meta-edit-modify'.  This
;; creates and opens a `pdf-meta-edit-mode' buffer whose contents is a
;; plain-text representation of the metadata of that PDF.  Edit the contents of
;; this buffer.  Once done, "commit" the buffer contents to the PDF file via C-c
;; C-c, which calls `pdf-meta-edit-commit'.
;;
;; The metadata of a PDF file includes bookmarks (outline), labels (document
;; pagination), page information, and other information.  The representation of
;; this metadata is composed of subsections.  For instance, information
;; pertaining to a bookmark subsection includes fields for the pdf page number,
;; the level of the bookmark, and the title of the bookmark.  Pdf-meta-edit
;; provides commands to conveniently add new bookmark and label subsections: see
;; `pdf-meta-edit-bookmark-subsection' and `pdf-meta-edit-label-subsection'.
;; For a thorough documentation of the representation of PDF metadata and
;; possible fields and values, see the cpdf manual:
;; https://github.com/johnwhitington/cpdf-source/blob/master/cpdfmanual.pdf.

;;; Code:

;;; Options
(defgroup pdf-meta-edit ()
  "Interface for editing PDF metadata via pdftk."
  :group 'files
  :prefix "pdf-meta-edit-")

(defcustom pdf-meta-edit-command (executable-find "pdftk")
  "String passed to the shell to call the pdftk.
Users should be aware that some versions of the pdftk binary do not
support updating page labels.  If this applies to you, please read the
package README (found at https://github.com/krisbalintona/pdf-meta-edit)
for more information on how to acquire the requisite pdftk binary."
  :type 'string)

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
  "C-c C-b" #'pdf-meta-edit-bookmark-subsection
  "C-c C-l" #'pdf-meta-edit-label-subsection
  "M-n" #'pdf-meta-edit-forward-subsection
  "M-p" #'pdf-meta-edit-backward-subsection
  "M-}" #'pdf-meta-edit-forward-section
  "M-{" #'pdf-meta-edit-backward-section)

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
  (unless pdf-meta-edit-command
    (error "Please ensure the system executable `pdftk' is installed and specified by the `pdf-meta-edit-command' option"))
  (let* ((metadata-buf-name (pdf-meta-edit--buffer-name pdf-file))
         (metadata-dump-command (concat pdf-meta-edit-command " \"" pdf-file "\" dump_data"))
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
         (temp-metadata-file (concat (temporary-file-directory) "pdf-meta-edit--" pdf-name))
         (temp-pdf (make-temp-file (concat (temporary-file-directory) "pdf-meta-edit--temp-pdf")))
         (metadata-update-command
          (concat pdf-meta-edit-command " \"" pdf-meta-edit-pdf-file "\" update_info \"" temp-metadata-file "\" output \"" temp-pdf "\"")))
    (cond
     ((not pdf-meta-edit-pdf-file)
      (error "No buffer-local value for `pdf-meta-edit-pdf-file'!"))
     ((not (file-exists-p pdf-meta-edit-pdf-file))
      (error "Value of `pdf-meta-edit-pdf-file' does not point to existing PDF file!")))
    (with-current-buffer metadata-buf-name
      (widen)
      (write-region (point-min) (point-max) temp-metadata-file))
    (shell-command metadata-update-command "*pdf-meta-edit: CLI output*")
    ;; We must replace the pdf with temp-pdf because pdftk does not allow having
    ;; the output file be the input file
    (rename-file temp-pdf pdf-meta-edit-pdf-file t)
    (kill-buffer metadata-buf-name)
    (message "Updated metadata!")))

(defun pdf-meta-edit-bookmark-subsection ()
  "Insert bookmark metadata section."
  (interactive nil pdf-meta-edit-mode)
  (let ((bookmark-level
         (if (save-excursion
               (goto-char (pos-bol))
               (looking-at "^Bookmark"))
             (save-excursion
               (save-match-data
                 (search-backward-regexp "^BookmarkBegin")
                 (search-forward-regexp (rx (group bol "BookmarkLevel:" (* whitespace))
                                            (group (+ num))
                                            eol))
                 (match-string 2)))
           "1"))
        reg-begin reg-end)
    (pdf-meta-edit-forward-subsection)
    (setq reg-begin (point))
    (forward-line -1)
    (save-excursion
      (end-of-line)
      (insert "\n"
              "BookmarkBegin\n"
              "BookmarkTitle: \n"
              "BookmarkLevel: "
              bookmark-level "\n"
              "BookmarkPageNumber: ")
      (setq reg-end (point)))
    (move-end-of-line 2)
    (pulse-momentary-highlight-region reg-begin reg-end)))

(defun pdf-meta-edit-label-subsection ()
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
          (completing-read "Label style: " possible-styles nil t))
         reg-begin reg-end)
    (pdf-meta-edit-forward-subsection)
    (setq reg-begin (point))
    (forward-line -1)
    (save-excursion
      (end-of-line)
      (insert "\n"
              "PageLabelBegin\n"
              "PageLabelNewIndex: 1\n"
              "PageLabelStart: 1\n"
              "PageLabelNumStyle: " style)
      (setq reg-end (point)))
    (move-end-of-line 3)
    (pulse-momentary-highlight-region reg-begin reg-end)))

;;;; Movement
(defun pdf-meta-edit-forward-section (&optional arg)
  "Move to the next ARG metadata section.
For instance, if the \"metadata section\" at point is a bookmark
subsection (a line beginning with \"Bookmark\"), then move to the
next line not beginning with \"Bookmark\".

If ARG is negative, move backward ARG metadata sections."
  (interactive "p" pdf-meta-edit-mode)
  (if (< 0 arg)
      (let (end-pt)
        (save-excursion
          (dotimes (_ (or arg 1))
            (let ((regexp (concat "^"
                                  (buffer-substring-no-properties (pos-bol)
                                                                  (save-excursion (beginning-of-line)
                                                                                  (forward-word)
                                                                                  (point))))))
              (while (and (not (eobp))      ; Avoid infinite loop
                          (save-excursion
                            (search-forward-regexp regexp
                                                   (save-excursion
                                                     (forward-line 1)
                                                     (pos-eol))
                                                   :noerror)))
                (forward-line 1))
              (setq end-pt (point)))))
        (if (= end-pt (point-max))
            (message "No next section")
          (goto-char end-pt)
          (beginning-of-line)))
    (pdf-meta-edit-backward-section (- arg))))

(defun pdf-meta-edit-backward-section (&optional arg)
  "Move to the previous ARG metadata section.
For instance, if the \"metadata section\" at point is a bookmark
subsection (a line beginning with \"Bookmark\"), then move to the
previous line not beginning with \"Bookmark\".

If ARG is negative, move backward ARG metadata sections."
  (interactive "p" pdf-meta-edit-mode)
  (if (< 0 arg)
      (if (= (pos-bol) (point-min))
          (message "No previous section")
        (let (end-pt)
          (save-excursion
            (dotimes (_ (or arg 1))
              (let ((regexp (concat "^"
                                    (buffer-substring-no-properties (pos-bol)
                                                                    (save-excursion
                                                                      (beginning-of-line)
                                                                      (forward-word)
                                                                      (point))))))
                ;; Go to the end of the previous section
                (while (and (not (bobp))      ; Avoid infinite loop
                            (save-excursion
                              (end-of-line)
                              (search-backward-regexp regexp
                                                      (save-excursion
                                                        (forward-line -1)
                                                        (pos-bol))
                                                      :noerror)))
                  (forward-line -1))
                ;; Go to the beginning of this section (the previous section)
                (let ((section-prefix
                       (buffer-substring-no-properties (pos-bol)
                                                       (save-excursion
                                                         (beginning-of-line)
                                                         (forward-word)
                                                         (point)))))
                  (while (and (not (bobp))
                              (progn
                                (forward-line -1)
                                (beginning-of-line)
                                (looking-at section-prefix)))))
                (setq end-pt (point)))))
          (goto-char end-pt)
          (beginning-of-line)))
    (pdf-meta-edit-forward-section (- arg))))

(defun pdf-meta-edit-forward-subsection (&optional arg)
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
