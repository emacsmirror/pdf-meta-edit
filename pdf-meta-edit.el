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

;;;###autoload
(defun pdf-meta-edit-bookmark-section ()
  "Insert bookmark metadata section."
  (interactive)
  (save-excursion
    (insert "\nBookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: "))
  (move-end-of-line 2))

;;;###autoload
(defun pdf-meta-edit-label-section ()
  "Insert bookmark metadata section."
  (interactive)
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
              "PageLabelNumStyle: " style))))

(defvar-keymap pdf-meta-edit-mode-map
  :doc "Mode map for `pdf-meta-edit-mode'."
  "C-c C-b" #'pdf-meta-edit-bookmark-section
  "C-c C-l" #'pdf-meta-edit-label-section)

(define-derived-mode pdf-meta-edit-mode fundamental-mode "Metadata"
  "Major mode for altering and viewing PDF metadata."
  :interactive t
  (use-local-map pdf-meta-edit-mode-map))

;;;###autoload
(defun pdf-meta-edit-modify (pdf-file)
  "Modify PDF-FILE metadata."
  (interactive (list (buffer-file-name)))
  (unless (string= "pdf" (file-name-extension pdf-file))
    (user-error "File is not a PDF!"))
  (unless (executable-find "pdftk")
    (error "System executable `pdftk' not found. Please install executable on filesystem to proceed"))
  (let* ((pdf-name (file-name-sans-extension (file-name-nondirectory pdf-file)))
         (buf-name (concat "*pdf-tools metadata: " pdf-name))
         (metadata-file (concat "/tmp/pdf-tools-metadata--" pdf-name))
         (temp-pdf (make-temp-file "/tmp/pdf-tools-metadata--temp-pdf"))
         (metadata-dump-command (concat "pdftk \"" pdf-file "\" dump_data"))
         (metadata-update-command
          (concat "pdftk \"" pdf-file "\" update_info \"" metadata-file "\" output \"" temp-pdf "\""))
         ;; TODO 2024-10-30: Make committing more robust by adding a named
         ;; commit command to `pdf-meta-edit-mode-map' whose activity
         ;; depends on a buffer-local value set here.  This makes the command
         ;; work in e.g. major mode changes.
         (commit-func (lambda ()
                        "Commit the changes to PDF metadata."
                        (interactive)
                        (with-current-buffer buf-name
                          (widen)
                          (write-region (point-min) (point-max) metadata-file))
                        (shell-command metadata-update-command "*pdf-tools metadata: CLI output")
                        (kill-buffer buf-name)
                        ;; Have to do it this way since `pdftk' does not allow
                        ;; having the output file be the input file
                        (rename-file temp-pdf pdf-file t)
                        (message "Updated metadata!"))))
    (save-buffer)
    (with-current-buffer (get-buffer-create buf-name)
      (insert (shell-command-to-string metadata-dump-command))
      (goto-char (point-min))
      (pdf-meta-edit-mode))
    (pop-to-buffer buf-name)
    (define-key pdf-meta-edit-mode-map (kbd "C-c C-c") commit-func)
    (set-buffer-modified-p nil)
    (message (substitute-command-keys "Press `C-c C-c' when finished editing PDF metadata. To see keybinds, press \\[describe-mode]"))))

;;; Provide
(provide 'pdf-meta-edit)
;;; pdf-meta-edit.el ends here
