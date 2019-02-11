;;; ebib+ -- Ebib Extensions

;;; Commentary:
;;; This file contains a somewhat arbitrary collection of extensions to
;;; `ebib'.

;;; Code:

(require 'ebib)

(defgroup ebib+ nil
  "Customization options for `ebib+'."
  :group 'ebib)

(defcustom ebib-download-directory nil
  "Directory to put downloaded documents.
Should probably be one `ebib-file-search-dirs'."
  :type '(string)
  :group 'ebib+)

(defun ebib--download-directory ()
  "Get the effective ebib download directory.
If `ebib-download-directory' is set, use that; otherwise, use the
first directory from `ebib-file-search-dirs'"
  (or ebib-download-directory
      (car ebib-file-search-dirs)))

(defun ebib--download-and-rename (url key ext)
  "Download `URL' and store it under `KEY' with extension `EXT'."
  (url-copy-file url
   (expand-file-name
    (concat (file-name-as-directory (ebib--download-directory))
            key "." ext))))

(defun ebib--infer-url-extension (url)
  "Infer the extension of the file at `URL'."
  (file-name-extension (url-filename (url-generic-parse-url url))))

(defun ebib-download-and-associate (url)
  "Downloand `URL', and associate it with the current entry.
A file extension will be prompted for if it cannot be inferred."
  (interactive "MURL: ")
  (let* ((key (ebib--get-key-at-point))
         (ext (or (ebib--infer-url-extension url)
                  (read-from-minibuffer "Extension: ")))
         (path (concat (file-name-as-directory (ebib--download-directory))
                                               key "." ext)))
    (url-copy-file url path)
    (ebib-db-set-field-value ebib-file-field
                             (ebib--transform-file-name-for-storing path)
                             key
                             ebib--cur-db)
    (ebib--update-buffers)
    (ebib--set-modified t)))

(provide 'ebib+)
;;; ebib+.el ends here
