;; -*- eval: (outline-minor-mode) -*-
;; Declarations and default definitions of host-specific configuration parameters.

;; * Major Modes
;; ** Ebib
(defvar host-ebib-bib-search-dirs nil)
(defvar host-ebib-file-search-dirs nil)
(defvar host-ebib-preload-bib-files nil)
(defvar host-ebib-file-associations nil)

;; ** Org
(defvar host-org-directory (expand-file-name "~/"))
(defvar host-org-agenda-directory (expand-file-name "~/"))
(defvar host-org-capture-triage-path (expand-file-name "~/triage.org"))
(provide 'init-host)
