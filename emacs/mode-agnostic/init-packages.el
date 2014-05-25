;; Package Manager Initialization

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Ledger-Mode is in flux, get it directly from the git repository checkout.
(add-to-list 'load-path "/home/sykora/src/scratch/linux/ledger/ledger/lisp")
(require 'ledger-mode)

(add-to-list 'load-path "/home/sykora/tmp/prepaint")
(require 'prepaint)

(provide 'init-packages)
