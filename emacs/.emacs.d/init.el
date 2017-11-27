;; Load org-mode
(require 'org-install)
(require 'ob-tangle)

;; Load "genesis.org"
(org-babel-load-file (expand-file-name "genesis.org" (expand-file-name user-emacs-directory)))
