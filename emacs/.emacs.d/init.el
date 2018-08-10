;; Load org-mode
(require 'org-install)
(require 'ob-tangle)

;; Load "genesis.org"
(org-babel-load-file (expand-file-name "genesis.org" (expand-file-name user-emacs-directory)))

;; Load "genesis_corp.org"
(setq genesis_corp (expand-file-name "genesis_corp.org" (expand-file-name user-emacs-directory)))
(when (file-exists-p genesis_corp)
  (org-babel-load-file genesis_corp))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
