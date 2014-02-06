;; extras
(provide 'extras)

;; auto-complete mode, see M-/ shortcut also
(require 'auto-complete)
(require 'popup)
(setq ac-modes '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))
(global-auto-complete-mode t)
