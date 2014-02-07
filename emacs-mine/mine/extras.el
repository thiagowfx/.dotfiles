;; extras
(provide 'extras)

;; auto-complete mode, see M-/ shortcut also
>>>>>>> 64011909112fb6f7831689d24e06fe744bb51d38
(require 'auto-complete)
(require 'popup)
(setq ac-modes '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))
(global-auto-complete-mode t)
