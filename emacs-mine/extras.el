;; extras
(provide 'extras)


;; redo utilities
(require 'redo+)
(when (fboundp 'redo+)
  (global-set-key        [C-S-z]              'undo))

;; auto-complete mode, also see M-/ shortcut
(require 'auto-complete)
(require 'popup)
(setq ac-modes '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))
(global-auto-complete-mode t)
