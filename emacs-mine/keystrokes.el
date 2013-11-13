;; keystrokes
(provide 'keystrokes)

;; template
;; (kbd "M-g")

(define-key global-map (kbd "RET")         'newline-and-indent)          ;; C-j like; alt: reindent-then-newline-and-indent
(global-set-key        "\M-g"              'goto-line)                   
(global-set-key        (kbd "<f3>")        'reload-dot-emacs)
(global-set-key        (kbd "<f4>")        'magit-status)
(global-set-key        (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key        (kbd "<f5>")        'compile)
(global-set-key        (kbd "<f6>")        'exec-program)
(global-set-key        (kbd "<f7>")        'exec-program-with-input)
(global-set-key        [f8]                'exec-program-with-input-and-output)
(global-set-key        [C-tab]             'other-window)                ;; easy switching buffers
(global-set-key        (kbd "<f9>")        'comment-or-uncomment-region) ;; (un)comment
(global-set-key        [f11]               'toggle-fullscreen)
(global-set-key        "\C-z"              'undo)
