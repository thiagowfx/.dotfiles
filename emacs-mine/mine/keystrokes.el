;; keystrokes
(provide 'keystrokes)

;; templates
;; (kbd "M-g")
;; [f1]
;; (kbd "<f1>")

(define-key global-map (kbd "RET")         'newline-and-indent)          ;; C-j like; alt: reindent-then-newline-and-indent
(global-set-key        "\M-g"              'goto-line)                   
(global-set-key        (kbd "<f3>")        'reload-dot-emacs)
(global-set-key        (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key        [f5]                'compile)
(global-set-key        [f6]                'exec-program-with-input-and-output)
(global-set-key        [f7]                'exec-program-with-input)
(global-set-key        [f8]                'exec-program)
(global-set-key        [f9]                'comment-or-uncomment-region)
(global-set-key        [f11]               'toggle-fullscreen)
(global-set-key        [C-tab]             'other-window)
(global-set-key        "\C-z"              'undo)
