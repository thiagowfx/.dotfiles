;; keystrokes
(provide 'keystrokes)

;; templates
;; (kbd "M-g")
;; [f1]
;; (kbd "<f1>")

(global-set-key    "\M-g"          'goto-line)                   
(global-set-key    (kbd "<f3>")    'reload-dot-emacs)
(global-set-key    (kbd "RET")     'newline-and-indent)	;; C-j like; alt: reindent-then-newline-and-indent
(global-set-key    [C-tab]         'other-window)
(global-set-key    [f5]            'compile)
(global-set-key    [f9]            'comment-or-uncomment-region)
(global-set-key    [f11]           'toggle-fullscreen)
