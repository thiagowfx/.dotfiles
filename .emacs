;RET acts likes C-j, automatically indenting newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

;autoactivate icomplete-mode
(icomplete-mode 99)

;switches between buffers using substrings
(iswitchb-mode t)

;display the column number in the mode bar
(column-number-mode)

;for find-file and switch-to-buffer
(ido-mode)

;I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil auto-save-default nil)

(winner-mode)

;easy switch buffers
(global-set-key [C-tab] 'other-window)

;every file will have a \n at the end and empty lines will be indicated
(setq require-final-newline t default-indicate-empty-lines t)

;don't show startup screen
(setq inhibit-startup-message t)

;highlight marked region
(transient-mark-mode t)

;syntax highlight everywhere
(global-font-lock-mode t)

;make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;fullscreen on F11 -- linux only
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;C-n at the end of a file acts like newline
;(setq next-line-add-newlines t)

;Referências de .emacs
;https://news.ycombinator.com/item?id=1654164
;https://github.com/vvv/dotfiles/blob/master/.emacs
;http://www.emacsrocks.com/
