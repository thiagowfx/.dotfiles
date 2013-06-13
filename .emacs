(add-to-list 'load-path "~/dotfiles/elisp")
(require 'install)

(define-key global-map (kbd "RET") 'newline-and-indent) ;RET acts likes C-j, automatically indenting newlines
(icomplete-mode 99)			;autoactivate icomplete-mode
(iswitchb-mode t)	    ;switches between buffers using substrings
(column-number-mode)	    ;display the column number in the mode bar
(ido-mode)			   ;for find-file and switch-to-buffer
(setq make-backup-files nil auto-save-default nil) ;I use version control, don't annoy me with backup files everywhere
(winner-mode)
(global-set-key [C-tab] 'other-window)	;easy switch buffers
(setq require-final-newline t default-indicate-empty-lines t) ;every file will have a \n at the end and empty lines will be indicated
(setq inhibit-startup-message t)	;don't show startup screen
(transient-mark-mode t)			;highlight marked region
(global-font-lock-mode t)		;syntax highlight everywhere
(fset 'yes-or-no-p 'y-or-n-p) ;make all "yes or no" prompts show "y or n" instead
(defun toggle-fullscreen (&optional f) ;fullscreen on F11 -- linux only
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
					;http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html ;lots of elisp packages!
