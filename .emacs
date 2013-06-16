;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 2013-2013 Thiago Barroso Perrotta.                         ;;
;; This file is free software licensed under the terms of the               ;;
;; GNU General Public License, version 3 or later.                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-dot-emacs ()
  "Reload .emacs file while inside emacs."
  (interactive)
  (load-file "~/.emacs"))
(global-set-key "\C-c\C-e" 'reload-dot-emacs)

; Add more repos. More packages and .el scripts.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(define-key global-map (kbd "RET") 'newline-and-indent) ;RET acts likes C-j, automatically indenting newlines ;alt: reindent-then-newline-and-indent
(global-set-key "\C-x\C-m" 'compile)	;shortcut for compile command
(icomplete-mode t)   ;autoactivate icomplete-mode
(iswitchb-mode t)    ;switches between buffers using substrings
(column-number-mode t) ;display the column number in the mode bar
(ido-mode t)	     ;powerful mode for find-file and switch-to-buffer
(setq make-backup-files nil auto-save-default nil) ;I use version control, don't annoy me with backup files everywhere
(winner-mode t) ;use C-c <left> to restore the previous window configurations -- useful for compile commands, for example
(global-set-key [C-tab] 'other-window)	;easy switching buffers
(setq require-final-newline t
      default-indicate-empty-lines t ;every file will have a \n at the end and empty lines will be indicated
      next-line-add-newlines nil ;C-n at the end of a file acts like newline --> i don't want it
      inhibit-startup-message t) ;don't show startup screen, it's annoying
(transient-mark-mode t)			;highlight marked region
(global-font-lock-mode t)		;syntax highlight everywhere
(fset 'yes-or-no-p 'y-or-n-p) ;make all "yes or no" prompts show "y or n" instead, which is less annoying
(setq compilation-read-command t) ; compilation: autocompile without prompting the user, unless you give it a prefix argument
(defun toggle-fullscreen (&optional f)
  "Fullscreen on F11 -- GNU/Linux Only"
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
	(delete-windows-on buffer)
	(tooltip-show "\n Compilation Successful ;-) \n "))
    (tooltip-show "\n Compilation Failed =/ \n "))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions 'notify-compilation-result)

;auto-complete mode, also look for M-/ shortcut
(require 'auto-complete)
(setq ac-modes '(c-mode c++-mode))
(global-auto-complete-mode t)

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq compile-command
;		  (format "g++ %s -o %s -Wall"
;			  (buffer-file-name)
;			  (file-name-sans-extesion buffer-file-name)))))  
		 (concat "g++ "
			 (buffer-file-name)
			 " -o "
			 (file-name-sans-extension buffer-file-name)
			 " -Wall"))))
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq compile-command
		 (concat "gcc "
			 (buffer-file-name)
			 " -o "
			 (file-name-sans-extension buffer-file-name)
			 " -Wall"))))

; Referências de .emacs
; Load your .emacs file while in emacs M-x load-file RET ~/.emacs
; https://news.ycombinator.com/item?id=1654164
; https://github.com/vvv/dotfiles/blob/master/.emacs
; http://www.emacsrocks.com/
; http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html ;lots of elisp packages!
; http://www.emacswiki.org/emacs/CompileCommand

;custom theme for emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (adwaita)))
 '(ecb-source-path (quote (("/" "/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
