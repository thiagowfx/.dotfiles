;; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Packages -- To upgrade them, use U x on *Packages* buffer
;; - ace-jump-mode ;; use C-0 to go anywhere
;; - auto-complete
;; - cmake-mode
;; - graphviz-dot-mode (graphviz)
;; - keyfreq
;; - markdown-mode
;; - nav
;; - pkgbuild-mode (for Arch Linux)
;; - popup
;; - rainbow-mode (colorize strings)
;; - smex
;; - yaml-mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Topics
;;
;; Narrowing:
;; - C-x n n: narrow between point and mark
;; - C-x n w: un-narrow (~widen)
;;
;; M-x Paradise:
;; - Imenu :: Go directly to a function definition.
;; - Occur :: Overview of your file (just choose a keyword)
;;
;; Registers:
;; - C-x r s <label> :: save region to register <label>
;; - C-x r i <label> :: insert register <label> to region
;;
;; Modes:
;; Follow-mode :: two buffers with the same content to scroll simultaneously
;; Scroll-all-mode :: scroll any two buffers simultaneously


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links
;; - http://www.emacsrocks.com/
;; - http://www.emacswiki.org/emacs/EmacsCrashCode/
;; - https://github.com/technomancy/better-defaults/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load-path (~/.emacs.d/)
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories (see `list-packages` and `package-install`)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq url-http-attempt-keepalives nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)


;; uniquify: better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)	;; rename after killing uniquify
(setq uniquify-ignore-buffers-re "^\\*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer
(file-name-shadow-mode t) ;; be smart about filenames in minibuffer
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-enable-last-directory-history t
      ido-case-fold t)
(setq confirm-nonexistent-file-or-buffer nil) ;; disable annoying confirmation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs GUI/X Interface

(setq-default frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))
(menu-bar-mode -1)
(tool-bar-mode -1)


;; Clipboard stuff
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

;; - Emacs Lisp
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; - C
(add-hook 'c-mode-hook (lambda () (setq compile-command (concat "gcc \""
								(buffer-file-name)
								"\" -o \""
								(file-name-sans-extension buffer-file-name)
								"\" -Wall -Wextra -g -O2"))))
;; - C++
(add-hook 'c++-mode-hook (lambda () (setq compile-command (concat "g++ \""
								  (buffer-file-name)
								  "\" -o \""
								  (file-name-sans-extension buffer-file-name)
								  "\" -Wall -Wextra -g -O2"))))

;; use C-c @ C-c to toggle folding (hiding) of functions
(add-hook 'lisp-mode-hook (lambda () (hs-minor-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode t)))
(add-hook 'c-mode-hook (lambda () (hs-minor-mode t)))
(add-hook 'c++-mode-hook (lambda () (hs-minor-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design
(set-cursor-color "white")
(show-paren-mode t)                 ;; highlight matching parenthesis
(setq show-paren-style 'expression) ;; and its expression
(global-linum-mode t)               ;; show line numbers
(setq default-indicate-empty-lines t) ;; vim like
(blink-cursor-mode -1)
(mouse-avoidance-mode 'animate) ;; mouse gets out of the way


;; Syntax Highlighting Everywhere
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweaks, Small Improvements and Custom Packages
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(winner-mode t) ;; use C-c <left> to restore the previous window view
(setq-default inhibit-debugger t) ;; disable auto debugging
(setq read-file-name-completion-ignore-case t)
(setq enable-recursive-minibuffers t) ;; stack minibuffers
(setq read-buffer-completion-ignore-case 't) ;; Ignore case when completing buffer names
(windmove-default-keybindings) ;; use shift + arrow key to switch buffers
(setq make-backup-files nil) ;; do not create example.txt~ files
(setq require-final-newline t)
(setq case-fold-search t)   ;; make searches case insensitive
(setq vc-follow-symlinks t) ;; do not ask for symlink confirmations
(setq apropos-do-all t)


;; Electric modes
;; (when (>= emacs-major-version 24)
;; (electric-pair-mode t)    ;; autoclose parenthesis
;; (electric-indent-mode +1) ;; autoindent lines
;;  )


;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


;; scroll
(setq scroll-margin 4
      scroll-conservatively 4
      scroll-preserve-screen-position t) ;; preserve screen pos with C-v/M-v


;; Startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arch Linux PKGBUILD Mode
(when (locate-library "pkgbuild-mode")
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))

;; Markdown Mode
(when (locate-library "markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Autoenter SH mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace-jump-mode
(when (locate-library "ace-jump-mode")
  (define-key global-map (kbd "C-0") 'ace-jump-mode)
  (setq ace-jump-mode-submode-list '(ace-jump-char-mode
                                     ace-jump-line-mode
                                     ace-jump-word-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete-mode
(when (and (locate-library "auto-complete")
	   (locate-library "popup"))
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-start 3) ;; how many chars to auto-activate AC?
  (define-key ac-mode-map (kbd "M-TAB") 'ac-fuzzy-complete)
  (global-auto-complete-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-mode
; (add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAGS
;; http://www.svi.nl/EmacsProgrammingTips
(setq tags-table-list (list "./" "./../" "./../../" "./../../../" (getenv "DEVELOP")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
(setq recentf-save-file (concat user-emacs-directory "recentf"))
(setq recentf-max-saved-items 150
      recentf-max-menu-items 25)
(recentf-mode t)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

(global-set-key "\C-x\C-r" 'recentf-open-files)
(global-set-key (kbd "\C-c\C-x\C-r") 'ido-choose-from-recentf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defuns

(defun reload-dot-emacs ()
  "Reload your ~/.emacs file."
  (interactive)
  (load-file "~/.emacs"))


(defun exec-program-with-io ()
  "Execute the current buffer name with the .in input and .out output"
  (interactive)
  (shell-command (concat "\""
			 (file-name-sans-extension buffer-file-name)
			 "\" < \""
			 (file-name-sans-extension buffer-file-name)
			 ".in\" "
			 "> \""
			 (file-name-sans-extension buffer-file-name)
			 ".out\"")))


(defun replace-last-sexp ()
  "Eval in place"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))


;; (defun notify-compilation-result (buffer msg)
;;   "Notify that the compilation is finished, close the *compilation* buffer if the compilation is successful, and set the focus back to Emacs frame"
;;   (if (string-match "^finished" msg)
;;       (progn
;; 	(delete-windows-on buffer)
;; 	(tooltip-show "\n Compilation Successful!!! ;-) \n "))
;;     (tooltip-show "\n Compilation Failed =/ mimi \n "))
;;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;   (select-frame-set-input-focus current-frame))
;; (add-to-list 'compilation-finish-functions 'notify-compilation-result)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keystrokes/keybindings

;; Templates:
;; - RET
;; - "\M-g"
;; - [C-tab]
;; - (kbd "M-g")
;; - [f1]
;; - (kbd "<f1>")

(global-set-key "\M-g"          'goto-line)                   
(global-set-key (kbd "RET")     'newline-and-indent) ;; C-j like
(global-set-key [C-tab]         'other-window)
(global-set-key (kbd "<f2>")    'reload-dot-emacs)
(global-set-key [f5]            'compile)
(global-set-key [f9]            'comment-or-uncomment-region)
(global-set-key [f11]           'toggle-fullscreen)
(global-set-key (kbd "M-/") 	'hippie-expand)
(global-set-key "\C-c\C-x\C-e" 	'replace-last-sexp)
(when (fboundp 'smex)
  (global-set-key "\M-x" 	'smex)
  (global-set-key "\M-X"	'smex-major-mode-commands))
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (wombat)))
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
