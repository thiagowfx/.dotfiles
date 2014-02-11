; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
; - ace-jump-mode
; - auto-complete-mode
; - graphviz-dot-mode (graphviz)
; - markdown-mode
; - pkgbuild-mode (for arch linux)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links
; - https://github.com/technomancy/better-defaults


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load-path (~/.emacs.d/)
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories (see list-packages and package-install)
(when (file-exists-p "~/.emacs.d/elpa/package.el")
  (when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
    (package-initialize)))
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(setq url-http-attempt-keepalives nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer
(file-name-shadow-mode t) ;; be smart about filenames in minibuffer
(icomplete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; X

;; Title for X frames. Template: source.cpp: thiago@arch
(setq-default frame-title-format ;; change title from a frame (X Only)
	      '(:eval (format "%s: %s@%s" 
			      (file-name-nondirectory (or (buffer-file-name) default-directory))
			      (or (file-remote-p default-directory 'user) user-login-name)
			      (or (file-remote-p default-directory 'host) system-name))))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defun toggle-fullscreen (&optional f)
  "Toggle Fullscreen -- GNU/Linux Only"
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

; - Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

; - C
(add-hook 'c-mode-hook (lambda () (setq compile-command (concat "gcc \""
								(buffer-file-name)
								"\" -o \""
								(file-name-sans-extension buffer-file-name)
								"\" -Wall -Wextra -g"))))
; - C++
(add-hook 'c++-mode-hook (lambda () (setq compile-command (concat "g++ \""
								  (buffer-file-name)
								  "\" -o \""
								  (file-name-sans-extension buffer-file-name)
								  "\" -Wall -Wextra -g"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and Tweaks...
(set-cursor-color "white")
(setq-default indent-tabs-mode nil)
(show-paren-mode t) ;; highlight matching parenthesis
(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode t) ;; use C-c <left> to restore the previous window view
(electric-pair-mode t) ;; autoclose parenthesis

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; ibuffer
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)	;; rename after killing uniquify
(setq uniquify-ignore-buffers-re "^\\*")

;; saveplace
(require 'saveplace)
(setq-default save-place t)

(setq scroll-margin 0                        
      scroll-conservatively 100000           
      scroll-up-aggressively 0               
      scroll-down-aggressively 0             
      scroll-preserve-screen-position t ;; preserve screen pos with C-v/M-v 
      compilation-read-command t  ;; autocompile without prompting the user, unless you give it a prefix argument?
      default-indicate-empty-lines t ;; vi like
      make-backup-files nil	     ;; file.ext~
      require-final-newline t
      case-fold-search t ;; make searches case insensitive
      save-place-file (concat user-emacs-directory "places")

      ;; clipboard stuff
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      vc-follow-symlinks t ;; do not ask for symlink confirmations
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Arch Linux PKGBUILDs
(when (locate-library "pkgbuild-mode")
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode
(when (locate-library "ace-jump-mode")
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-0") 'ace-jump-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete-mode
(when (and (locate-library "auto-complete")
	   (locate-library "popup"))
  (require 'auto-complete)
  (require 'popup)
  (setq ac-modes '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))
  (global-auto-complete-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tracks recently used files
(require 'recentf)   
(setq recentf-save-file               "~/.emacs.d/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode: a powerful mode for find-file and switch-to-buffer - interactively do things
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-enable-last-directory-history t
      ido-case-fold t)
(setq confirm-nonexistent-file-or-buffer nil) ;; disable annoying confirmation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keystrokes

;; Templates:
;; - (kbd "M-g")
;; - [f1]
;; - (kbd "<f1>")

(global-set-key    "\M-g"          'goto-line)                   
(global-set-key    (kbd "<f3>")    'reload-dot-emacs)
(global-set-key    (kbd "RET")     'newline-and-indent)	;; C-j like; alt: reindent-then-newline-and-indent
(global-set-key    [C-tab]         'other-window)
(global-set-key    [f5]            'compile)
(global-set-key    [f9]            'comment-or-uncomment-region)
(global-set-key    [f11]           'toggle-fullscreen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (misterioso)))
 '(ecb-source-path (quote (("/" "/"))))
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
