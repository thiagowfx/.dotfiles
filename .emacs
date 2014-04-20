;; -*- lisp -*-

;; (add-to-list 'load-path "/path/to/your/package")
;; (require 'package-provide-name)
;; (when (locate-library "package-name") (package-configs-here))
;; (when (require 'provide-name nil 'noerror) (package-configs-here))

(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(when (>= emacs-major-version 24)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")))

(load-theme 'wombat t)

;; Helper Functions
(defun add-something-to-mode-hooks (mode-list something)
  "Helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
(add-something-to-mode-hooks '(emacs-lisp-mode-hook lisp-mode-hook) 'turn-on-eldoc-mode)


;; Miscellaneous
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)            ; do not create *~ files
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)
(ido-mode t)                            ; completion: { | | | }
(icomplete-mode t)                      ; completion: {,,,}
(setq-default ido-enable-flex-matching t
              ido-enable-last-directory-history t
              confirm-nonexistent-file-or-buffer nil) ; disable annoying confirmation
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(windmove-default-keybindings)
(global-subword-mode t)                 ; camel-case
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(show-paren-mode t)
(winner-mode t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(setq-default frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(setq-default indicate-empty-lines t)
(setq visible-bell t)
(setq use-dialog-box nil)
(setq echo-keystrokes 0.1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t) ; stack minibuffers, exit with `top-level`
(setq read-buffer-completion-ignore-case t)
(setq require-final-newline t)
(setq case-fold-search t)    ; make searches case insensitive
(setq vc-follow-symlinks t)  ; do not ask for symlink confirmations
(global-auto-revert-mode t)  ; autoload modified files outside emacs
(setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))
(recentf-mode t)
(setq default-frame-alist '((cursor-color . "white")))
(set-cursor-color "white")

(when (locate-library "drag-stuff") ; M-down, M-up
  (drag-stuff-global-mode t))

(when (locate-library "smart-mode-line")
  (sml/setup))

(when (locate-library "mode-icons")
  (mode-icons-mode))

(when (locate-library "goto-chg")
  (global-set-key [(control .)] 'goto-last-change))

(when (locate-library "control-lock")   ; no-pinky
  (require 'control-lock)
  (global-set-key "\C-z" 'control-lock-toggle))

(when (locate-library "helm")
  (helm-mode t)
  (global-set-key "\M-x" 'helm-M-x))

(when (locate-library "undo-tree")
  (undo-tree-mode t)
  (defalias 'undo 'undo-tree-undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "C-+") 'undo-tree-redo))

(when (locate-library "js2-mode")
  (defalias 'javascript-mode 'js2-mode))

(when (locate-library "web-mode")
  (defalias 'html-mode 'web-mode))

(when (locate-library "ace-jump-mode")
  (define-key global-map (kbd "C-0") 'ace-jump-mode)
  (setq-default ace-jump-mode-submode-list '(ace-jump-char-mode
                                             ace-jump-line-mode
                                             ace-jump-word-mode)))

(require 'savehist)
(savehist-mode t)

(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/")

(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; Markdown Mode
;; <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
(when (locate-library "markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

(add-hook 'c++-mode-hook
          (lambda () (setq compile-command (concat "g++ \""
                                                   (buffer-file-name)
                                                   "\" -o \""
                                                   (file-name-sans-extension buffer-file-name)
                                                   "\" -Wall -g -O2"))))

(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (global-set-key "\M-\t" 'auto-complete)
  (setq ac-auto-show-menu nil)
  (setq ac-sources '(ac-source-abbrev
                     ac-source-dictionary
                     ac-source-features
                     ac-source-filename
                     ac-source-files-in-current-dir
                     ac-source-functions
                     ac-source-imenu
                     ac-source-symbols
                     ac-source-variables
                     ac-source-yasnippet
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers)))
                                        ; ac-source-semantic
                                        ; ac-source-gtags

(defun reload-dot-emacs ()
  "Reload your ~/.emacs file."
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "<f2>")    'reload-dot-emacs)

(defun replace-last-sexp ()
  "Eval in place"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(global-set-key "\C-c\C-x\C-e"  'replace-last-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer cleaning
(defun untabify-buffer ()
  "Converts tabs to space on the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indents the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Custom keystrokes/keybindings
;; - RET, "\M-g", [C-tab], (kbd "M-g"), [f1], (kbd "<f1>"), [?\C-\t], (kbd "<C-S-iso-lefttab>")
(global-set-key "\M-g"          'goto-line)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key [f5]            'compile)
(global-set-key [f6]            'magit-status)
(global-set-key [f9]            'comment-or-uncomment-region)
(global-set-key [f12]           'cleanup-buffer)
(global-set-key (kbd "C-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-x\C-r" 		'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Homepages/References
;; - http://www.emacsrocks.com/
;; - http://www.emacswiki.org/
;; - http://www.emacswiki.org/emacs/EmacsCrashCode/
;; - https://github.com/technomancy/better-defaults/
;; - http://www.aaronbedra.com/emacs.d/ ;; teaches how to install all (missing) packages from a given list
;; - http://draketo.de/light/english/emacs/babcore
;; - http://www.nongnu.org/emacs-tiny-tools/keybindings/index-body.html
