;; -*- emacs-lisp -*-

(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(when (>= emacs-major-version 24)
  (when (fboundp 'load-theme)
    (load-theme 'wombat t)
    (setq default-frame-alist '((cursor-color . "white")))
    (set-cursor-color "white"))

  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")))

;; Helper Functions
(defun add-something-to-mode-hooks (mode-list something)
  "Helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
(defvar my-programming-alist '(c c++ emacs-lisp lisp python))
(add-something-to-mode-hooks '(emacs-lisp lisp) 'turn-on-eldoc-mode)

;; Miscellaneous
(prefer-coding-system 'utf-8)
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default standard-indent 2)
(windmove-default-keybindings)          ; shift + arrow keys
(global-subword-mode t)                 ; camel-case
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(show-paren-mode t)
(winner-mode t)                         ; C-c left, C-c right
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(setq-default frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(setq-default indicate-empty-lines t)
(setq visible-bell t)
(setq echo-keystrokes 0.1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t) ; stack minibuffers, exit with `top-level`
(setq read-buffer-completion-ignore-case t)
(setq require-final-newline t)
(setq case-fold-search t)    ; make searches case insensitive
(setq vc-follow-symlinks t)  ; do not ask for symlink confirmations
(global-auto-revert-mode t)  ; autoload modified files outside emacs
(setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))

;; stop all BACKUPs
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; external libraries
(when (locate-library "drag-stuff") ; M-down, M-up
  (drag-stuff-global-mode t))

(when (locate-library "smart-mode-line")
  (sml/setup))

(when (locate-library "mode-icons")
  (mode-icons-mode))

(when (locate-library "goto-chg")       ; more powerful than C-u C-SPC
  (global-set-key [(control .)] 'goto-last-change))

(when (locate-library "mediawiki")
  (require 'archwiki-credentials))

(when (locate-library "helm")
  (helm-mode t)
  (defalias 'imenu 'helm-imenu)
  (defalias 'occur 'helm-occur)
  (global-set-key "\M-x" 'helm-M-x)
  (global-set-key "\C-c\C-c\M-x" 'execute-extended-command))

(when (locate-library "smex")
  (global-set-key "\M-X" 'smex)
  (global-set-key "\C-c\M-x" 'smex-major-mode-commands))

;; (when (locate-library "guru-mode")
;;  (guru-global-mode t))

(when (locate-library "undo-tree")
  (undo-tree-mode t)
  (defalias 'undo 'undo-tree-undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "C-+") 'undo-tree-redo))

(when (and (locate-library "js2-mode") (locate-library "ac-js2"))
  (defalias 'javascript-mode 'js2-mode)
  (add-hook 'js2-mode-hook 'ac-js2-mode))

(when (locate-library "web-mode")
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(when (and (locate-library "key-chord") (locate-library "ace-jump-mode"))
  (key-chord-define-global "jj" 'ace-jump-word-mode)
  (key-chord-define-global "jk" 'ace-jump-char-mode)
  (key-chord-define-global "jl" 'ace-jump-line-mode)
  (key-chord-mode 1))

(when (locate-library "idle-highlight")
  (add-something-to-mode-hooks my-programming-alist 'idle-highlight))

(when (locate-library "pkgbuild-mode")
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))

(when (locate-library "org")
  (defun my-org-hook ()
    (progn
      (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
      (when (locate-library "drag-stuff")
        (drag-stuff-global-mode -1))))
  (setq org-src-fontify-natively t)
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("INPROGRESS" . "orange")
          ("DONE" . "green")
          ("POSTPONED" . "blue")))
  (when (locate-library "org2blog")
    (require 'org2blog-autoloads)
    (require 'wordpress-credentials))
  (add-hook 'org-mode-hook 'my-org-hook))

(when (locate-library "markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

(when (locate-library "magit")
  (global-set-key "\C-xg" 'magit-status))

(when (locate-library "yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1))

(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
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

;; -------------------------------------------------------------
;; -------------------------------------------------------------
;; native libraries

(when (locate-library "ido")
  (ido-mode t)
  (icomplete-mode t)
  (setq-default ido-enable-flex-matching t)
  (setq-default ido-enable-last-directory-history t)
  (setq-default confirm-nonexistent-file-or-buffer nil))

(when (locate-library "recentf")
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (setq recentf-save-file (concat user-emacs-directory "recentf"))
  (global-set-key "\C-x\C-r" 'recentf-open-files))

(when (locate-library "savehist")
  (require 'savehist)
  (savehist-mode t))

(when (locate-library "saveplace")
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "saved-places")))

(when (locate-library "uniquify")
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

(when (locate-library "sh-mode")
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode)))

(when (locate-library "compile")
  (setq compilation-read-command nil)
  (add-hook 'c++-mode-hook
            (lambda () (setq compile-command (concat "g++ \""
                                                     (buffer-file-name)
                                                     "\" -o \""
                                                     (file-name-sans-extension buffer-file-name)
                                                     "\" -Wall -g -O2")))))

(defun reload-dot-emacs ()
  "Reload your ~/.emacs file."
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "<f2>") 'reload-dot-emacs)

(defun replace-last-sexp ()
  "Eval in place"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(global-set-key "\C-c\C-e" 'replace-last-sexp)

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
(global-set-key [f9]            'comment-or-uncomment-region)
(global-set-key [f12]           'cleanup-buffer)
(global-set-key (kbd "C-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/.mygit/icpc-journal/lists.org")))
 '(safe-local-variable-values (quote ((require-final-newline))))
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
