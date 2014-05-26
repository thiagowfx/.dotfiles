;; -*- emacs-lisp -*-

(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(when (>= emacs-major-version 24)
  (load-theme 'wombat t)
  (setq default-frame-alist '((cursor-color . "white")))
  (set-cursor-color "white")
  (set-face-attribute 'default nil :height 100)
  (blink-cursor-mode -1)

  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")))

(defun add-something-to-mode-hooks (mode-list something)
  "Helper function to add a callback to multiple hooks.
Example usage: (add-something-to-mode-hooks my-programming-alist 'idle-highlight))"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

(defun replace-last-sexp ()
  "Eval inplace."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(global-set-key "\C-c\C-x\C-e" 'replace-last-sexp)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key "\C-c\C-xk" 'kill-all-buffers)

(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))
(global-set-key [f12] 'cleanup-buffer)

;; keystrokes/keybindings - RET, "\M-g", [C-tab], (kbd "M-g"), [f1], (kbd "<f1>"), [?\C-\t], (kbd "<C-S-iso-lefttab>")
(global-set-key "\M-g"          'goto-line)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key [f5]            'compile)
(global-set-key [f9]            'comment-or-uncomment-region)
(global-set-key (kbd "C-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-unset-key "\C-z")

;; Miscellaneous
(prefer-coding-system 'utf-8)
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(mouse-avoidance-mode 'exile)
(setq indicate-empty-lines t)
(setq visible-bell t)
(setq echo-keystrokes 0.1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t) ; stack minibuffers, exit with `top-level`
(setq read-buffer-completion-ignore-case t)
(setq require-final-newline t)
(setq case-fold-search t)    ; make searches case insensitive
(setq vc-follow-symlinks t)  ; do not ask for symlink confirmations

(progn
  ;; Emacs frame appearance
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (line-number-mode t)
  (setq frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami"))))))

(progn
  ;; Backup files
  (setq make-backup-files nil)
  (setq backup-inhibited t)
  (setq auto-save-default nil))

;; --------------------------------------------------
;; external libraries
;; -------------------------------------------------
;; (when (locate-library "git-auto-commit-mode")
;;   (setq gac-automatically-push-p t))

(when (locate-library "smart-mode-line")
  (sml/setup))

(when (locate-library "mode-icons")
  (mode-icons-mode))

(when (locate-library "goto-chg") ; more powerful than C-u C-SPC
  (global-set-key [(control .)] 'goto-last-change))

(when (locate-library "smex")
  (global-set-key "\M-x" 'smex)
  (global-set-key "\M-X" 'smex-major-mode-commands)
  (global-set-key "\C-c\M-x" 'execute-extended-command))

(when (locate-library "undo-tree")
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "C-+") 'undo-tree-redo))

(when (locate-library "web-mode")
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))))

(when (and (locate-library "key-chord") (locate-library "ace-jump-mode"))
  (key-chord-define-global "jk" 'ace-jump-char-mode)
  (key-chord-define-global "jl" 'ace-jump-line-mode)
  (key-chord-mode 1))

(when (locate-library "pkgbuild-mode")
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))

;; orgmode
(when (locate-library "org")
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-c\C-l" 'org-insert-link)
  (setq org-src-fontify-natively t)
  (setq org-return-follows-link t) ;; alt: C-c C-o
  (setq org-hierarchical-todo-statistics t)
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "|" "DONE" "PERFECT")))
  (setq org-directory (getenv "ORGHOME"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("b" "open-bookmarks" item (file+headline "~/mygit/open-bookmarks/README.org" "ORGCAPTURE")
           "- %?\n %i\n")
          ("t" "TODO" entry (file "")
           "* TODO %?\n  %i\n")
          ("p" "plain" item (file "")
           "- %?\n %i\n")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROGRESS" . "orange")
          ("DONE" . "green")
          ("PERFECT" . "salmon")))
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (when (locate-library "org2blog")
    (require 'org2blog-autoloads)
    (require 'wordpress-credentials)))

(when (locate-library "markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

(when (locate-library "magit")
  (global-set-key "\C-cg" 'magit-status))

(when (locate-library "yasnippet")
  (require 'yasnippet)
  (yas-global-mode t))

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

;; -------------------------------------------------------------
;; built-in/native libraries
;; -------------------------------------------------------------
(when (locate-library "server")
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(when (locate-library "autorevert")
  (global-auto-revert-mode t))

(when (locate-library "paren")
  (show-paren-mode t))

(when (locate-library "indent")
  (setq indent-tabs-mode nil)
  (setq standard-indent 2)
  (setq tab-width 2))

(when (locate-library "ibuffer")
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(when (locate-library "windmove")
  (windmove-default-keybindings))

(when (locate-library "winner")
  (winner-mode t))

(when (locate-library "fill")
  ;; (auto-fill-mode t)
  (setq fill-column 72))

(when (locate-library "subword")
  (global-subword-mode t))

(when (locate-library "linum")
  (global-linum-mode t))

(when (locate-library "bookmark")
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")))

(when (locate-library "eldoc")
  (add-something-to-mode-hooks '(emacs-lisp lisp) 'turn-on-eldoc-mode))

(when (locate-library "ido")
  (ido-mode t)
  (setq-default ido-enable-flex-matching t)
  (setq-default ido-enable-last-directory-history t)
  (setq-default confirm-nonexistent-file-or-buffer nil))

(when (locate-library "icomplete")
  (icomplete-mode t))

(when (locate-library "recentf")
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (setq recentf-save-file (concat user-emacs-directory "recentf"))
  (global-set-key "\C-c\C-r" 'recentf-open-files))

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
            (lambda () (setq compile-command
			     (format "g++ %s %s -o %s"
				     "-g -O2 -Wall"
				     (buffer-file-name)
				     (file-name-sans-extension buffer-file-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/MASTERTODO.org" "~/Dropbox/org/MASTERICPC.org" "~/Dropbox/org/raytracer.org")))
 '(paradox-github-token t)
 '(safe-local-variable-values (quote ((eval setq-default gac-automatically-push-p t) (require-final-newline))))
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
