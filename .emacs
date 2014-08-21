;; -*- emacs-lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes (quote ("dd43c9f997208c61ce0f4855932cc20a57ae2f37fe2ced218dace5c8c321d1e8" default)))
 '(magit-use-overlays nil)
 '(safe-local-variable-values (quote ((eval setq-default gac-automatically-push-p t) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face tabs trailing lines-tail) (require-final-newline . t))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(progn
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (line-number-mode t))

;; el-get bootstrapping
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))
(setq el-get-user-package-directory (concat user-emacs-directory "el-get-init-files"))

(package-initialize)

;; extra recipes for packages yet unknown to el-get
(setq el-get-sources
      '((:name anzu
               :after (progn
                        (global-anzu-mode t)
                        (global-set-key (kbd "M-%") 'anzu-query-replace)
                        (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)))
        (:name auto-complete
               :after (progn
                        (require 'auto-complete-config)
                        (ac-config-default)
                        (global-auto-complete-mode t)
                        (ac-set-trigger-key "TAB")
                        (global-set-key (kbd "C-x TAB") 'auto-complete)
                        (defun ac-common-setup () (setq ac-sources (append ac-sources '(ac-source-filename ac-source-files-in-current-dir ac-source-imenu ac-source-words-in-buffer))))))
        (:name drag-stuff
               :after (drag-stuff-global-mode))
        (:name emmet-mode
               :after (progn
                        (add-hook 'sgml-mode-hook 'emmet-mode)
                        (add-hook 'css-mode-hook  'emmet-mode)
                        (setq emmet-move-cursor-between-quotes t)))
        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))
        (:name fic-mode
               :after (progn
                        (require 'fic-mode)
                        (add-hook 'prog-mode-hook '(lambda () (fic-mode t)))))
        (:name flycheck
               :after (progn
                        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                        (add-hook 'after-init-hook #'global-flycheck-mode)))
	(:name git-gutter
	       :after (progn
			(global-git-gutter-mode t)
			(git-gutter:linum-setup)))
        (:name go-mode
               :after (progn
                        (add-hook 'before-save-hook #'gofmt-before-save)
                        (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-d") #'godoc-at-point)))))
	(:name help+
	       :after (require 'help+))
        (:name hlinum
               :after (hlinum-activate))
	(:name init-eldoc
	       :after (require 'init-eldoc))
        (:name magit
               :after (global-set-key (kbd "C-c g") 'magit-status))
        (:name mode-icons
               :after (mode-icons-mode))
        (:name monokai-theme
               :after (load-theme 'monokai))
        (:name multiple-cursors
               :after (progn
                        (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                        (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                        (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                        (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
                        (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)))
        (:name org-mode
               :after (progn
                        (setq-default major-mode 'org-mode)
                        (setq org-src-fontify-natively t)
                        (setq org-confirm-babel-evaluate nil)
                        (setq org-directory (concat (getenv "HOME") "/Dropbox/org"))
                        (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "TROUBLE" "|" "DONE")))
                        (setq org-todo-keyword-faces '(("TODO" . "medium turquoise") ("PROGRESS" . "slate blue") ("TROUBLE" . "dark red") ("DONE" . "forest green")))
                        (require 'org-crypt)
                        (org-crypt-use-before-save-magic)
                        (setq org-tags-exclude-from-inheritance '("crypt"))
                        (setq org-crypt-key "A905373C")
                        (add-hook 'org-mode-hook '(lambda () (auto-fill-mode t)))
                        (add-hook 'org-mode-hook '(lambda () (drag-stuff-mode -1)))
                        (add-hook 'org-mode-hook '(lambda () (smartparens-mode -1)))))
        (:name org2blog
               :after (progn
                        (require 'org2blog-autoloads)
                        (add-to-list 'load-path (concat user-emacs-directory "credentials"))
                        (require 'wordpress-credentials)))
        (:name paradox
               :type elpa)
        (:name projectile
               :after (projectile-global-mode))
        (:name smartparens
               :after (smartparens-global-mode))
        (:name smex
               :after (global-set-key (kbd "M-x") 'smex))
        (:name undo-tree
               :after (progn
                        (global-undo-tree-mode t)
                        (global-set-key (kbd "C-_") 'undo-tree-undo)
                        (global-set-key (kbd "C-+") 'undo-tree-redo)))
        (:name volatile-highlights
               :after (volatile-highlights-mode t))))

;; packages -- autoremove/cleanup: (el-get-cleanup my:el-get-packages)
(setq my:el-get-packages '(ag
                           anzu
                           auto-complete
                           bookmark+
                           cmake-mode
                           dired+
                           drag-stuff
                           emmet-mode
                           expand-region
                           fic-mode
                           flycheck
                           git-auto-commit-mode
			   git-gutter
                           go-autocomplete
                           go-mode
                           go-eldoc
                           go-projectile
			   help+
                           hlinum
                           icomplete+
			   init-eldoc
                           js2-mode
                           json-mode
                           magit
                           markdown-mode
                           mode-icons
                           monokai-theme
                           multiple-cursors
                           org-mode
                           org2blog
                           paradox
                           pkgbuild-mode
                           projectile
                           projectile-rails
                           ruby-mode
                           smartparens
                           smex
                           undo-tree
                           volatile-highlights
                           web-mode
                           yaml-mode))
(el-get 'sync my:el-get-packages)

(setq user-full-name "Thiago Perrotta"
      user-mail-address "thiagoperrotta95@gmail.com")
(setq case-fold-search t)
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq visible-bell t)
(setq indicate-empty-lines t)
(setq echo-keystrokes 0.1)
(setq enable-recursive-minibuffers t)
(setq require-final-newline t)
(setq read-buffer-completion-ignore-case t)
(setq vc-follow-symlinks t)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq-default fill-column 72)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(icomplete-mode t)
(savehist-mode t)
(winner-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(global-linum-mode t)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(progn
  (require 'saveplace)
  (setq save-place t)
  (setq save-place-file (concat user-emacs-directory "save-places-file")))
(setq zbookmark-default-file (concat user-emacs-directory "bookmarks-file"))
(progn
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/"))
(progn
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (setq recentf-save-file (concat user-emacs-directory "recentf")))
(progn
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-enable-last-directory-history t)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq ido-save-directory-list-file (concat user-emacs-directory "ido-file"))
  (setq ido-show-dot-for-dired t)
  (setq ido-default-buffer-method 'selected-window))
(progn
  (setq indent-tabs-mode nil)
  (setq standard-indent 2)
  (setq tab-width 2))
(progn
  (require 'epa)
  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-name-regexp-update))
(setq browse-url-generic-program (executable-find "google-chrome-stable") browse-url-browser-function 'browse-url-generic)
(blink-cursor-mode -1)
(show-paren-mode t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(set-frame-font "Source Code Pro 9")
(setq frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))

(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

(global-set-key [C-tab] 'cleanup-buffer)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<menu>")  'compile)
(global-set-key (kbd "C-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x g")   'goto-line)
(global-set-key (kbd "C-x c")   'save-buffers-kill-terminal)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-unset-key "\C-\M-h")

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'c++-mode-hook (lambda () (setq compile-command (format "g++ %s %s -o %s"
                                                                  "-g -O2 -Wall"
                                                                  (buffer-file-name)
                                                                  (file-name-sans-extension buffer-file-name)))))
