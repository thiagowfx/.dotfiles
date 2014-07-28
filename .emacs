;; -*- lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face tabs trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; el-get bootstrapping
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory (concat user-emacs-directory "el-get-init-files"))

(package-initialize)
(add-to-list 'load-path (concat user-emacs-directory "credentials"))
;;(el-get 'sync)

;; extra recipes for packages yet unknown to el-get
(setq el-get-sources
      '((:name ace-jump-mode
               :after (global-set-key (kbd "C-z") 'ace-jump-mode))
        (:name auto-complete
               :after (progn
                        (require 'auto-complete)
                        (require 'auto-complete-config)
                        (ac-config-default)
                        (ac-flyspell-workaround)
                        (global-auto-complete-mode t)
                        (ac-set-trigger-key "TAB")
                        (global-set-key (kbd "C-c <tab>") 'auto-complete)
                        (defun ac-common-setup () (setq ac-sources
                                                        (append
                                                         ac-sources '(ac-source-filename
                                                                      ac-source-files-in-current-dir
                                                                      ac-source-imenu
                                                                      ac-source-words-in-buffer))))))
        (:name auto-yasnippet
               :type elpa
               :after (progn
                        (global-set-key (kbd "C-\(") 'aya-create)
                        (global-set-key (kbd "C-\)") 'aya-expand)))
        (:name autopair
               :after (autopair-global-mode))
        (:name drag-stuff
               :after (progn
                        (drag-stuff-global-mode)
                        (add-hook 'org-mode-hook '(lambda () (drag-stuff-mode -1)))))
        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))
        (:name fixmee
               :after (progn
                        (global-fixmee-mode t)
                        (global-set-key (kbd "C-c f") 'fixmee-goto-nextmost-urgent)))
        (:name flycheck
               :after (add-hook 'after-init-hook #'global-flycheck-mode))
        (:name git-auto-commit-mode
               :type elpa)
        (:name go-mode
               :after (progn
                        (add-hook 'before-save-hook #'gofmt-before-save)
                        (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-d") #'godoc-at-point)))))
        (:name goto-chg
               :after (global-set-key (kbd "C-.") 'goto-last-change))
        (:name hungry-delete
               :after (progn
                        (require 'hungry-delete)
                        (global-hungry-delete-mode)))
        (:name magit
               :after (global-set-key (kbd "C-c g") 'magit-status))
        (:name mode-icons
               :type elpa
               :after (mode-icons-mode))
        (:name monokai-theme
               :type elpa
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
                        (setq org-src-fontify-natively t)
                        (setq org-directory "~/Dropbox/org")
                        (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "TROUBLE" "|" "DONE")))
                        (setq org-todo-keyword-faces '(("TODO" . "medium turquoise") ("PROGRESS" . "slate blue") ("TROUBLE" . "dark red") ("DONE" . "forest green")))
                        (require 'org-crypt)
                        (org-crypt-use-before-save-magic)
                        (setq org-tags-exclude-from-inheritance '("crypt"))
                        (setq org-crypt-key "A905373C")))
        (:name org2blog
               :after (progn
                        (require 'org2blog-autoloads)
                        (require 'wordpress-credentials)))
        (:name popwin
               :after (progn
                        (require 'popwin)
                        (popwin-mode t)))
        (:name projectile
               :after (projectile-global-mode))
        (:name rainbow-mode
               :after (add-hook 'css-mode-hook '(lambda () (rainbow-mode t))))
        (:name smart-cursor-color
               :type elpa
               :after (smart-cursor-color-mode t))
        (:name smart-mode-line
               :after (sml/setup))
        (:name smex
               :after (global-set-key (kbd "M-x") 'smex))
        (:name undo-tree
               :after (progn
                        (global-undo-tree-mode t)
                        (global-set-key (kbd "C-_") 'undo-tree-undo)
                        (global-set-key (kbd "C-+") 'undo-tree-redo)))
        (:name yasnippet
               :after (progn
                        (yas-global-mode t)
                        (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt yas-x-prompt yas-dropdown-prompt))
                        (add-hook 'yas-minor-mode-hook #'(lambda () (yas-activate-extra-mode 'perrotta-mode)))))
        ))

;; packages
(setq my:el-get-packages
      '(ace-jump-mode
        auctex
        auto-complete
        auto-complete-c-headers
        auto-yasnippet
        autopair
        bash-completion
        completing-help
        dired+
        drag-stuff
        expand-region
        fixmee
        flycheck
        flycheck-color-mode-line
        flyspell
        git-auto-commit-mode
        go-mode
        go-autocomplete
        go-eldoc
        go-imports
        go-lint
        go-oracle
        go-projectile
        go-test
        goto-chg
        hungry-delete
        init-eldoc
        js2-mode
        magit
        markdown-mode
        mode-icons
        monokai-theme
        multiple-cursors
        org-mode
        org2blog
        pkgbuild-mode
        popwin
        projectile
        rainbow-mode
        rainbow-identifiers
        smart-cursor-color
        smart-mode-line
        smex
        smooth-scroll
        smooth-scrolling
        undo-tree
        web-mode
        yasnippet
        ))
(el-get 'sync my:el-get-packages)
;;(el-get-emacswiki-refresh el-get-recipe-path-emacswiki t)

;; mine
(setq user-full-name "Thiago Perrotta"
      user-mail-address "thiagoperrotta95@gmail.com")
(setq-default major-mode 'org-mode)
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
(setq fill-column 72)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(icomplete-mode t)
(savehist-mode t)
(winner-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(global-linum-mode t)
;; (global-hl-line-mode t)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(progn
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
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
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (line-number-mode t))
(progn
  (setq indent-tabs-mode nil)
  (setq standard-indent 2)
  (setq tab-width 2))
(progn
  (require 'epa)
  (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-name-regexp-update))
(blink-cursor-mode -1)
(show-paren-mode t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(set-frame-font "Terminus-9")
(setq frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))

(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

(defun replace-last-sexp ()
  "Eval in place."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(global-set-key "\C-c\C-x\C-e" 'replace-last-sexp)
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
(global-unset-key "\C-z")
(global-unset-key "\C-\M-h")

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'"  . web-mode))

(progn
  ;; (setq compilation-read-command nil)
  (add-hook 'c++-mode-hook (lambda () (setq compile-command
                                            (format "g++ %s %s -o %s"
                                                    "-g -O2 -Wall"
                                                    (buffer-file-name)
                                                    (file-name-sans-extension buffer-file-name))))))
