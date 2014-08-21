;; -*- emacs-lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(auto-save-default nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks-file")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/google-chrome-stable")
 '(compilation-always-kill t)
 '(custom-safe-themes (quote ("dd43c9f997208c61ce0f4855932cc20a57ae2f37fe2ced218dace5c8c321d1e8" default)))
 '(echo-keystrokes 0.1)
 '(epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
 '(fill-column 72)
 '(global-auto-revert-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(icomplete-mode t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido-file")
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message "thiago")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(magit-use-overlays nil)
 '(major-mode (quote org-mode))
 '(make-backup-files nil)
 '(org-confirm-babel-evaluate nil)
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(read-buffer-completion-ignore-case t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf-file")
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((eval setq-default gac-automatically-push-p t) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face tabs trailing lines-tail) (require-final-newline . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/save-places-file")
 '(savehist-mode t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(standard-indent 2)
 '(tab-width 2)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator "/")
 '(user-full-name "Thiago Barroso Perrotta")
 '(user-mail-address "thiagoperrotta95@gmail.com")
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(which-function-mode t)
 '(windmove-wrap-around t)
 '(winner-mode t)
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(progn
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (line-number-mode t)
  (blink-cursor-mode -1))

;; el-get bootstrapping
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(package-initialize)

(setq el-get-sources
      '((:name anzu
               :after (progn
                        (global-anzu-mode t)
                        (global-set-key (kbd "M-%") 'anzu-query-replace)
                        (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)))
        (:name company-mode
               :after (progn
                        (global-company-mode t)
                        (global-set-key (kbd "C-x TAB") 'company-complete)))
        (:name drag-stuff
               :after (drag-stuff-global-mode))
        (:name emmet-mode
               :after (progn
                        (add-hook 'css-mode-hook  'emmet-mode)
                        (add-hook 'sgml-mode-hook 'emmet-mode)
                        (setq emmet-move-cursor-between-quotes t)))
        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))
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
        (:name idle-highlight-mode
               (add-hook 'prog-mode-hook (lambda () (idle-hightlight-mode t))))
        (:name ido-vertical-mode
               :after (ido-vertical-mode))
        (:name ido-hacks
               :after (ido-hacks-mode))
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
                        (setq org-directory (concat (getenv "HOME") "/Dropbox/org"))
                        (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "TROUBLE" "|" "DONE")))
                        (setq org-todo-keyword-faces '(("TODO" . "turquoise") ("PROGRESS" . "slate blue") ("TROUBLE" . "dark red") ("DONE" . "forest green")))
                        (require 'epa)
                        (require 'org-crypt)
                        (org-crypt-use-before-save-magic)
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
        (:name powerline
               :after (powerline-default-theme))
        (:name projectile
               :after (projectile-global-mode))
        (:name redo+
               :after (progn
                        (require 'redo+)
                        (global-set-key (kbd "C-+") 'redo)))
        (:name smartparens
               :after (smartparens-global-mode))
        (:name smex
               :after (global-set-key (kbd "M-x") 'smex))
        (:name switch-window
               :after (global-set-key (kbd "C-x o") 'switch-window))
        (:name sublimity
               :type elpa
               :after (progn
                        (require 'sublimity)
                        (require 'sublimity-scroll)
                        (sublimity-mode t)))
        (:name volatile-highlights
               :after (volatile-highlights-mode t))
        (:name yasnippet
               :after (yas-global-mode t))))

(setq wanted-packages '(anzu ;; ok
                        bookmark+ ;; ok
                        cmake-mode ;; ok
                        company-mode
                        dired+ ;; ok
                        drag-stuff ;; ok
                        emmet-mode
                        expand-region ;; ok
                        flycheck ;; ok
                        git-auto-commit-mode
                        git-gutter ;; ok
                        go-mode ;; ok
                        go-eldoc ;; ok
                        go-projectile
                        help+
                        icomplete+ ;; ok
                        idle-highlight-mode
                        ido-hacks
                        ido-vertical-mode ;; ok
                        init-eldoc ;; ok
                        js3-mode
                        json-mode
                        magit ;; ok
                        markdown-mode ;; ok
                        mode-icons ;; ok
                        monokai-theme ;; ok
                        multiple-cursors ;; ok
                        org-mode ;; ok
                        org2blog ;; ok
                        paradox ;; ok
                        pkgbuild-mode ;; ok
                        powerline ;; ok
                        projectile ;; ok
                        projectile-rails
                        redo+ ;; ok
                        ruby-mode ;; ok
                        smartparens ;; ok
                        smex ;; ok
                        sublimity
                        switch-window
                        volatile-highlights
                        web-mode
                        yaml-mode
                        yasnippet
                        yasnippet-snippets))
(el-get-cleanup wanted-packages)
(el-get 'sync wanted-packages)

(setq backup-inhibited t)
(fset 'yes-or-no-p 'y-or-n-p)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(windmove-default-keybindings)
(set-frame-font "Source Code Pro 9")
(setq frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))

(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(global-set-key (kbd "C-S-n") (lambda() (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda() (interactive) (ignore-errors (previous-line 5))))

(global-set-key [C-tab] 'cleanup-buffer)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x g")   'goto-line)
(global-set-key (kbd "C-x c")   'save-buffers-kill-terminal)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-unset-key "\C-\M-h")

(add-to-list 'auto-mode-alist '(".bashrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".gnus" . lisp-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

(add-hook 'c++-mode-hook (lambda () (setq compile-command (format "g++ %s %s -o %s"
                                                                  "-g -O2 -Wall"
                                                                  (buffer-file-name)
                                                                  (file-name-sans-extension buffer-file-name)))))
