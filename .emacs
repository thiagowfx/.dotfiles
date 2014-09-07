;; -*- emacs-lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/google-chrome-stable")
 '(column-number-mode t)
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
 '(menu-bar-mode nil)
 '(org-confirm-babel-evaluate nil)
 '(org-crypt-key "A905373C")
 '(org-directory "/home/thiago/Dropbox/org")
 '(org-mode-hook (quote ((lambda nil (smartparens-mode -1)) (lambda nil (auto-fill-mode t)) #[nil "\300\301\302\303\304$\207" [org-add-hook before-save-hook org-encrypt-entries nil t] 5] #[nil "\300\301\302\303\304$\207" [org-add-hook change-major-mode-hook org-show-block-all append local] 5] #[nil "\300\301\302\303\304$\207" [org-add-hook change-major-mode-hook org-babel-show-result-all append local] 5] org-babel-result-hide-spec org-babel-hide-all-hashes (lambda nil (progn (require (quote eldoc)) (require (quote eldoc-extension)) (setq eldoc-idle-delay 0) (setq eldoc-argument-case (quote eldoc-argument-list)) (turn-on-eldoc-mode))))))
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(org-todo-keyword-faces (quote (("TODO" . "turquoise") ("PROGRESS" . "slate blue") ("TROUBLE" . "dark red") ("DONE" . "forest green"))))
 '(org-todo-keywords (quote ((sequence "TODO" "PROGRESS" "TROUBLE" "|" "DONE"))))
 '(read-buffer-completion-ignore-case t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf-file")
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((eval setq-default gac-automatically-push-p t) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face tabs trailing lines-tail) (require-final-newline . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/save-places-file")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
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
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))
(global-set-key [C-tab] 'cleanup-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x c") 'save-buffers-kill-terminal)
(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
(global-unset-key (kbd "C-z"))

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name color-theme-almost-monokai
               :after (color-theme-almost-monokai))
        (:name flycheck
               :after (progn
                        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                        (add-hook 'after-init-hook #'global-flycheck-mode)))
        (:name git-gutter
               :after (progn
                        (global-git-gutter-mode t)
                        (git-gutter:linum-setup)))
        (:name ido-vertical-mode
               :after (ido-vertical-mode))
        (:name init-eldoc
               :after (require 'init-eldoc))
        (:name multiple-cursors
               :after (progn
                        (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                        (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                        (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                        (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
                        (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)))
        (:name org2blog
               :after (progn
                        (require 'org2blog-autoloads)
                        (add-to-list 'load-path (concat user-emacs-directory "credentials"))
                        ;; (require 'wordpress-credentials)
))
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
               :after (global-set-key (kbd "M-x") 'smex))))
(setq mine/wanted-packages '(bookmark+ cmake-mode color-theme-almost-monokai dired+ flycheck git-gutter icomplete+ ido-vertical-mode init-eldoc markdown-mode multiple-cursors org-mode org2blog pkgbuild-mode powerline projectile redo+ smartparens smex))
(el-get-cleanup mine/wanted-packages)
(el-get 'sync mine/wanted-packages)
