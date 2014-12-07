(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.1)
 '(ac-comphist-file "/home/thiago/.emacs.d/ac-comphist-file.dat")
 '(ac-delay 0.01)
 '(ac-quick-help-delay 0.5)
 '(ac-quick-help-height 25)
 '(auto-revert-check-vc-info t)
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/chromium")
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-auto-jump-to-first-error t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("dd43c9f997208c61ce0f4855932cc20a57ae2f37fe2ced218dace5c8c321d1e8" default)))
 '(echo-keystrokes 0.1)
 '(epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
 '(fill-column 72)
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-linum-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-idle-breadcrumbs-mode t nil (semantic/idle))
 '(global-semantic-idle-scheduler-mode t)
 '(global-semantic-show-unmatched-syntax-mode t)
 '(global-semantic-stickyfunc-mode t)
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
 '(keyboard-coding-system (quote utf-8-unix))
 '(magit-use-overlays nil)
 '(major-mode (quote org-mode))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Dropbox/org/csf-next.org")))
 '(org-confirm-babel-evaluate nil)
 '(org-crypt-key "A905373C")
 '(org-directory "/home/thiago/Dropbox/org")
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "turquoise")
     ("PROGRESS" . "slate blue")
     ("TROUBLE" . "dark red")
     ("DONE" . "forest green"))))
 '(org-todo-keywords (quote ((sequence "TODO" "PROGRESS" "TROUBLE" "|" "DONE"))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(read-buffer-completion-ignore-case t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf-file")
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval setq-default gac-automatically-push-p t)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)
     (require-final-newline . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/save-places-file")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(semantic-mode t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator "/")
 '(user-full-name "Thiago Perrotta")
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

(require 'comint)
(require 'iso-transl)

(defun cleanup-buffer ()
  "Buffer cleaning, performing a bunch of operations on the whitespace content of it."
  (interactive)
  (save-excursion
    (message "Cleaning buffer...")
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (message "Buffer cleaned!")))

(defun load-my-packages ()
  "Load el-get packages."
  (interactive)
  (message "Loading packages...")
  (el-get 'sync el-get-wanted-packages)
  (el-get-cleanup el-get-wanted-packages)
  (message "Packages loaded!"))

;; function mappings
(global-set-key (kbd "<f2>") 'load-my-packages)
(global-set-key (kbd "<f3>") 'cleanup-buffer)

;; native mappings
(global-set-key (kbd "C-x TAB") 'auto-complete)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;; annoying maps
(global-unset-key (kbd "C-z"))

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))

(el-get 'sync)

(setq el-get-sources
      '(

        ;; default
        (:name color-theme-almost-monokai
               :after (color-theme-almost-monokai))

        (:name flycheck
               :after (add-hook 'after-init-hook #'global-flycheck-mode))

        (:name multiple-cursors
               :after (progn
                        (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                        (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                        (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                        (global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
                        (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
                        ))

        (:name org-mode
               :after (progn
                        (require 'org-crypt)
                        (global-set-key (kbd "C-c a") 'org-agenda)
                        (org-crypt-use-before-save-magic)
                        (setq org2blog/wp-blog-alist
                              '(("Everyday Serendipity"
                                 :url "http://thiagoperrotta.wordpress.com/xmlrpc.php"
                                 :username "thiagowfx"
                                 :default-title "Title"
                                 :default-categories ("")
                                 :tags-as-categories nil)))
                        (add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
                        ))

        (:name projectile
               :after (projectile-global-mode))

        (:name smex
               :after (global-set-key (kbd "M-x") 'smex))

        (:name undo-tree
               :after (global-set-key (kbd "C-+") 'undo-tree-redo))


        ;; extras


        ;; more extras


        ;; more extras -- C++
        (:name function-args
               :after (progn
                        (require 'function-args)
                        (fa-config-default)
                        (define-key c-mode-map  [(control tab)] 'moo-complete)
                        (define-key c++-mode-map  [(control tab)] 'moo-complete)
                        (define-key c-mode-map (kbd "M-o")  'fa-show)
                        (define-key c++-mode-map (kbd "M-o")  'fa-show)
                        ))
        ))

(defvar el-get-wanted-packages
  '(
    ;; default
    auto-complete
    color-theme-almost-monokai
    evil
    flycheck
    magit
    multiple-cursors
    org-mode
    projectile
    smex
    undo-tree
    
    ;; extras
    cmake-mode
    fic-mode
    markdown-mode
    org2blog
    pkgbuild-mode

    ;; more extras -- general

    ;; more extras -- C++
    function-args
    ))

(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
