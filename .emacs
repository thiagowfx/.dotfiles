;; -*- lisp -*-
(load-theme 'wombat t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snapshot of my custom packages
;; upgrade them with U x on the *Packages* buffer
(fset 'package-get-snapshot-line [?\C-a ?\C-s ?  ?\C-s ?\C-m ?\C-  ?\C-e ?\C-w escape ?  backspace ?\C-a ?\; ?\; ?  ?- ?\C-d ?\C-a ?\C-n])
;; - ac-c-headers
;; - ac-helm
;; - ac-js2
;; - ace-jump-mode
;; - auto-complete
;; - auto-complete-c...
;; - blank-mode
;; - cmake-mode
;; - company
;; - dash
;; - deft
;; - epl
;; - f
;; - flycheck
;; - fuzzy
;; - fuzzy-match
;; - git-commit-mode
;; - git-rebase-mode
;; - gitignore-mode
;; - graphviz-dot-mode
;; - helm
;; - helm-company
;; - helm-git
;; - help+
;; - icomplete+
;; - ido-better-flex
;; - ido-hacks
;; - imenu+
;; - info+
;; - js2-mode
;; - json-mode
;; - magit
;; - markdown-mode
;; - markdown-mode+
;; - mode-icons
;; - mouse+
;; - org
;; - paredit
;; - pkg-info
;; - pkgbuild-mode
;; - popup
;; - powerline
;; - projectile
;; - rainbow-mode
;; - s
;; - scss-mode
;; - shell-command
;; - simple-httpd
;; - skewer-mode
;; - smart-mode-line
;; - smartparens
;; - smex
;; - undo-tree
;; - web-mode
;; - xclip
;; - yaml-mode
;; - yasnippet
;;                             emacs-live
;;                             semantic-mode
;;                             gccsense
;;                             global
;;                             yasnippet(yas)
;;                             emacs-eclim
;;                             ac-slime
;;                             gist
;;                             htmlize
;;                             marmalade
;;                             nrepl
;;                             o-blog
;;                             paredit
;;                             restclient
;; skewer-mode
;; jss
;;                             rvm) "Default packages")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Topics
;;
;; macros
;; - C-( or F3, later C-) or F4, execute with C-x e
;;
;; narrowing:
;; - C-x n n: narrow between point and mark
;; - C-x n w: un-narrow (~widen)
;;
;; IDE paradise:
;; - Imenu :: Go directly to a function definition.
;; - Occur :: Overview of your file (just choose a keyword)
;;
;; registers and bookmarks:
;; - C-x r b         :: jump to bookmark
;; - C-x r i <label> :: insert register <label> to region
;; - C-x r l         :: list bookmarks
;; - C-x r s <label> :: save region to register <label>
;; - C-x r SPC <label> :: save point position to register
;; - C-x r j <label> :: go to saved point position
;;
;; useful (built-in) modes and commands
;; - follow-mode     :: scroll buffers (equal)
;; - scroll-all-mode :: scroll buffers (different)
;; - compare-windows :: diff two open buffers on point
;; - top-level       :: get out of the minibuffer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Homepages
;; - http://www.emacsrocks.com/
;; - http://www.emacswiki.org/
;;
;;
;; References
;; - http://www.emacswiki.org/emacs/EmacsCrashCode/
;; - https://github.com/technomancy/better-defaults/
;; - http://www.aaronbedra.com/emacs.d/ ;; teaches how to install all (missing) packages from a given list
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load-path :: ~/.emacs.d
;; If not using MELPA/package.el, please do the following to manually load a package:
;; (add-to-list 'load-path "/path/to/your/package")
;; (require 'package-provide-name)
;; If creating a new config, please add the following to avoid errors when the package is not present:
;; (when (locate-library "package-name") (package-configs-here))
;; OR (when (require 'provide-name nil 'noerror) (package-configs-here))
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories
;; See `list-packages` and `package-install`
(when (>= emacs-major-version 24)
  (package-initialize)
  (setq-default package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                                   ("melpa" . "http://melpa.milkbox.net/packages/")
                                   ("gnu" . "http://elpa.gnu.org/packages/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DO NOT create backups
(setq make-backup-files nil) ;; do not create *~ files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard Stuff
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;; Completion: { | | | }
(ido-mode t)
(setq-default ido-enable-flex-matching t
              ido-enable-last-directory-history t
              confirm-nonexistent-file-or-buffer nil) ;; disable annoying confirmation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icomplete
;; Completion: {,,,}
(icomplete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline stats
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline
;; modern modeline
;; (when (locate-library "powerline")
;;   (powerline-default-theme))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-mode-line
;; better modeline
(when (locate-library "smart-mode-line")
  ;;   (setq sml/theme 'dark)
  (sml/setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-icons-mode
;; replaces some major modes with icons on the modeline
(when (locate-library "mode-icons")
  (mode-icons-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual-line -- Better C-a C-e C-k for very long line
;; Toggle it interactively with M-x visual-line-mode
;; See also M-x toggle-truncate-lines (line wrapping)
;; (global-visual-line-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell with aspell
;; (setq-default flyspell-issue-welcome-flag nil
;;               ispell-program-name "/usr/bin/aspell"
;;               ispell-list-command "list")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;; (setq-default flycheck-highlighting-mode 'lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet (yas)
;; Activate it globally
;; (when (locate-library "yasnippet")
;;   (yas-global-mode 1)
;;   (yas-reload-all)
;;   (global-set-key "\C-c\M-\t" 'yas/expand))
;; If you want to load it on a per-buffer basis:
;; Use (yas-minor-mode) and (yas-reload-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev
;; (info "(emacs) Abbrevs"
;; sample use of emacs abbreviation feature
;; (define-abbrev-table 'global-abbrev-table
;;   '(
;;     ("8abbrev" "abbreviation")
;;     ))
;; ;; stop asking whether to save newly added abbrev when quitting emacs
;; (setq save-abbrevs nil)
;; ;; turn on abbrev mode globally
;; (setq-default abbrev-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile :: Manages projects
;; https://github.com/bbatsov/projectile
;; Use C-c p as a prefix for everything,
;; If you want to change it... (setq projectile-keymap-prefix (kbd "C-c C-p"))
;; add .projectile file to the root of your project folder to enable it
;; if using a git, hg, etc project, this is not needed
;; to turn projectile on for every folder, just add
;; (setq projectile-require-project-root nil)
;; (when (locate-library "projectile")
;;  (projectile-global-mode))
;; per-mode basis
;; (add-hook 'ruby-mode-hook 'projectile-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deft
;; Simple integrated note taking for emacs
(when (locate-library "deft")
  (setq-default deft-directory "~/Dropbox/deft"
                deft-use-filename-as-title t
                deft-extension "org"
                deft-text-mode 'org-mode)
  (global-set-key [f8] 'deft))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabs vs spaces war
(setq-default indent-tabs-mode nil) ;; only spaces, please
(setq-default tab-width 4)
(setq-default standard-indent 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windmove
;; use shift + arrow key to switch buffers
(windmove-default-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subword mode
;; treat a word camel-case like
(global-subword-mode t)

;; change-case commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens :: autocloses parenthesis
;; Looks better than autopair
(when (locate-library "smartparens")
  (setq sp-autoescape-string-quote nil)
  (smartparens-global-mode t))

;; highlight matching parenthesis
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winner mode
;; use C-c <left> and C-c <right> to cycle through window views
(winner-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saveplace
(require 'saveplace) ;; do not remove this
(setq-default save-place t
              save-place-file (concat user-emacs-directory "places")
              recentf-max-saved-items 150)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify :: better buffer names
(require 'uniquify) ;; do not remove this
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-ignore-buffers-re "^\\*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll
(setq scroll-margin 3
      scroll-conservatively 3
      scroll-preserve-screen-position t) ;; preserve screen pos with C-v/M-v


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding (hiding)
;;- Use C-c @ C-c to toggle it (on a function)
;; (add-hook 'c++-mode-hook (lambda () (hs-minor-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup / Interface / Default Behavior
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")
(setq-default frame-title-format (concat "%b - " (message "%s@emacs" (replace-regexp-in-string "\n$" "" (shell-command-to-string "whoami")))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode t)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'exile) ;; mouse gets out of the way
(global-linum-mode t) ;; show line numbers, toggle interactively with M-x linum-mode
(setq-default indicate-empty-lines t) ;; vim like
(setq visible-bell t)     ;; flash the frame to represent a bell
(setq use-dialog-box nil) ;; use the echo area for everything
(setq echo-keystrokes 0.1) ;; display keystrokes on the minibuffer as soon as possible
;; (delete-selection-mode t) ;; delete text on the region after inserting any character
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq-default inhibit-debugger t) ;; disable (emacs) from autoentering in the debugger
(setq read-file-name-completion-ignore-case t)
(setq enable-recursive-minibuffers t) ;; stack minibuffers, exit with `top-level`
(setq read-buffer-completion-ignore-case t) ;; Ignore case when completing buffer names
(setq require-final-newline t)
(setq case-fold-search t)   ;; make searches case insensitive
(setq vc-follow-symlinks t) ;; do not ask for symlink confirmations
(global-auto-revert-mode t) ;; autoloads modified files outside emacs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode
;; Use it instead of the built-in javascript-mode
(when (locate-library "js2-mode")
  (defalias 'javascript-mode 'js2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
(add-hook 'c++-mode-hook (lambda () (setq compile-command (concat "g++ \""
                                                                  (buffer-file-name)
                                                                  "\" -o \""
                                                                  (file-name-sans-extension buffer-file-name)
                                                                  "\" -Wall -Wextra -g -O2"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arch Linux PKGBUILD Mode
(when (locate-library "pkgbuild-mode")
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown Mode
;; <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
(when (locate-library "markdown-mode")
  ;; (add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zsh
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex
(when (fboundp 'smex)
  ;; (smex-initialize) ;; can be omitted, loaded later (interactively)
  (global-set-key "\M-x"        'smex)
  (global-set-key "\M-X"        'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'execute-extended-command)) ;; the old M-x


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace-jump-mode
(when (locate-library "ace-jump-mode")
  (define-key global-map (kbd "C-0") 'ace-jump-mode)
  ;; default order (cycling)
  (setq-default ace-jump-mode-submode-list '(ace-jump-char-mode
                                             ace-jump-line-mode
                                             ace-jump-word-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete-mode
;; (when (and (locate-library "auto-complete")
;;            (locate-library "popup"))
;;   (define-key global-map "\M-\t" 'auto-complete)) ;; calling it manually
;;   (require 'auto-complete-config)
;;   (ac-config-default)
;;   (setq-default ac-auto-start 3) ;; how many chars to auto-activate AC
;;   (global-auto-complete-mode t)
;;   (add-to-list 'ac-sources 'ac-source-abbrev)
;;   (add-to-list 'ac-sources 'ac-source-dictionary)
;;   (add-to-list 'ac-sources 'ac-source-yasnippet)
;;   (add-to-list 'ac-sources 'ac-source-words-in-buffer)
;;   (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-mode
;; Autocompletes your code
;; You may want to bind: company-complete
;; (when (locate-library "company")
;;   (add-hook 'after-init-hook 'global-company-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icicles
;; Minibuffer completion framework, almost everywhere
;; Use TAB or S-TAB to complete
;; Use C-( and M-( to cycle through the different completion methods
;; (when (locate-library "icicles")
;;   (icy-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm :: minibuffer autocompletion
(when (locate-library "helm")
  (helm-mode t))
;; You can also use `helm-mini`


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-company
(when (and (locate-library "helm")
           (locate-library "company"))
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo-tree
;; Adds better undo support, plus redo
(when (locate-library "undo-tree")
  (undo-tree-mode t)
  (defalias 'undo 'undo-tree-undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-+") 'redo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags, Etags, Gtags
;; http://www.svi.nl/EmacsProgrammingTips
;; (setq tags-table-list (list "./" "./../" "./../../" "./../../../" (getenv "DEVELOP")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmarks
;; M-x bookmark-...
(setq bookmark-default-file  (concat user-emacs-directory "bookmarks")) ;; emacs 23 compatibility
(defalias 'bookmarks-menu-list 'bookmarks-bmenu-list) ;; easier to remember
;; switch to bookmarks buffer on startup
;; (bookmark-bmenu-list)
;; (switch-to-buffer "*Bookmark List*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;; manages your recent (acessed) files
(setq-default recentf-save-file (concat user-emacs-directory "recentf")
              recentf-max-saved-items 150
              recentf-max-menu-items 25)
(recentf-mode t)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key "\C-x\C-r" 'recentf-open-files)
(global-set-key "\C-c\C-x\C-r" 'ido-choose-from-recentf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reload-dot-emacs
(defun reload-dot-emacs ()
  "Reload your ~/.emacs file."
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "<f2>")    'reload-dot-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec-program-with-IO
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace-last-sexp
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
(global-set-key [f12] 'cleanup-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keystrokes/keybindings
;; Templates:
;; - RET, "\M-g", [C-tab], (kbd "M-g"), [f1], (kbd "<f1>"), [?\C-\t], (kbd "<C-S-iso-lefttab>")
(global-unset-key "\C-xf")
(global-set-key "\M-g"          'goto-line)
(global-set-key (kbd "RET")     'newline-and-indent) ;; C-j like
(global-set-key [?\C-\t]        'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>")  (lambda () (interactive) (other-window -1)))
(global-set-key [f5]            'compile)
(global-set-key [f9]            'comment-or-uncomment-region)
(global-set-key (kbd "C-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-xrq"        'save-buffers-kill-terminal)


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
(put 'narrow-to-region 'disabled nil)
