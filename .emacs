; -*- lisp -*-

;; Packages
; emacs-pkgbuild-mode (for arch linux)
; markdown-mode

;; Emacs references
; - Load your .emacs file while in emacs M-x load-file RET ~/.emacs
; - Alternatively, ~/.emacs.d/init.el
; - http://www.emacswiki.org/emacs/
; - http://www.emacsrocks.com/
; - Learning GNU Emacs (O'Reilly Media)
; - Writing GNU Emacs Extensions (O'Reilly Media)
; - The Emacs Reference Manual (Richard Stallman)


;; set the load path, add everything under ~/.emacs.d to it
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; set the repos
(when (file-exists-p "~/.emacs.d/elpa/package.el")
  (when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
    (package-initialize)))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq url-http-attempt-keepalives nil)


;; Modeline
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; Minibuffer
(file-name-shadow-mode t)                    ;; be smart about filenames in minibuffer
(icomplete-mode t)                           ;; autoactivate icomplete-mode

;; Title
(setq-default frame-title-format ;; change title from a frame - X Only
	      '(:eval (format "%s : %s@%s"
			      (file-name-nondirectory (or (buffer-file-name) default-directory))
			      (or (file-remote-p default-directory 'user) user-login-name)
			      (or (file-remote-p default-directory 'host) system-name))))

;; Hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; C
(add-hook 'c-mode-hook
	  (lambda () (setq compile-command
			   (concat "gcc \""
				   (buffer-file-name)
				   "\" -o \""
				   (file-name-sans-extension buffer-file-name)
				   "\" -Wall -Wextra -g"))))
;; C++
(add-hook 'c++-mode-hook (lambda ()
			   (setq compile-command
				 (concat "g++ \""
					 (buffer-file-name)
					 "\" -o \""
					 (file-name-sans-extension buffer-file-name)
					 "\" -Wall -Wextra -g"))))

;; Tweaks
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-margin 0                        
      scroll-conservatively 100000           
      scroll-up-aggressively 0               
      scroll-down-aggressively 0             
      scroll-preserve-screen-position t ;; preserve screen pos with C-v/M-v 
      compilation-read-command nil ;; autocompile without prompting the user, unless you give it a prefix argument
      default-indicate-empty-lines t ;; vi like
      make-backup-files nil	     ;; file.ext~
      mouse-yank-at-point t
      require-final-newline t
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      vc-follow-symlinks t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message "")                 

;; Custom Modes
(global-font-lock-mode t) ;; syntax highlight everywhere
(menu-bar-mode -1)	  ;; hide the menu bar
(winner-mode t) ;; use C-c <left> to restore the previous window view

;; auto-complete mode, see M-/ shortcut also
(require 'auto-complete)
(require 'popup)
(setq ac-modes '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))
(global-auto-complete-mode t)

;; recent files, to save recently used files
(require 'recentf)   
(setq recentf-save-file               "~/.emacs.d/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)

(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; ido-mode stuff - powerful mode for find-file and switch-to-buffer - interactively do things
(require 'ido)
(ido-mode 'both)
(setq ido-save-directory-list-file "~/.ido.last"
      ido-ignore-buffers               ;; ignore these guys
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
	"^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
      ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
      ido-case-fold  t                    ;; be case-insensitive
      ido-enable-last-directory-history t ;; remember last used dirs
      ido-max-work-directory-list 30	  ;; should be enough
      ido-max-work-file-list      50	  ;; remember many
      ido-use-filename-at-point nil       ;; don't use filename at point (annoying)
      ido-use-url-at-point nil            ;; don't use url at point (annoying)
      ido-enable-flex-matching nil        ;; don't try to be too smart
      ido-max-prospects 8                 ;; don't spam my minibuffer
      ido-confirm-unique-completion t)    ;; wait for RET, even with unique completion
(setq confirm-nonexistent-file-or-buffer nil) ;; disable annoying confirmation


;; File extensions

;; Arch Linux PKGBUILDs
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; AuCTeX stuff
;; (setq TeX-PDF-mode t)                        ;; get AUCTeX to work in PDF insted of dvi mode
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq TeX-save-query nil)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (require 'ac-math) ;; ac-math for LaTeX
;; (add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of `latex-mode`
;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;;   (setq ac-sources (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;; 			   ac-sources)))
;; (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
;; (setq TeX-view-program-list '(("Evince" "zathura %o"))) ;; set zathura to view documents


;; defuns

(defun reload-dot-emacs ()
  "Reload your ~/.emacs file."
  (interactive)
  (load-file "~/.emacs"))

(defun toggle-fullscreen (&optional f)
  "Toggle Fullscreen -- GNU/Linux Only"
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

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
;;   "Notify that the compilation is finished,
;; close the *compilation* buffer if the compilation is successful,
;; and set the focus back to Emacs frame"
;;   (if (string-match "^finished" msg)
;;       (progn
;; 	(delete-windows-on buffer)
;; 	(tooltip-show "\n Compilation Successful!!! ;-) \n "))
;;     (tooltip-show "\n Compilation Failed =/ mimi \n "))
;;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;   (select-frame-set-input-focus current-frame))
;; (add-to-list 'compilation-finish-functions 'notify-compilation-result)


;; keystrokes

;; templates 
;; (kbd "M-g")
;; [f1]
;; (kbd "<f1>")

(global-set-key    "\M-g"          'goto-line)                   
(global-set-key    (kbd "<f3>")    'reload-dot-emacs)
(global-set-key    (kbd "RET")     'newline-and-indent)	;; C-j like; alt: reindent-then-newline-and-indent
(global-set-key    [C-tab]         'other-window)
(global-set-key    [f5]            'compile)
(global-set-key    [f9]            'comment-or-uncomment-region)
(global-set-key    [f11]           'toggle-fullscreen)


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
