;; custom-minor-modes
(provide 'custom-modes)

;; Modeline
(column-number-mode t)                       ;; display the column number in the mode bar
(line-number-mode t)
(size-indication-mode t)                     ;; display size of the current file

;; Minibuffer
(file-name-shadow-mode t)                    ;; be smart about filenames in minibuffer
(icomplete-mode t)                           ;; autoactivate icomplete-mode
(setq icomplete-prospects-height 1           ;; don't spam the minibuffer
      icomplete-compute-delay 0)             ;; don't wait

;; Tweaks
(setq-default frame-title-format ;; change title from a frame - X Only
	      '(:eval (format "%s : %s@%s"
			      (file-name-nondirectory (or (buffer-file-name) default-directory))
			      (or (file-remote-p default-directory 'user) user-login-name)
			      (or (file-remote-p default-directory 'host) system-name))))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(fset 'yes-or-no-p 'y-or-n-p) ;; "yes or no" ==> "y or n" on prompts
(setq scroll-margin 0                        
      scroll-conservatively 100000           
      scroll-up-aggressively 0               
      scroll-down-aggressively 0             
      scroll-preserve-screen-position t) ;; preserve screen pos with C-v/M-v 
(setq compilation-read-command nil ;; autocompile without prompting the user, unless you give it a prefix argument
      default-indicate-empty-lines t ;; vi like
      make-backup-files nil ;; file.ext~
      mouse-yank-at-point t
      require-final-newline t
      x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      vc-follow-symlinks t)                 

;; Custom Modes
(global-font-lock-mode t) ;; syntax highlight everywhere
(menu-bar-mode -1)	  ;; hide the menu bar
(winner-mode t) ;; use C-c <left> to restore the previous window view

;; recent files, to save recently used files
(require 'recentf)   
(setq recentf-save-file               "~/.emacs.d/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)

(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; do not show any startup message, it's annoying
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message                "")

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

;; custom mode hooks

;; C
(add-hook 'c-mode-hook
	  (lambda () (setq compile-command
			   (concat "gcc \""
				   (buffer-file-name)
				   "\" -o \""
				   (file-name-sans-extension buffer-file-name)
				   "\" -Wall -Wextra"))))
;; C++
(add-hook 'c++-mode-hook (lambda ()
			   (setq compile-command
				 (concat "g++ \""
					 (buffer-file-name)
					 "\" -o \""
					 (file-name-sans-extension buffer-file-name)
					 "\" -Wall -Wextra"))))
;; Java
(add-hook 'java-mode-hook (lambda ()
			    (setq compile-command
				  (concat "javac \""
					  (buffer-file-name)
					  "\""))))

;; file extensions

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
