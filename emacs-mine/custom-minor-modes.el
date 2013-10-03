;; custom-minor-modes
(provide 'custom-minor-modes)

;; modeline stuff
(line-number-mode t)
(column-number-mode t)                       ;; display the column number in the mode bar
(size-indication-mode t)                     ;; display size of the current file

;; minibuffer
(file-name-shadow-mode t)                    ;; be smart about filenames in minibuffer
(setq icomplete-prospects-height 1           ;; don't spam my minibuffer
      icomplete-compute-delay 0)             ;; don't wait
(iswitchb-mode t)                            ;; switches between buffers using substrings
(icomplete-mode t)                           ;; autoactivate icomplete-mode
(setq enable-recursive-minibuffers nil ;;  allow mb cmds in the mb
      max-mini-window-height .25	   ;;  max 2 lines
      minibuffer-scroll-window nil
      resize-mini-windows nil)

;; enable winner mode - C-c <left> restore the previous window configs
(winner-mode t)

;; mouse
(setq mouse-yank-at-point t)

;; enable ibuffer
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; don't annoy me with backup files everywhere
(setq make-backup-files nil)
;      auto-save-default nil)

;; lines
(setq require-final-newline t
      default-indicate-empty-lines t         
      next-line-add-newlines nil)            ;; C-n at the end of a file acts like newline

;; startup message
(setq inhibit-startup-message t              ;; don't show startup screen, it's annoying
      inhibit-startup-echo-area-message t
      initial-scratch-message                ";; scratch buffer created -- let's rock\n")

;; UI elements
(transient-mark-mode t)			     ;; highlight marked region
(global-font-lock-mode t)		     ;; syntax highlight everywhere
(menu-bar-mode nil)		             ;; hide the menu bar

;; some tweaking
(fset 'yes-or-no-p 'y-or-n-p)                ;; make all "yes or no" prompts show "y or n" instead
(setq compilation-read-command nil)          ;; compilation: autocompile without prompting the user, unless you give it a prefix argument

;; stop annoying prompt in git
(setq vc-follow-symlinks t)                 

;; eldoc mode (function signatures in modeline)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; do smooth scrolling, please
(setq scroll-margin 0                        
      scroll-conservatively 100000           
      scroll-up-aggressively 0               
      scroll-down-aggressively 0             
      scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 

;; enable clipboard integration
(setq x-select-enable-clipboard t            ;; copy-paste should work ...
      interprogram-paste-function            ;; ...with...
      'x-cut-buffer-or-selection-value)      ;; ...other X clients

;; change title from a frame
(setq-default  frame-title-format
	       '(:eval
		 (format "%s@%s: %s"
			 (or (file-remote-p default-directory 'user) user-login-name)
			 (or (file-remote-p default-directory 'host) system-name)
			 (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; recent files, to save recently used files
(require 'recentf)   
(setq recentf-save-file               "~/.emacs.d/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)

;; ido-mode stuff - powerful mode for find-file and switch-to-buffer - http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
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
;; (setq TeX-view-program-list '(("Evince" "zathura %o"))) ;; set zathura to view documents (gambiarra)
