; -*- lisp -*-

;; install these after a fresh emacs installation

;; mandatory
; auto-complete-mode
; redo+

;; optional
; markdown-mode
; emacs-pkgbuild-mode (for arch linux)

;; set the load path, add everything under ~/.emacs.d to it
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; set repos
(when (file-exists-p "~/.emacs.d/elpa/package.el")
  (when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
    (package-initialize)))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq url-http-attempt-keepalives nil)

;; load custom configs
(require 'file-extensions)
(require 'keystrokes)
(require 'custom-minor-modes)
(require 'custom-mode-hooks)
(require 'defuns)
(require 'extras)

;; (some) .emacs references used here
; - Load your .emacs file while in emacs M-x load-file RET ~/.emacs
; - http://www.emacsrocks.com/
; - http://www.emacswiki.org/emacs/
; - http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html ;lots of elisp packages!
; - https://news.ycombinator.com/item?id=1654164
; - https://github.com/vvv/dotfiles/blob/master/.emacs
; - http://www.djcbsoftware.nl/dot-emacs.html

;;custom dark theme for emacs
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
