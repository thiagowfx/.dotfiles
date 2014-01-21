;; defuns
(provide 'defuns)

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
