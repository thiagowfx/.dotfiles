;; custom mode hooks
(provide 'custom-mode-hooks)

;; C++
(add-hook 'c++-mode-hook (lambda ()
			   (setq compile-command
				 (concat "g++ \""
					 (buffer-file-name)
					 "\" -o \""
					 (file-name-sans-extension buffer-file-name)
					 "\" -Wall -Wextra -g"))))
;; C
(add-hook 'c-mode-hook
	  (lambda () (setq compile-command
			   (concat "gcc \""
				   (buffer-file-name)
				   "\" -o \""
				   (file-name-sans-extension buffer-file-name)
				   "\" -Wall -Wextra -g"))))
;; Java
(add-hook 'java-mode-hook (lambda ()
			    (setq compile-command
				  (concat "javac \""
					  (buffer-file-name)
					  "\""))))
