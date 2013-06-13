;;; install.el --- Package to ease installation of Elisp packages

;; Copyright (C) 2001, 2003, 2004, 2005, 2006  Stefan Monnier

;; Author: Stefan Monnier <monnier@gnu.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This little package is meant to ease up the task of installing
;; third party ELisp packages.  I.e. it takes care of placing it in
;; an appropriate location, finds out what code is necessary to get
;; the package activated, sets up your .emacs file to activate the
;; package, and byte-compiles the package.

;; It should work on both single-file packages and tarballs.

;; On tarball packages, it does a bit of guess work to figure out
;; where are which files and how to use them.  This is bound to
;; fail sometimes.

;; The main entry points are the following:

;; M-x install-file RET
;;    Takes a file as an argument (an elisp file or a tarball) and installs
;;    and activates it.  This includes copying/untarring into the
;;    site-lisp area.
;;
;; M-x install-buffer RET
;;    Same thing but with a buffer (elisp or tar mode).
;;
;; M-x install-directory-inplace RET
;;    Takes a directory containing an elisp package (probably tarball you
;;    just untarred) and sets it up so it can be used (byte-compiles files,
;;    sets up autoloads), and activates it.  All of this without copying it
;;    into the site-lisp area.
;;
;; M-x install-list-packages RET
;;    A placeholder for what should become a UI that will allow you to list
;;    available/installed/activated packages, activate/deactivate them,
;;    install/uninstall, ...

;; Tested on:
;; + ProofGeneral
;; + sml-mode
;; + AUCTeX   (missed the info page)
;; + X-Symbol (as an XEmacs package)
;; + Gnus     (but doesn't install the info doc :-( )
;; + BBDB     (misses the main info page)
;; - WhizzyTeX (needs to hack the perl script and stuff :-( )
;; ? ECB
;; ? JDEE
;; ? preview-latex
;; ? VM
;; ? mmm-mode
;; ? Semantic

;; The above list should be taken with a heavy grain of salt: each bullet
;; was true at some point, but the code of install.el hs changed since and
;; the code of those packages as well.

;; The on-disk structure is as follows:
;; - there are two area: the `home' and the `site' each with their
;;   respective directory (~/lib/emacs and /usr/share/emacs/site-lisp)
;;   and file (.emacs and site-start).
;; - There is a distinction between installing and activating.
;;   Installing only places the files on disk, whereas activating sets up
;;   autoloads and friends.
;; - Activation is done on a directory by directory basis.  Each directory
;;   has an `autoloads' file.  Loading it activates the package(s)
;;   in directory.
;; - Single-file packages are placed together in the toplevel directory
;;   whereas tarball-packages are placed in their own subdirectory (so they
;;   can be activated independently).

;;; Todo:

;; - don't ask whether to activate site-wide packages installed in home.
;; - Create Info from Texinfo when needed.
;; - Try harder to find Info files such as doc/auctex.
;; - UI to (un)install and (de)activate packages, get a list, ...
;; - If a single-file package lacks ;;;###autoload, try to add them
;;   based on the Commentary section or something.
;; - don't prompt for site-wide/home from within the command.  Only do that
;;   from the interactive spec, if at all.
;; - install-activate should be called install-save-activation.
;;   Better separate activation and writing the activation command.
;; - don't use (load "/foo/bar/baz/autoloads" 'install) but rather something
;;   like (install-activate "baz") together with a search path like
;;   install-package-dirs (or maybe reuse load-path for that).
;; - maybe in install-activate remember load-file-name so as to know which
;;   file to modify if the user wants to remove the activation command.

;;; Code:

(require 'em-glob)
(eval-when-compile (require 'cl))

(defgroup install nil
  "Elisp package installation")

(defmacro install-filter (list exp)
  (declare (debug t))
  `(let ((res nil))
     (dolist (x ,list (nreverse res))
       (if ,exp (push x res)))))

(defcustom install-site-file
  (or
   (locate-file (or site-run-file "site-start") load-path load-suffixes)
   (let ((lp (mapcar 'abbreviate-file-name load-path)))
     ;; Prefer non-user directories.
     (setq lp (or (install-filter lp (not (string-match "\\`~/" x))) lp))
     ;; Prefer site-lisp directories.
     (setq lp (or (install-filter lp (string-match "/site-lisp\\'" x)) lp))
     ;; Prefer shorter directory names (i.e. parents rather than subdirs).
     (setq lp (sort lp (lambda (d1 d2) (< (length d1) (length d2)))))
     ;; 
     (expand-file-name (concat (or site-run-file "site-start") ".el") (car lp))))
  "Customization file where we store site-wide activation settings."
  :type 'file)

(defcustom install-site-dir (file-name-directory install-site-file)
  "Directory where we install site-wide packages."
  :type 'directory)

(defcustom install-home-file (or user-init-file
				 (convert-standard-filename "~/.emacs"))
  "Customization file where we store user-specific activation settings."
  :type 'file)

(defcustom install-home-dir
  ;; FIXME: We should be careful never to choose one of Emacs's own
  ;; directories, even if the user installed Emacs in his home dir.
  (let ((lp (mapcar 'abbreviate-file-name load-path)))
    ;; Only consider writable directories.
    (setq lp (install-filter lp (file-writable-p x)))
    ;; Only consider user directories.
    (setq lp (install-filter lp (string-match "\\`~/" x)))
    ;; Prefer shorter directory names (i.e. parents rather than subdirs).
    (setq lp (sort lp (lambda (d1 d2) (< (length d1) (length d2)))))
    ;; Default to ~/lib/emacs.
    (if (or (null lp)
	    ;; If it's a subdir of lib/emacs, use lib/emacs.  This can happen
	    ;; because Install does not automatically add lib/emacs to the
	    ;; load-path if it only installs tar packages underneath.
	    (string-match "\\`~/lib/emacs/" (car lp)))
	"~/lib/emacs/"
      (car lp)))
  "Directory into which we install user-specific elisp packages."
  :type 'directory)

(defconst install-autoload-file "autoloads"
  "Name of autoload files used by Install.")

(defcustom install-compress-source-files nil ;; ".gz"
  "If non-nil, Install will try to compress file.")

(defcustom install-byte-compile t
  "If non-nil, elisp files are byte-compiled during installation."
  :type 'boolean)

;;

(defun install-get-dir ()
  "Return the directory into which to install packages."
  (or (and (file-writable-p install-site-dir)
	   (y-or-n-p "Install site-wide? ")
	   install-site-dir)
      (progn
	(unless (file-writable-p install-home-dir)
	  (setq install-home-dir
		(let ((default-directory
                        (expand-file-name
                         (file-name-as-directory install-home-dir))))
		  (read-directory-name "Directory to install into: ")))
	  (unless (file-directory-p install-home-dir)
	    (make-directory install-home-dir t)))
	install-home-dir)))

(defun install-get-file ()
  "Return the file into which to activate packages."
  (or (and (file-writable-p install-site-file)
	   (y-or-n-p "Activate site-wide? ")
	   install-site-file)
      install-home-file))

(defmacro install-with-file (file &rest body)
  (declare (debug t) (indent 1))
  `(let ((install-with-existing-file (find-buffer-visiting ,file)))
     (with-current-buffer (or install-with-existing-file
			      (find-file-noselect ,file))
       (prog1 (save-current-buffer ,@body)
	 (unless install-with-existing-file
	   (kill-buffer (current-buffer)))))))

;;;###autoload
(defun install-file (file)
  (interactive "fFile to install: ")
  (with-current-buffer (find-file-noselect file)
    (install-buffer)))


;;;###autoload
(defun install-buffer ()
  "Install the current elisp buffer as a package.
The package is install in `install-home-dir', autoloads are added
to the `install-autoload-file' in that directory and the
`install-custom-file' is then updated to load these autoloads."
  (interactive)
  (cond
   ((derived-mode-p 'tar-mode) (install-tar-buffer))
   ((not (derived-mode-p 'emacs-lisp-mode))
    (error "I only know how to install tar.gz and elisp files."))
   (t
    (install-elisp-buffer))))

(defun install-elisp-buffer ()
  (let* ((install-dir (install-get-dir))
         (package (file-name-nondirectory buffer-file-name))
         (file (expand-file-name package install-dir))
         (autoload (expand-file-name install-autoload-file install-dir)))
    (when (and install-compress-source-files
               (string-match "\\.el\\'" file))
      (setq file (concat file (if (stringp install-compress-source-files)
                                  install-compress-source-files ".gz"))))
    ;; Install the elisp file.
    (write-region (point-min) (point-max) file)
    ;; Extract the autoloads into a separate file.
    (install-update-autoloads autoload)
    ;; Activate.
    (install-activate autoload)
    ;; Finally, byte compile.  In the present case (a single-file package),
    ;; this could be done before activation.
    (if install-byte-compile
        (byte-compile-file file))))

(defun install-tar-buffer ()
  "Like `install-buffer' but for a tar package rather than single file."
  (let* ((name (file-name-nondirectory buffer-file-name))
	 ;; Strip off ".tar.gz", ".tar", ".tgz", ".tar.Z", ...
	 (name (if (string-match "\\.[tT][^.]+\\(\\.[^.]+\\)?\\'" name)
		   (substring name 0 (match-beginning 0)) name))
	 (install-dir (install-get-dir))
	 (default-directory (expand-file-name (file-name-as-directory name)
                                              install-dir)))
    ;; Install the files.
    ;; FIXME: check what `tar-untar-buffer' does with symlinks and stuff.
    (if (not (file-directory-p default-directory))
        (make-directory default-directory)
      (unless (y-or-n-p (format "%s already exists.  Overwite? "
                                default-directory))
        (error "Abort")))
    (tar-untar-buffer)
    (let ((files (directory-files default-directory
				  nil "\\`\\([^.]\\|\\.[^.]\\|\\.\\..\\)" t)))
      ;; If the tar file already had everything under a single directory,
      ;; remove the redundant level of directory.
      (when (and (= (length files) 1) (file-directory-p (car files)))
	(let* ((f (car files))
	       ;; Keep the longest name of the two, assuming that the
	       ;; difference is that the longer one has a version number.
	       (final (if (> (length name) (length f)) name f))
	       (temp (if (= (length name) (length f)) (concat f ".tmp") f)))
	  ;; FIXME: the dir might already exist.
	  (rename-file f (expand-file-name temp install-dir))
	  (setq default-directory
                (expand-file-name (file-name-as-directory install-dir)))
	  (delete-directory name)
	  ;; FIXME: the dir might already exist.
	  (unless (equal final temp) (rename-file temp final))
	  (setq name final)
	  (setq default-directory (expand-file-name final)))))
    (install-directory-inplace default-directory)))

;;;###autoload
(defun install-directory-inplace (dir)
  "Prepare and activate the current directory for use by Emacs.
Sets up the autoload files, activates them and byte-compiles if needed."
  (interactive
   (list (if (derived-mode-p 'dired-mode)
	     list-buffers-directory
	   (read-directory-name "Directory to install: " nil nil t))))
  (let ((default-directory (expand-file-name (file-name-as-directory dir))))
    ;; Extract the autoloads.
    (install-setup-tree)
    ;; Activate the package.
    (install-activate (expand-file-name (install-get-activation-file)))
    ;; Finally, byte-compile the files.
    (if install-byte-compile
	(install-byte-compile-dir))))

(defun install-dirs-of-files (files)
  "Return a list of subdirs containing elisp files."
  (let ((dirs nil)
	(ignore (regexp-opt
		 (cons
		  ;; Ignore contrib directories because they tend to contain
		  ;; either less-debugged code, or packages that might
		  ;; already be installed and can thus interfere.
		  "contrib/"
		  (let ((exts nil))
		    (dolist (ext completion-ignored-extensions exts)
		      (if (eq (aref ext (1- (length ext))) ?/)
			  (push ext exts))))))))
    ;; Collect the dirs that hold elisp files.
    (dolist (file files dirs)
      (let ((dir (file-name-directory file)))
	(unless (or (member dir dirs)
		    (and dir (string-match ignore dir)))
	  (push dir dirs))))))

(defun install-find-elisp-dirs ()
  "Return a list of subdirs containing elisp files."
  (install-dirs-of-files (install-glob "**/*.el")))

(defun install-byte-compile-dir ()
  "Byte compile all elisp files under the current directory."
  (let ((load-path (append (mapcar (lambda (dir)
				     (if dir
					 (expand-file-name dir)
				       default-directory))
				   (install-find-elisp-dirs))
			   load-path)))
    (byte-recompile-directory default-directory 0)))

(defun install-glob (pattern)
  (let ((res (eshell-extended-glob pattern)))
    (if (listp res) res)))

(defun install-get-activation-file ()
  "Return the file to load to activate the package.
This is usually \"./autoloads\", but it can also be \"lisp/foo-site.el\"."
  (if (file-exists-p install-autoload-file)
      install-autoload-file
    (or (car (install-glob (concat "**/" install-autoload-file)))
	(car (install-glob "**/auto-autoloads.el"))
	(car (install-glob "**/*-site.el")))))

(defun install-setup-tree ()
  (eshell-glob-initialize)
  ;; Look for elisp files.
  (let ((dirs (install-find-elisp-dirs))
	(autoload-files nil)
	(toplevel nil))
    ;; Prepare each elisp subdir and collect info along the way.
    ;; PROBLEM: if a dir that contains elisp files contains no autoload, we
    ;; don't know whether to add it to load-path or not: if it's a library
    ;; it should be added, but it might be some unimportant `contrib' or
    ;; testing code that should be left alone.
    (dolist (dir dirs)
      (let ((default-directory (expand-file-name (or dir default-directory))))
	;; Remove *.elc files, in case they were not compiled for our version.
	(mapc 'delete-file (install-glob "*.elc"))
	;; Extract autoloads.
	(let ((sites (or (install-glob "auto-autoloads.el")
			 (install-glob "*-load.el")
			 (install-glob "*-site.el"))))
	  (if (= 1 (length sites))
	      ;; Some packages come with a <pkg>-site.el file instead
	      ;; of using autoloads.  In that case, just load that file.
	      (progn
		(install-sanitize-autoloads-file (car sites))
		(push (concat dir (car sites)) autoload-files))
	    ;; Otherwise.  Make an autoloads file and load it.
	    ;; FIXME: Don't make hundreds of autoload files.
	    (let ((exists (file-exists-p install-autoload-file)))
	      (if (not (install-update-autoloads install-autoload-file))
		  ;; Don't stupidly add empty autoloads files.
		  ;; PROBLEM: this removes DIR from load-path as well.
		  (unless exists (delete-file install-autoload-file))
		(push (concat dir install-autoload-file) autoload-files)))))))
    (unless autoload-files
      ;; We didn't find any autoload file.  I.e. there's nothing to load.
      ;; Most likely this package should be used via `require', like elib.
      (setq autoload-files
	    (mapcar (lambda (dir) (concat dir install-autoload-file)) dirs))
      (mapc 'install-sanitize-autoloads-file autoload-files))
    ;; Setup the toplevel activation file.
    (if (and (= 1 (length autoload-files))
	     (equal (car autoload-files) (install-get-activation-file)))
	(setq toplevel (car autoload-files))
      (setq toplevel install-autoload-file)
      (dolist (file autoload-files)
	(unless (equal file toplevel)
	  (install-activate
	   `(expand-file-name
	     ,(file-relative-name
	       (expand-file-name file)
	       (file-name-directory (expand-file-name toplevel)))
	     (file-name-directory load-file-name))
	   toplevel))))
    ;; Make up an info/dir file if necessary and register the info dirs.
    (let ((info-dirs (install-make-info)))
      (when info-dirs
	(install-with-file toplevel
	  (unless (derived-mode-p 'emacs-lisp-mode) (emacs-lisp-mode))
	  (goto-char (point-min))
	  (unless (re-search-forward "(add-to-list[ \t\n]+'Info-default-directory-list" nil t)
	    (forward-comment (point-max))
	    (while (re-search-backward "^" nil t))
	    (unless (bolp) (newline))
	    (let ((top-dir (file-name-directory (expand-file-name toplevel))))
	      (dolist (dir info-dirs)
		(setq dir (expand-file-name (or dir default-directory)))
		(if (equal dir top-dir)
		    (insert "(add-to-list 'Info-default-directory-list (file-name-directory load-file-name))\n")
		  (let ((text (pp-to-string (file-relative-name dir top-dir))))
		    (if (string-match "\n\\'" text)
			(setq text (substring text 0 -1)))
		    (insert "(add-to-list 'Info-default-directory-list\n"
			    "             (expand-file-name " text
			    " (file-name-directory load-file-name)))\n")))))
	    (save-buffer 0)))))))

(defun install-sanitize-autoloads-file (file)
  "Make sure that the autoload file FILE exists and behaves sanely."
  (install-with-file file
    (unless (derived-mode-p 'emacs-lisp-mode) (emacs-lisp-mode))
    ;; Make file and buffer writable (for CVSREAD=on kind of situations).
    (when buffer-read-only
      (set-file-modes buffer-file-name
		      (logior ?\200 (file-modes buffer-file-name)))
      (toggle-read-only))
    (goto-char (point-min))
    ;; Insert a little boiler plate if there's nothing yet.
    (when (eobp)
      (insert ";;; " (file-name-nondirectory file)
	      " --- automatically extracted autoloads\n"
	      ";;\n"
	      ";;; Code:\n\n"
	      "\n;; Local Variables:\n"
	      ";; version-control: never\n"
	      ";; no-byte-compile: t\n"
	      ";; no-update-autoloads: t\n"
	      ";; End:\n"
	      ";;; " (file-name-nondirectory file)
	      " ends here\n")
      (goto-char (point-min)))
    ;; Make sure it will setup the load path properly.
    (unless (re-search-forward "\\<load-file-name\\>" nil t)
      (forward-comment (point-max))
      (while (re-search-backward "^" nil t))
      (unless (bolp) (newline))
      (unless (eq (char-before (1- (point))) ?\n) (newline))
      (insert ";; Tell Emacs to look for elisp files in this directory."
	      ;; Add some sort of signature.
	      "  -- Install\n")
      (insert "(add-to-list 'load-path
              (or (file-name-directory load-file-name) (car load-path)))\n\n")
      (save-buffer 0)))
  file)

(defvar generated-autoload-file)

(defun install-update-autoloads (autoload)
  "Update file AUTOLOAD.  This will create the file if necessary.
Returns non-nil if there is anything autoloaded into it."
  (setq autoload (expand-file-name autoload))
  (let ((bufp (find-buffer-visiting autoload)))
    (let ((generated-autoload-file (install-sanitize-autoloads-file autoload)))
      ;; (update-file-autoloads file)
      (update-directory-autoloads (file-name-directory autoload)))
    ;; Make sure the file sets up the load-path appropriately.
    (with-current-buffer (find-file-noselect autoload)
      (unless (derived-mode-p 'emacs-lisp-mode) (emacs-lisp-mode))
      (goto-char (point-min))
      (re-search-forward "^" nil t) ;Find the first autoload entry.
      (forward-comment (point-max))
      (prog1 (not (eobp))
	(unless bufp (kill-buffer (current-buffer)))))))

(defun install-activate (autoload &optional into)
  "Update INTO to make sure it loads AUTOLOAD.
AUTOLOAD can be an expression.
If it is a string, this also loads it into the currently running Emacs.
If provided, INTO specifies the file which should load AUTOLOAD.
The default is to use `install-get-file'."
  (when (stringp autoload)
    (setq autoload (abbreviate-file-name autoload))
    (load autoload))
  (install-with-file (or into (install-get-file))
    (unless (derived-mode-p 'emacs-lisp-mode) (emacs-lisp-mode))
    (save-excursion
      (let ((text (pp-to-string autoload)))
	(if (string-match "\n\\'" text)
	    (setq text (substring text 0 -1)))
	(goto-char (point-min))
	(unless (re-search-forward (regexp-quote text) nil t)
	  (goto-char (point-min))
	  (forward-comment (point-max))
	  (while (re-search-backward "^" nil t))
	  (unless (bolp) (newline))
	  ;; Pass `install' as argument to load: this both makes Emacs
	  ;; ignore the load if the file is missing and is used as a marker
	  ;; indicating that this load statement was introduced by us.
	  (insert "(load " text " 'install)\n")
	  (save-buffer))))))

;;;###autoload
(defun install-list-packages ()
  "Show the installed packages."
  (interactive)
  (dired (install-get-dir)))

;; Info files and DIR files.
;; Some of this should probably be moved to info.el.

(defconst install-info-dir "-*- Text -*-\n\n\
File: dir	Node: Top	This is the top of the INFO tree\
\n\n* Menu:\n\n"
  "Text content of a barebones empty `info/dir' file.")

(defun install-find-info-files ()
  (let ((files (or (install-glob "**/*.info*")
		   (install-glob "**/info/*")))
	(tmp nil))
    (dolist (f files)
      (unless (or (member f '("dir" "localdir"))
		  (and (string-match "-[0-9]+" f)
		       (member (replace-match "" t t f) files))
		  (not (string-match "\\.info\\>\\|\\(\\`\\|/\\)[^.]+\\(\\'\\|\\.\\(gz\\|Z\\)\\)" f)))
	(push f tmp)))
    tmp))

(defun install-make-info ()
  "Make an info/dir file if necessary and return the info directories."
  ;; FIXME: This should create the info files from the Texinfo files
  ;; if necessary !!
  ;; Problems to do that:
  ;; - detect when necessary.  E.g. BBDB comes with an info page for
  ;;   the bbdb-filters stuff, but the main bbdb doc is in texinfo.
  ;; - figure out how to makeinfo the thing.  E.g. AucTeX comes with
  ;;   a whole bunch of Texinfo files and it's really not clear which
  ;;   is the right one.
  ;; - The info file might be there, but not found.  E.e. AucTeX has its
  ;;   page in doc/auctex.
  (let* ((files (install-find-info-files))
	 (dirs (install-dirs-of-files files))
	 (dir-files nil))
    ;; Remove files that were in ignored directories.
    (dolist (file files)
      (unless (member (file-name-directory file) dirs)
	(setq files (delq file files))))
    ;; Check that there's something to do.
    (when files
      (assert dirs)
      (dolist (dir dirs)
	(if (file-exists-p (expand-file-name "dir" dir))
	    (push (expand-file-name "dir" dir) dir-files)))
      (unless dir-files
	;; Pick the dir closest to the toplevel to put the main dir file.
	(setq dirs (sort dirs (lambda (s1 s2) (< (length s1) (length s2)))))
	(install-with-file (expand-file-name "dir" (car dirs))
	  (assert (= (point-min) (point-max)))
	  (insert install-info-dir)
	  (narrow-to-region (point) (point-max))
	  (dolist (file files)
	    (let ((section "Miscellaneous")
		  (entry nil))
	      (install-with-file file
		(goto-char (point-min))
		(if (not (re-search-forward
			  (concat "^START-INFO-DIR-ENTRY\n"
				  "\\([* \t].*\n\\)+"
				  "END-INFO-DIR-ENTRY$") nil t))
		    ;; No entry in the file, let's build a default one.
		    (let ((base (file-name-nondirectory
				 (file-name-sans-extension file))))
		      (setq entry (concat "* " (upcase base)
					  ": (" base ").\n")))
		  (setq entry (match-string 1))
		  (goto-char (point-min))
		  (when (re-search-forward
			 "^INFO-DIR-SECTION[ \t]+\\(.*[^ \t\n]\\)" nil t)
		    (setq section (match-string 1)))))
	      (goto-char (point-min))
	      (unless (search-forward entry nil t)
		(unless (re-search-forward (concat "^" (regexp-quote section) "[ \t]*\n") nil 'move)
		  (unless (bobp) (newline))
		  (insert section) (newline))
		(insert entry))))
	  (save-buffer 0)
	  (kill-buffer (current-buffer))))
      dirs)))
	      
	      

(provide 'install)
;; arch-tag: 7f3dd74d-b21f-462e-91bd-1e53fa19a2ee
;;; install.el ends here
