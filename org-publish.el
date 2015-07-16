#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*-emacs-lisp-*-

;; Load stuff out of ~/.emacs.d/
(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;
;; Org config
(defvar org-publish/init "org-publish-init.el"
  "The name of the file to run org-publish from when running as a
  script")
(defvar org-publish/debug nil
  "Show extra debugging output")
(defvar org-publish/packages '()
  "A list of packages to ensure are installed at launch.")

;;;;;;;;;;;;;;;;;;;;
;; Package Handling

(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

(require 'cl)

(defun org-publish/packages-installed-p (packages)
  (loop for p in packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun org-publish/install-packages (packages)
  "Install packages listed in org-publish/packages"
  (interactive)
  (remove-duplicates packages)
  (unless (org-publish/packages-installed-p packages)
    ;; check for new packages (package versions)
    (package-refresh-contents)
    ;; install the missing packages
    (dolist (package packages)
      (when (not (package-installed-p package))
        (with-demoted-errors (package-install package))))))

(defun org-publish/publish-project (project)
  (let* ((publish-dir (plist-get (cdr project) :publishing-directory))
         (org-publish-timestamp-directory (concat publish-dir "/.org-timestamps/" (car project))))
    (org-publish-project project)))

(defun org-publish/run-init ()
  (load org-publish/init))

;; Clear the project list, probably not needed...
(setq org-publish-project-alist nil)

;; Over complicated option handling
(let (options-done
      names)
  (pop argv)
  (while argv
    (let ((option (pop argv)))
      (cond
       (options-done (push option names))
       ;; Don't process options after "--"
       ((string= option "--")
        (setq options-done t))
       ((string= option "--debug")
        (setq debug t))
       ((string-prefix-p "--" option)
        (message "Unknown option: %s" option)
        (kill-emacs 1))
       (t (push option names)))))
  (let ((file (pop names)))
    (when file
      (setq org-publish/init file))))

(org-publish/run-init)

(kill-emacs 0)
