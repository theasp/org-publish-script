#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*-emacs-lisp-*-

;; Load stuff out of ~/.emacs.d/
(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

(setq debug-on-error t)

(defvar org-publish/init-file "org-publish-init.el"
  "The name of the file to run org-publish from when running as a
  script")
(defvar org-publish/debug nil
  "Show extra debugging output")

(require 'cl)

(defun org-publish/packages-installed-p (packages)
  "Check to see if all of the packages listed are installed"
  (loop for p in packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun org-publish/install-packages (packages)
  "Install missing packages"
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
  "Publish a project after set the base-directory to be an
absolute path to work around stupid behaviour from
org-publish-org-sitemap and set the timestamp directory to be in
the publish directory"
  (let* ((base-dir (plist-get (cdr project) :base-directory))
         (publish-dir (plist-get (cdr project) :publishing-directory))
         (org-publish-timestamp-directory (concat publish-dir "/.org-timestamps/" (car project))))
    (plist-put (cdr project) :base-directory (file-truename base-dir))
    (org-publish-project project)))

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
      (setq org-publish/init-file file))))

(load org-publish/init-file)

(kill-emacs 0)
