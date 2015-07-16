;;;;;;;;;;;;;;;;;;;;
;; Org config
(require 'org)

(setq debug-on-error t)

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
