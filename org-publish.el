#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*-emacs-lisp-*-

;;(setq debug-on-error t)

(defvar publish/org-dir (expand-file-name "./"))
(defvar publish/publish-dir (expand-file-name "./publish/output/"))

;; Over complicated option handling
(let (options-done
      names)
  (pop argv)  ; Remove the -- separator
  (while argv
    (let ((option (pop argv)))
      (cond
       (options-done (push option names))
       ;; Don't process options after "--"
       ((string= option "--") (setq options-done t))
       ;; --org-dir dir
       ((string= option "--org-dir")
        (setq publish/org-dir (pop argv)))
       ;; --org-dir=dir
       ((string-match "\\`--org-dir=\\(\\(?:.\\|\n\\)*\\)\\'" option)
        (setq publish/org-dir (match-string 1 option)))
       ;; --publish-dir dir
       ((string= option "--publish-dir")
        (setq publish/publish-dir (pop argv)))
       ;; --publish-dir=dir
       ((string-match "\\`--publish-dir=\\(\\(?:.\\|\n\\)*\\)\\'" option)
        (setq publish/publish-dir (match-string 1 option)))
       ((string-prefix-p "--" option)
        (message "Unknown option: %s" option)
        (kill-emacs 1))
       (t (push option names)))
      (unless (> (length publish/org-dir) 0)
        (message "Missing argument for --org-dir!")
        (kill-emacs 1))
      (unless (> (length publish/publish-dir) 0)
        (message "Missing argument for --publish-dir!")
        (kill-emacs 1)))))

;; Load stuff out of ~/.emacs.d/
(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;
;; Package Handling

;; Add extra elpa repos
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("mepla-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Comment out if you've already loaded this package...
(require 'cl)

(defvar publish/packages  '(graphviz-dot-mode
                            ido-vertical-mode
                            lua-mode
                            org-plus-contrib
                            css-mode
                            ess
                            gnuplot)
  "A list of packages to ensure are installed at launch.")

(defun publish/require-package (package)
  "Add a package to the list of required packages, and then attempt to load it"
  (add-to-list 'publish/packages package)
  (if (require package nil t)
      t
    (progn (message (format "Missing package: %s" package))
           nil)))

(defun publish/packages-installed-p (packages)
  (loop for p in packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun publish/install-packages ()
  "Install packages listed in publish/packages"
  (interactive)
  (remove-duplicates publish/packages)
  (unless (publish/packages-installed-p publish/packages)
    ;; check for new packages (package versions)
    (package-refresh-contents)
    ;; install the missing packages
    (dolist (package publish/packages)
      (when (not (package-installed-p package))
        (with-demoted-errors (package-install package))))))

(publish/install-packages)

;;;;;;;;;;;;;;;;;;;;
;; Org config
(add-to-list 'publish/packages 'org-plus-contrib)
(require 'org)
(require 'org-agenda)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (awk . t)
   (sh . t)
   (perl . t)
   (dot . t)
   (calc . t)
   (R . t)
   (gnuplot . t)
   (awk . t)))

;; Allow babel to execute commands and not ask about them
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate t)

;; Don't make backups
(setq make-backup-files nil)

;; Export planning informtation by default
(setq org-export-with-planning t)

;; Use unicode characters for checkboxes
(defun unicode-for-org-html-checkbox (checkbox)
  "Format CHECKBOX into Unicode Characters."
  (case checkbox (on "&#x22A0;")
        (off "&#x25FB;")
        (trans "&#x22A1;")
        (t "")))
(defadvice org-html-checkbox (around unicode-checkbox activate)
  (setq ad-return-value (unicode-for-org-html-checkbox (ad-get-arg 0))))

;; Store the timestamp in the publish directory
(defvar org-publish-timestamp-directory (concat publish/publish-dir "/.org-timestamps/"))

;; Define the dynamic project, which is the org files to process
(defvar publish/notes-public
  (list "notes-public"
        :base-directory publish/org-dir
        :recursive t
        :exclude "publish\\|calendars\\|.PRIVATE"
        :publishing-directory publish/publish-dir
        :publishing-function '(org-html-publish-to-html)
        :section-numbers nil
        :html-head-include-default-style nil
        :html-head "<link rel=\"stylesheet\" href=\"/~asp/notes/css/org-andrew.css\">"
        :html-link-up "./"
        :html-link-home "/~asp/notes/"
        :auto-sitemap t
        :sitemap-title "Andrew's Notes"
        :sitemap-filename "index.org"
        :sitemap-ignore-case t))

(defvar publish/notes-private
  (list "notes-private"
        :base-directory (concat publish/org-dir "/.PRIVATE/")
        :recursive t
        :exclude "calendars"
        :publishing-directory (concat publish/publish-dir "/.PRIVATE/")
        :publishing-function '(org-html-publish-to-html)
        :section-numbers nil
        :html-head-include-default-style nil
        :html-head "<link rel=\"stylesheet\" href=\"/~asp/notes/css/org-andrew.css\">"
        :html-link-up "./"
        :html-link-home "/~asp/notes/.PRIVATE/"
        :auto-sitemap t
        :sitemap-title "Andrew's Private Notes"
        :sitemap-filename "index.org"
        :sitemap-ignore-case t))

;; Define the static project, which is the output of the org files, or
;; random files in the org directory
(defvar publish/notes-static
  (list "notes-static"
        :base-directory publish/org-dir
        :base-extension "js\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
        :recursive t
        :exclude "publish"
        :publishing-directory publish/publish-dir
        :publishing-function 'org-publish-attachment))

(setq org-publish-project-alist nil)

(defun publish/publish-project (project)
  (let ((org-publish-timestamp-directory (concat publish/publish-dir "/.org-timestamps/" (car project))))
    (org-publish-project project)))

(publish/publish-project publish/notes-private)
(publish/publish-project publish/notes-public)
(publish/publish-project publish/notes-static)

;; Copy the htaccess file in place
(copy-file (concat publish/org-dir "/.htaccess") (concat publish/publish-dir "/.htaccess") t)
(copy-file (concat publish/org-dir "/.htdigest") (concat publish/publish-dir "/.htdigest") t)

(kill-emacs 0)
