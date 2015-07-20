                          ━━━━━━━━━━━━━━━━━━━━
                           ORG PUBLISH SCRIPT


                            Andrew Phillips
                          ━━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 Examples
.. 1.1 Using Mini-CI and git
..... 1.1.1 Step 1: Add Org Publish Init Script to Org
..... 1.1.2 Step 2: Install Mini-CI
..... 1.1.3 Step 3: Set up Mini-CI Project
..... 1.1.4 Step 4: Start the Mini-CI Daemon
..... 1.1.5 Step 5: Add Hook to git that Notifies Mini-CI





1 Examples
══════════

1.1 Using Mini-CI and git
─────────────────────────

  This assumes you have a git repository in ~/.git-repo/org.git on the
  same machine you will be running Mini-CI on.  Adjust as needed.


1.1.1 Step 1: Add Org Publish Init Script to Org
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Make the publish directory:
  ┌────
  │ mkdir ~/org/publish
  └────

  Put the following in ~/org/publish/org-publish-init.el:
  ┌────
  │ (prefer-coding-system 'utf-8)
  │ 
  │ ;; Add extra elpa repos
  │ (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  │ (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  │ (add-to-list 'package-archives '("mepla-stable" . "http://stable.melpa.org/packages/"))
  │ (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  │ 
  │ (org-publish/install-packages '(graphviz-dot-mode
  │                                 lua-mode
  │                                 org-plus-contrib
  │                                 css-mode
  │                                 ess
  │                                 htmlize
  │                                 gnuplot))
  │ 
  │ (require 'ox-odt)
  │ 
  │ ;; active Babel languages
  │ (org-babel-do-load-languages
  │  'org-babel-load-languages
  │  '((emacs-lisp . t)
  │    (awk . t)
  │    (sh . t)
  │    (perl . t)
  │    (dot . t)
  │    (calc . t)
  │    (R . t)
  │    (gnuplot . t)
  │    (awk . t)))
  │ 
  │ ;; Allow babel to execute commands and not ask about them.  Note that
  │ ;; this is unsafe.
  │ (setq org-confirm-babel-evaluate nil)
  │ (setq org-export-babel-evaluate t)
  │ 
  │ ;; Don't make backups
  │ (setq make-backup-files nil)
  │ 
  │ ;; Publish planning information by default
  │ (setq org-export-with-planning t)
  │ 
  │ ;; Fontify source
  │ (setq org-html-htmlize-output-type 'css)
  │ 
  │ ;; Use unicode characters for checkboxes.  Comment to disable
  │ (defun unicode-for-org-html-checkbox (checkbox)
  │   "Format CHECKBOX into Unicode Characters."
  │   (case checkbox (on "&#x22A0;")
  │         (off "&#x25FB;")
  │         (trans "&#x22A1;")
  │         (t "")))
  │ (defadvice org-html-checkbox (around unicode-checkbox activate)
  │   (setq ad-return-value (unicode-for-org-html-checkbox (ad-get-arg 0))))
  │ 
  │ (org-publish/publish-project
  │  '("org-dynamic"
  │    :base-directory "./"
  │    :recursive t
  │    :exclude "^\\(publish\\|calendars\\|.PRIVATE\\)$"
  │    :publishing-directory "./publish/output"
  │    :publishing-function org-html-publish-to-html
  │    :auto-sitemap t
  │    :sitemap-title "My Org-Mode Files"
  │    :sitemap-filename "index.org"
  │    :sitemap-ignore-case t))
  │ 
  │ (org-publish/publish-project
  │  '("org-static"
  │    :base-directory "./"
  │    :base-extension "js\\|css\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
  │    :recursive t
  │    :exclude "^\\(publish\\|.PRIVATE\\)$"
  │    :publishing-directory "./publish/output"
  │    :publishing-function org-publish-attachment))
  └────

  Check the file in and push to your repository.


1.1.2 Step 2: Install Mini-CI
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  See [Mini-CI].


  [Mini-CI] https://github.com/theasp/mini-ci


1.1.3 Step 3: Set up Mini-CI Project
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Make a directory:
  ┌────
  │ mkdir -p ~/var/mini-ci/org
  └────

  Put the follwing into ~/var/mini-ci/org/config:
  ┌────
  │ REPO_PLUGIN=git
  │ GIT_URL=~/.git-repo/andrew-org
  │ POLL_FREQ=0
  │ BUILD_KEEP=5
  └────

  Make the tasks directory:
  ┌────
  │ mkdir ~/var/mini-ci/org/tasks.d
  └────

  Put the following into ~/var/mini-ci/org/tasks.d/500-org-publish
  ┌────
  │ #!/bin/bash
  │ set -ex
  │ 
  │ org-publish ./publish/org-publish-init.el
  └────

  Put the following into ~/var/mini-ci/org/tasks.d/800-rsync-to-web:
  ┌────
  │ #!/bin/bash
  │ set -ex
  │ 
  │ OUTDIR=./publish/output
  │ PUBLISHDIR=~/public_html/org
  │ 
  │ rsync --delete -a $OUTDIR/ $PUBLISHDIR/
  └────

  Make the tasks executable:
  ┌────
  │ chmod +x ~/var/mini-ci/org/tasks.d/*
  └────


1.1.4 Step 4: Start the Mini-CI Daemon
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Start the Mini-CI daemon by running:
  ┌────
  │ mini-ci -d ~/var/mini-ci/org poll
  └────

  If everything works, you should have files in your web directory,
  otherwise examine the Mini-CI log files.

  Add the following to cron to ensure the daemon is always running:
  ┌────
  │ */5 * * * * test -d var/mini-ci/org && mini-ci --oknodo -d var/mini-ci/org poll
  └────


1.1.5 Step 5: Add Hook to git that Notifies Mini-CI
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Add the following to ~/.git-repo/org.git/hooks/post-update:
  ┌────
  │ #!/bin/bash
  │ 
  │ set -e
  │ 
  │ mini-ci -d ~/var/mini-ci/org -m update
  └────

  Make the hook executable:
  ┌────
  │ chmod +x ~/.git-repo/org.git/hooks/post-update
  └────

  Now anytime something is pushed to your repository it will trigger
  Mini-CI to build a new copy of the website.
