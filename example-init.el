(prefer-coding-system 'utf-8)

;; Add extra elpa repos
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("mepla-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(org-publish/install-packages '(graphviz-dot-mode
                                lua-mode
                                org-plus-contrib
                                css-mode
                                ess
                                htmlize
                                gnuplot))

(require 'ox-odt)

;; active Babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (awk . t)
;;    (sh . t)
;;    (perl . t)
;;    (dot . t)
;;    (calc . t)
;;    (R . t)
;;    (gnuplot . t)
;;    (awk . t)))

;; Allow babel to execute commands and not ask about them.  Note that
;; this is unsafe.
;; (setq org-confirm-babel-evaluate nil)
;; (setq org-export-babel-evaluate t)

;; Use unicode characters for checkboxes and such
;; DO NOT USE THIS, IT BREAKS <>
;;(setq org-html-use-unicode-chars t)

;; Don't make backups
(setq make-backup-files nil)

;; Publish planning information by default
(setq org-export-with-planning t)

;; Add :class "sortable" to the default table arguments
(setq org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides" :class "sortable"))

;; Fontify source
(setq org-html-htmlize-output-type 'css)

;; Use unicode characters for checkboxes.  Comment to disable
(defun unicode-for-org-html-checkbox (checkbox)
  "Format CHECKBOX into Unicode Characters."
  (case checkbox (on "&#x22A0;")
        (off "&#x25FB;")
        (trans "&#x22A1;")
        (t "")))
(defadvice org-html-checkbox (around unicode-checkbox activate)
  (setq ad-return-value (unicode-for-org-html-checkbox (ad-get-arg 0))))

;; Unfortunatly ox-odt doesn't seem to publish properly.  Uncommenting
;; the following defun may help:
;; (defun org-odt-publish-to-odt (plist filename pub-dir)
;;   "Publish an org file to ODT.

;; FILENAME is the filename of the Org file to be published.  PLIST
;; is the property list for the given project.  PUB-DIR is the
;; publishing directory.

;; Return output file name."
;;   (let* ((org-inhibit-startup t)
;;          (visitingp (find-buffer-visiting filename))
;;          (work-buffer (or visitingp (find-file-noselect filename))))
;;     (prog1 (with-current-buffer work-buffer
;;              (let ((outfile (concat (file-name-sans-extension filename) ".odt")))
;;                (message (concat "Making " outfile))
;;                (org-odt--export-wrap
;;                 outfile
;;                 (let* ((org-odt-embedded-images-count 0)
;;                        (org-odt-embedded-formulas-count 0)
;;                        (org-odt-automatic-styles nil)
;;                        (org-odt-object-counters nil)
;;                        ;; Let `htmlfontify' know that we are interested in collecting
;;                        ;; styles.
;;                        (hfy-user-sheet-assoc nil))
;;                   ;; Initialize content.xml and kick-off the export process.
;;                   (let ((output (org-export-as 'odt nil nil nil plist))
;;                         (out-buf (progn
;;                                    (require 'nxml-mode)
;;                                    (let ((nxml-auto-insert-xml-declaration-flag nil))
;;                                      (find-file-noselect
;;                                       (concat org-odt-zip-dir "content.xml") t)))))
;;                     (with-current-buffer out-buf (erase-buffer) (insert output)))))
;;                (org-publish-attachment plist outfile pub-dir))))))

;; Embed these files in ODT
;; (setq org-odt-inline-image-rules '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")))

;; Convert to docx after producing odt
;;(setq org-odt-preferred-output-format "doc")

;; Use org-publish/{styles,content}.xml for ODT files
;;(setq org-odt-styles-file (concat org-publish/org-dir "org-publish/styles.xml"))
;;(setq org-odt-content-template-file (concat org-publish/org-dir "org-publish/content.xml"))

(org-publish/publish-project
 '("org-dynamic"
   :base-directory "~/org"
   :recursive t
   :exclude "^\\(publish\\|calendars\\|.PRIVATE\\)$"
   :publishing-directory "~/public_html/org"
   :publishing-function org-html-publish-to-html
   :auto-sitemap t
   :sitemap-title "Andrew's Notes"
   :sitemap-filename "index.org"
   :sitemap-ignore-case t))

(org-publish/publish-project
 '("notes-static"
   :base-directory "~/org"
   :base-extension "js\\|css\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
   :recursive t
   :exclude "^\\(publish\\|.PRIVATE\\)$"
   :publishing-directory "~/public_html/org/"
   :publishing-function org-publish-attachment))

;; Copy the htaccess file in place
;; (copy-file "~/.org/.htaccess" "~/public_html/org/.htaccess" t)
