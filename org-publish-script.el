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
    (when file (setq org-publish/init file))))

;; Load stuff out of ~/.emacs.d/
(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

(org-publish/run-init)

(kill-emacs 0)
