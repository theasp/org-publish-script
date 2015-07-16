#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*-emacs-lisp-*-

;; Load stuff out of ~/.emacs.d/
(unless (require 'package nil t)
  (load
   (expand-file-name "~/.emacs.d/elpa/package.el")))
(package-initialize)

(setq debug-on-error t)

