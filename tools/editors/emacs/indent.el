#!/usr/bin/emacs --script

(load "tuareg")
(load-file
  (concat
    (let (osd (getenv "OPA_SOURCE_DIR"))
      (if (eq nil osd)
        "."
        osd))
    "/utils/emacs/emacs_conf.el"))

(mapc
 (lambda (file)
   (find-file file)
   (tuareg-mode)
   (indent-region (point-min) (point-max))
   (whitespace-cleanup)
   (save-buffer))
 command-line-args-left)
