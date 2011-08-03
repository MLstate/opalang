;; This file contains the standard emacs conf for code that complies with our guidelines

;;  * use tuareg-mode
;;  * insert at the end of your .emacs:
;;      (load-file "~/opalang/utils/emacs/emacs_conf.el")
;;    (or wherever you have your opalang repo)
;; A good way to do it is to take avantage of the OPA_SOURCE_DIR env. var. you've previously set (haven't you?!)
;;      (load-file (concat (getenv "OPA_SOURCE_DIR")  "utils/emacs/emacs_conf.el"))

;; Thank you for taking this in consideration as soon as possible.

;; Cheers,
;; Mathieu + Louis (the ML team)

;; GUIDELINES:
;;
;; * The code should be indented with the auto mode (using indent-region macro)
;;   (a few exceptions are allowed for lisibility)
;;
;; * Cleanup whitespace as much as possible, it helps clean git commits. This
;;   conf helps you do it; for more, add this to the custom-set-variables of your
;;   .emacs:
;;   '(before-save-hook (quote (whitespace-cleanup)))
;;   It will automatically cleanup at save time. But beware, when you edit
;;   unclean files, it may add a lot of changes, so it MUST be used in
;;   conjunction with "git add -p", so that you check what you are pushing.
;;
;; Our dear friends the VIM users should make sure they respect the same
;; indentation rules (but I trust that VIM has a less "let the user do dirty
;; things if he doesn't care" policy)

;; this is used for showing tabs with the same color as whitespaces
(add-hook 'font-lock-mode-hook
              (lambda ()
                (font-lock-add-keywords
                  nil
                  '(("\t" 0 'trailing-whitespace prepend)))))

(custom-set-variables

 ;; Basic emacs configuration
 ;; - Clean whitespace
 '(indent-tabs-mode nil)
 '(show-trailing-whitespace t)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(indicate-empty-lines t)
 '(fill-column 80)
 '(whitespace-style '(empty indentation trailing tabs space-after-tab space-before-tab tab-mark))

 ;; Standard and proper tuareg-mode configuration. Should be global for
 ;; everyone, any changes not agreed upon will be reverted.
 '(tuareg-default-indent 2)
 '(tuareg-in-indent 0)
 '(tuareg-let-always-indent t)
 '(tuareg-with-indent 0)
 '(tuareg-function-indent 0)

 ;; Some convenience features
 '(copyright-regexp "\\(©\\|(c)\\|@copyright{}\\|[Cc]opyright\\s *:?\\s *\\(?:(C)\\)?\\|[Cc]opyright\\s *:?\\s *©\\)\\s *\\(?:[^0-9
]*\\s *\\)?\\([1-9]\\([-0-9, ';/*%#
        ]\\|\\s<\\|\\s>\\)*[0-9]+\\)")
 '(compilation-error-screen-columns nil) ;; Correct jumping to error columns

 ;; Hints for the local configuration
 ;; - help emacs find your source files from error messages
 ;;'(compilation-search-path (quote (nil "libqml" "libqmlcompil" "opa" "~/opalang")))
)

(add-hook 'before-save-hook 'copyright-update) ;; Maintain our "copyright" headers

;; OCaml backtrace and errmsg style: "file "f", line n, characters n-n",
;; so that you can clic-jump to errors
(require 'compile)
(add-to-list 'compilation-error-regexp-alist '("[Ff]ile \"\\(.*?\\)\", line \\([0-9]+\\)\\(, characters \\([0-9]+\\)-\\([0-9]+\\)\\)?" 1 2 (4 . 5) 0))

;; Get our compiler-msgs colors when compiling inside emacs
(defun compile ()
  (interactive nil)
  (let ((command (compilation-read-command (eval compile-command))))
    (unless (equal command (eval compile-command)) (setq compile-command command))
    (if (member "*compilation*" (mapcar 'buffer-name (buffer-list nil)))
        '((switch-to-buffer "*compilation*") (kill-buffer nil)))
    (compilation-start command t))
)
(ansi-color-for-comint-mode-on)

(defvar evil-caml-keywords
  '("open" "include"))
(add-hook 'tuareg-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 `((,(regexp-opt evil-caml-keywords 'words) 0 font-lock-warning-face t)))))

(defun enable_flyspell ()
  (ispell-change-dictionary "american")
  (flyspell-prog-mode)
)

;; Enable spell-checking on OPA comments and strings
(add-hook 'opa-mode-hook 'enable_flyspell)

;; Enable spell-checking on Caml comments and strings
(add-hook 'tuareg-mode-hook 'enable_flyspell)
