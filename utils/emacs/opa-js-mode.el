;;; opa-js-mode.el --- Major mode for editing Opa-Js source text

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;; Maintainer: Karl Landstrom <karl.landstrom@brgeight.se>
;; Version: 2.2.1
;; Date: 2008-12-27
;; Keywords: languages, oop

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; The main features of this Opa-Js mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments.
;;
;; This package has (only) been tested with GNU Emacs 21.4 (the latest
;; stable release).
;;
;; Installation:
;;
;; Put this file in a directory where Emacs can find it (`C-h v
;; load-path' for more info). Then add the following lines to your
;; Emacs initialization file:
;; 
;;    (autoload 'opa-js-mode "opa-js-mode" nil t)
;;    (autoload 'opa-classic-mode "opa-mode" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.opa$" . opa-js-mode)) ;; <-- Set the default mode here
;;    (add-to-list 'auto-mode-alist '("\\.js\\.opa$" . opa-js-mode))
;;    (add-to-list 'auto-mode-alist '("\\.classic\\.opa$" . opa-classic-mode))
;;
;; General Remarks:
;; 
;; This mode assumes that block comments are not nested inside block
;; comments and that strings do not contain line breaks.
;; 
;; Exported names start with "opa-js-" whereas private names start
;; with "js-".
;; 
;; Changes:
;;
;; See opa-js.el.changelog.

;;; Code:

(require 'cc-mode)
(require 'font-lock)
(require 'newcomment)

(defmacro inc(a) (list 'setq a (list '1+ a)))
(defmacro dec(a) (list 'setq a (list '1- a)))

(defgroup opa-js nil 
  "Customization variables for `opa-js-mode'."
  :tag "Opa-Js"
  :group 'languages)

(defcustom opa-js-indent-level 4
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'opa-js)

(defcustom opa-js-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued
expressions. The value must be no less than minus
`opa-js-indent-level'."
  :type 'integer
  :group 'opa-js)

(defcustom opa-js-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :type 'boolean
  :group 'opa-js)


;; --- Keymap ---

(defvar opa-js-mode-map nil 
  "Keymap used in Opa-Js mode.")

(unless opa-js-mode-map 
  (setq opa-js-mode-map (make-sparse-keymap)))

(when opa-js-auto-indent-flag
  (mapc (lambda (key) 
	  (define-key opa-js-mode-map key 'opa-js-insert-and-indent))
	'("{" "}" "(" ")" ":" ";" ",")))

(defun opa-js-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))


;; --- Syntax Table And Parsing ---

(defvar opa-js-mode-syntax-table-xhtml nil
  "Syntax table used in `opa-js-mode' buffers for xhtml constructs.")
(if opa-js-mode-syntax-table-xhtml
    nil
  (setq opa-js-mode-syntax-table-xhtml (make-syntax-table))
  (setq i 0) (while (< i 256)
               (modify-syntax-entry i "." opa-js-mode-syntax-table-xhtml)
               (inc i))
  )

(defvar opa-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?\' "w" table)
    table)
  "Syntax table used in Opa-Js mode.")

(defvar js-ident-as-word-syntax-table
  (let ((table (copy-syntax-table opa-js-mode-syntax-table)))
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?% "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\' "w" table)
    table)
  "Alternative syntax table used internally to simplify detection
  of identifiers and keywords and its boundaries.")


(defun js-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward 
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun js-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun js-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (backward-char))
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse) 
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))


(defun js-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


;; --- Font Lock ---

(defface opa-font-stop-face
  '((t (:background "red" :foreground "white")))
  "Opa font stop face"
  :group 'opa-js-faces)

(defface opa-font-xhtml-face
  '((t :inherit font-lock-string-face))
  "Opa face to use for XHTML code"
  :group 'opa-faces)

(defface opa-font-raw-ident-face
  '((t :underline t))
  "Opa face for raw idents (between backquotes)"
  :group 'opa-faces)

(defface opa-font-doc-directive-face
  '((t :foreground "turquoise3"))
  "Opa face to use for opadoc directives"
  :group 'opa-faces)

(defface opa-font-doc-title-face
  '((default (:inherit font-lock-doc-face))
    (t (:weight bold)))
  "Opa face to use for titles in opadoc"
  :group 'opa-faces)

(defface opa-font-directive-face
  '((t (:foreground "orangered3")))
  "Opa js-like face for directives"
  :group 'opa-js-faces)

(defface opa-font-ppdebug-face
  '((t (:bold t)))
  "Opa js-like face for ppdebug directives"
  :group 'opa-js-faces)

(defun js-inside-param-list-p ()
  "Return non-nil if point is inside a function parameter list."
  (condition-case err
      (save-excursion
	(up-list -1)
	(and (looking-at "(")
	     (progn (backward-word 1)
		    (or (looking-at "function")
			(progn (backward-word 1) (looking-at "function"))))))
    (error nil)))


(defconst js-function-heading-1-re 
  "^[ \t]*function[ \t]+\\(\\w+\\)"
  "Regular expression matching the start of a function header.")

(defconst js-function-heading-2-re 
  "^[ \t]*\\(\\w+\\)[ \t]*:[ \t]*function\\>"
  "Regular expression matching the start of a function entry in
  an associative array.")

(defvar bident-regexp "[a-zA-Z0-9_]*")
(defvar ident-regexp (concat "[a-zA-Z_']" bident-regexp))
(defvar type-ident-regexp (concat "[a-zA-Z_']" bident-regexp))
(defvar dotted-type-ident-regexp (concat "\\(" ident-regexp "\\.\\)*" type-ident-regexp))

;; OPA directives, bit confused in js-like as to which are preceded by @ and which aren't
;; Here, we allow both for all directives, @private and private will both be highlighted
(defconst opa-directives
  '("xml" "typeval" "static_content_directory" "static_resource_directory" "static_source_content"
    "static_binary_content" "static_include_directory" "catch" "client" "fail" "typeof" "lazy" "lazy_record"
    "thread_context" "with_thread_context" "throw" "track" "wrap" "unwrap" "callcc" "uncps" "atomic" "js_ident"
    "expand" "spawn" "wait" "server" "unsafe_cast" "toplevel" "assert" "opensums" "publish" "both" "prefer_client"
    "prefer_server" "prefer_both" "both_implem" "private" "public" "package" "nonexpansive" "asynchronous"
    "compiletime" "sliced_expr" "may_cps" "llarray" "specialize" "server_private" "opacapi" "stringifier"
    "xmlizer" "serializer" "comparator" "abstract" "exposed" "protected" "deprecated" "i18n"))

(defconst opa-all-directives
    (mapcar (function (lambda (s) (concat "@" s))) opa-directives))

(defconst js-directive-re
  (regexp-opt (append opa-directives opa-all-directives) 'words)
  "Regular expression matching OPA directives")

(defconst js-keyword-re
  (regexp-opt '("then" "else" "with" "end" "true" "false" "open"
                "type" "if" "match" "do" "parser" "xml_parser" "database" "rec" "and" "as" "css" "db" "with"
                "function" "recursive" "case" "default" "module" "or" "var"
                "import" "package") 'words)
  "Regular expression matching any Opa-Js keyword.")

(defconst js-basic-type-re
  (regexp-opt '("bool" "float" "int" "string" "void") 'words)
  "Regular expression matching any predefined type in Opa-Js.")

(defconst js-constant-re
  (regexp-opt '("false" "nil" "true") 'words)
  "Regular expression matching any future reserved words in Opa-Js.")

(defconst js-font-lock-keywords-1
  (list 
   "\\<import\\>" 
   (list js-function-heading-1-re 1 font-lock-function-name-face)
   (list js-function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock.")

(defconst js-font-lock-keywords-2
  (append js-font-lock-keywords-1
          (list (list js-keyword-re 1 font-lock-keyword-face)
                (cons js-directive-re '(0 'opa-font-directive-face))
                ;(cons js-basic-type-re font-lock-type-face)
                (cons js-constant-re font-lock-constant-face)))
  "Level two font lock.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various useful functions

(defun buffer-substring1 (pos)
  (buffer-substring pos (1+ pos)))

;;(defun is-in-xhtml-zone (p)
;;  (eq (get-text-property p 'syntax-table) opa-js-mode-syntax-table-xhtml))

(defun is-in-ppdebug-zone ()
  (eq (get-text-property (point) 'face) 'opa-font-ppdebug-face))

(defun is-in-comment-zone ()
  (let ((prop (get-text-property (point) 'face)))
    (or (eq prop font-lock-comment-face)
        (eq prop font-lock-doc-face))))

(defun is-in-strict-comment-zone ()
  (eq (get-text-property (point) 'face) font-lock-comment-face))

(defun is-in-doc-zone ()
  (eq (get-text-property (point) 'face) font-lock-doc-face))

(defun is-in-keyword-zone-with-point (p)
  (eq (get-text-property p 'face) font-lock-keyword-face))

(defun is-in-keyword-zone ()
  (is-in-keyword-zone-with-point (point)))

(defun is-in-string-zone ()
  (eq (get-text-property (point) 'face) font-lock-string-face))

(defun is-in-type-zone ()
  (eq (get-text-property (point) 'face) font-lock-type-face))

(defun re-search-forward--skipping-comments (re end)
  (let ((found nil))
    (while (and (not found) (re-search-forward re end t))
      (setq found (not (is-in-comment-zone))))
    found))

(defun re-search-forward--skipping-strings-comments-ppdebug (re end)
  (let ((found nil))
    (while (and (not found) (re-search-forward re end t))
      (setq found (not (or (is-in-comment-zone) (is-in-string-zone) (is-in-ppdebug-zone)))))
    found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xhtml parsing and setting face property

; returns the beginning position and sets (point) to the end position
(defun find-nested--skipping-strings-comments (re_open re_close end)
  (if (re-search-forward--skipping-strings-comments-ppdebug re_open end)
      (let ((b (point))
            (re (concat re_open "\\|" re_close))
            (deep 1))
        (while (and (< (point) end) (re-search-forward--skipping-strings-comments-ppdebug re end))
          ;(message (concat "find-nested--skipping-strings-comments found at " (int-to-string (point)) " deep is " (int-to-string deep)))
          (goto-char (match-beginning 0))
          (if (looking-at re_close)
              (if (= (dec deep) 0) (setq end (point)))
            (inc deep))
          (forward-char) ; move to the next char otherwise we will search at the same place
          )
        b)))

(defun skip-attributes ()
  (while (looking-at " +[a-zA-Z][a-zA-Z0-9_:-]* *= *")
      (goto-char (match-end 0))
      (cond ((looking-at "\"[^\"]*\"")
             (goto-char (match-end 0)))
            ((looking-at "'[^\']*'")
             (goto-char (match-end 0)))
            ((looking-at "{[^}]*}")
             (goto-char (match-end 0)))
            (t (re-search-forward "[ \t/<>]")
               (backward-char)))))

;; starting from point being just after opening tag "tag", search the closing tag
(defun search-xhtml-end (tag)
  (skip-attributes)
  (if (and (not (equal tag "")) (looking-at "[ \n\t]*/>"))
      ; <tag/> format
      (match-end 0)
    (let ((re (concat "</?\\(" tag "\\)?>"))
        (end (1- (point-max)))
        (deep 1))
    (while (and (< (point) end) (re-search-forward--skipping-comments re (point-max)))
      ;(message (concat "search-xhtml-end looking for " tag " found at " (int-to-string (point)) " deep is " (int-to-string deep)))
      (if (equal (buffer-substring1 (1+ (match-beginning 0))) "/")
        (if (= (dec deep) 0) (setq end (match-end 0)))
        (inc deep)))
    ;(message (concat "search-xhtml-end looking for " tag " found at " (int-to-string (point)) " deep is " (int-to-string deep)))
    (goto-char end)
    (point))))

;; search an opening tag before "end"
(defun search-xhtml-beg-raw (end)
  (let ((found nil))
    (while (and (not found) )
      (setq found (not (is-in-comment-zone))))
    found))

(defun search-xhtml-beg (end)
  (let ((found (re-search-forward--skipping-strings-comments-ppdebug "<\\([a-zA-Z][a-zA-Z0-9:-]*\\|>\\)" end)))
    (if (and found (equal (buffer-substring1 (1- (point))) ">"))
        (backward-char))
    found))

(defun set-xhtml-property (beg end)
  (put-text-property beg end 'syntax-table opa-js-mode-syntax-table-xhtml)
  (put-text-property beg end 'face 'opa-font-xhtml-face)
  (put-text-property beg end 'font-lock-multiline t)

  (save-excursion
    (goto-char beg)
    (while (re-search-forward--skipping-strings-comments-ppdebug "<\\(/?[a-zA-Z][a-zA-Z0-9-]* */?\\)\\|/>" end)
      (if (equal (buffer-substring1 (match-beginning 0)) "<")
          (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face)
        ; colorize the "/" in "<zzzz .... />"
        (put-text-property (match-beginning 0) (1+ (match-beginning 0)) 'face 'font-lock-keyword-face))))
)

(defun set-xhtml-except-braces (beg end)
  (goto-char beg)
  (let ((beg_brace nil))
    (while (setq beg_brace (find-nested--skipping-strings-comments "[^\\]{" "[^\\]}" end))
      (set-xhtml-property beg beg_brace)
      (setq beg (point))))
  (set-xhtml-property beg end))


(defun opa-highlights-xhtml (end)
  ;(message (concat "opa-highlights-xhtml on " (int-to-string start) ".." (int-to-string end)))
  (if (search-xhtml-beg end)
      (let* ((b (match-beginning 0))
             (tag (buffer-substring (1+ b) (point)))
             (e (search-xhtml-end tag)))
        ;(message (concat "setting xhtml on " (int-to-string b) ".." (int-to-string e)))
        (set-xhtml-except-braces b e)
        e
        )))

(defun pnt (str) (message "%s" (concat str ": point=\"" (buffer-substring (point) (+ (point) 20)) "\"")))
(defun mtch (str) (message "%s" (concat str ": match=\"" (buffer-substring (match-beginning 0) (match-end 0)) "\"")))
(defun mtch-ft (str from to) (message "%s" (concat str ": match=\"" (buffer-substring from to) "\"")))

(defun match-opa-record-type-element (limit)
  (when (match-opa-type limit)
    (when (and (looking-at (concat " *" ident-regexp))
               (goto-char (match-end 0)))
      t)))

(defun match-opa-record-type (limit)
  (when (and (looking-at "{ *")
             (goto-char (match-end 0))
             (match-opa-record-type-element limit))
    (while (and (looking-at " *, *")
                (goto-char (match-end 0))
                (match-opa-record-type-element limit)))
    (when (looking-at " *}")
      (goto-char (match-end 0)))))

(defun match-opa-tuple-type (limit)
  (when (and (looking-at "( *")
             (goto-char (match-end 0))
             (match-opa-type limit))
    (while (and (looking-at " *, *")
                (goto-char (match-end 0))
                (match-opa-type limit)))
    (when (looking-at " *)")
      (goto-char (match-end 0)))))

(defun match-opa-simple-type (limit)
  (when (looking-at dotted-type-ident-regexp)
    (goto-char (match-end 0))
    (let ((data (match-data)))
      (if (match-opa-tuple-type limit)
          t
        (set-match-data data)
        t))))

(defun match-opa-param-type (limit)
  (when (looking-at dotted-type-ident-regexp)
    (goto-char (match-end 0))
    (when (and (looking-at " *")
               (goto-char (match-end 0))
               (match-opa-tuple-type limit)))))

(defun match-opa-nonfun-type (limit)
  (cond
   ((match-opa-simple-type limit) t)
   ((match-opa-tuple-type limit) t)
   ((match-opa-record-type limit) t)
   ((match-opa-param-type limit) t)
   (t nil)))

;; parsing the lhs of an arrow types ie a comma separated list (perhaps empty) of types
;; @return -1 if nothing was parsed
;;         otherwise, the position of the end of the first type parsed
;;                    (ie before the first comma, if any)
(defun match-opa-arrow-lhs (limit)
  (let ((before-first-comma -1))
    (when (match-opa-nonfun-type limit)
      (setq before-first-comma (match-end 0))
      (while (and (looking-at " *, *")
                  (goto-char (match-end 0))
                  (match-opa-nonfun-type limit))))
    before-first-comma))

;; not parsing a -> b -> c on purpose because is it not valid syntax (anymore)
;; if you modify it, beware that you must parse [a -> c], [a,b -> c], [-> c]
;; but NOT [a,b] or []
(defun match-opa-type2 (limit)
  (let ((before-first-comma (match-opa-arrow-lhs limit)))
    (if (and (looking-at " *-> *") ;; if you see an arrow, parse it as a type
             (goto-char (match-end 0))
             (match-opa-nonfun-type limit))
      (progn (goto-char (match-end 0)) t)
      ;; if you don't see any arrow, two choices
      (if (= before-first-comma -1)
          ;; you didn't parse anything, so you fail in parsing the type
          nil
          ;; you parsed one type (and possibly others with separated commas)
        (progn
          ;; use the first one of these types: in [ident:a,b], only [a]
          ;; is a valid type since no arrow follows
          (set-match-data (list before-first-comma before-first-comma))
            ;; setting match-data with a wierd value but the following
            ;; function will read just the second value anyway
          t)))))

(defun match-opa-type (limit)
  ;(pnt "match-opa-type")
  (cond
   ((match-opa-type2 limit) t)
   ((match-opa-simple-type limit) t)
   ((match-opa-tuple-type limit) t)
   ((match-opa-record-type limit) t)
   ((match-opa-param-type limit) t)
   (t nil)))

(defun match-variable-declaration (limit)
  ;(message "match-variable-declaration ")
  (when (re-search-forward "\\<\\(var\\)\\> *" limit t)
    ;(mtch "match-variable-declaration")
    (goto-char (match-end 0))
    (let ((start (match-end 0)))
      ;(mtch-ft "match-variable-declaration(start)" start (+ start 20))
      (when (match-opa-type limit)
        (let ((end (match-end 0)))
          ;(mtch-ft "match-variable-declaration(end)" (- end 20) end)
          (when (looking-at "[ \t]*\\([a-zA-Z0-9_,()]+\\)[ \t]*\\([=;].*\\|/[/*]\\|$\\)")
            ;(mtch-ft "match-variable-declaration type" start end)
            ;(mtch-ft "match-variable-declaration var" (match-beginning 1) (match-end 1))
            (set-match-data (list start end (match-beginning 1) (match-end 1)))
            (<= end limit)))))))

(defun match-variable-assign (limit)
  (when (and (re-search-forward " *(" limit t)
             (goto-char (match-end 0)))
    (let ((start (match-end 0)))
      (when (match-opa-type limit)
        (let ((end (match-end 0)))
          (when (looking-at "[ \t]*)[ \t]*\\([a-zA-Z0-9_,()]+\\)[ \t]*\\([=;].*\\|/[/*]\\|$\\)")
            (set-match-data (list start end (match-beginning 1) (match-end 1)))
            (<= end limit)))))))

(defun match-opa-doc-directive-aux (limit)
  (if (re-search-forward "@[a-zA-Z]+" limit t)
      (if (is-in-doc-zone) 'succeed 'continue)
    'stop))

(defun match-opa-doc-title-aux (limit)
  (if (re-search-forward "{[0-9]+\\([^}]*\\)}" limit t)
      (if (is-in-doc-zone) 'succeed 'continue)
    'stop))

(defun skip-coercion-and-comments (limit)
  (let ((continue t))
    (while (and continue (< (point) limit))
      (looking-at "\s*")
      (goto-char (match-end 0))
      ;(if (looking-at ":\s*");; <-- FIX, need typedef regex
      ;    (goto-char (match-end 0))
        (if (or (is-in-comment-zone) (is-in-type-zone))
            (forward-char)
          (setq continue nil)))));)

(defun match-opa-binding-aux (limit)
  ;; matching a.b.c so that in {r with a.b.c : 1} a.b.c highlighted in pink
  (if (re-search-forward (concat "\\<" ident-regexp "\\>" "\\(\\.\\<" ident-regexp "\\>\\)*") limit t)
      (let ((begin (match-beginning 0)))
        (skip-coercion-and-comments limit)
        (if (and (looking-at "\s*\\(:\\)\\([a-zA-Z_0-9 ]\\|$\\)") (< (point) limit))
            (let ((end (match-end 1)))
              (set-match-data (list begin end))
              'succeed)
          'continue
          ))
    'stop))

(defun match-wrapper (f limit)
  (let ((result)
        (continue t))
    (while continue
      (let ((res (funcall f limit)))
        (if (eq res 'continue)
            t
          (if (eq res 'succeed)
              (progn
                (setq result t)
                (setq continue nil))
            (progn ; res = 'stop
              (setq result nil)
              (setq continue nil))))))
    result))

(defun match-opa-doc-directive (limit)
  (match-wrapper 'match-opa-doc-directive-aux limit))

(defun match-opa-binding (limit)
  (match-wrapper 'match-opa-binding-aux limit))

(defun match-opa-doc-title (limit)
  (match-wrapper 'match-opa-doc-title-aux limit))

(defun match-invalid-space-aux (limit)
  (if (re-search-forward "[a-zA-Z0-9_)]\\(  *\\)([^*]" limit t)
      (if (is-in-keyword-zone-with-point (match-beginning 0))
          ; if (1 == 2) shouldn't complain because of the space after "if"
          'continue
        'succeed)
    'stop))

(defun match-invalid-space (limit)
  (match-wrapper 'match-invalid-space-aux limit))

;; Limitations with variable declarations: There seems to be no
;; sensible way to highlight variables occuring after an initialized
;; variable in a variable list. For instance, in
;;
;;    var x, y = f(a, b), z
;;
;; z will not be highlighted. Also, in variable declaration lists
;; spanning several lines only variables on the first line will be
;; highlighted. To get correct fontification, every line with variable
;; declarations must contain a `var' keyword.

(defconst js-font-lock-keywords-3
  (append 
   js-font-lock-keywords-2
   (list 

    ; bypasses (must be done before other rules in case the bypass contains weird chars, eg: %%+%%)
    (cons "%%[^%]*%%" '(0 'font-lock-string-face))

    ;; ppdebug
    (cons "#<[^>]*>" '(0 'opa-font-ppdebug-face))

    ;; var declarations with types: var typ nam = ...
    '(match-variable-declaration . '((0 font-lock-type-face) (1 font-lock-variable-name-face)))

    ;; assignments with types (typ) name = ...
    '(match-variable-assign . '((0 font-lock-type-face) (1 font-lock-variable-name-face)))

    ;; subrule name (parser)
    (cons (concat "\\(" ident-regexp "=\\)") '(0 font-lock-builtin-face))

    ;; some more operators
    (cons "\\(->\\|<-\\|-\\)" 0)

    ;; invalid spaces between function name and open parenthesis
    '(match-invalid-space . '(1 'opa-font-stop-face))

    ;; opa-doc directives
    (cons 'match-opa-doc-directive '(0 'opa-font-doc-directive-face t))
    (cons 'match-opa-doc-title '(1 'opa-font-doc-title-face t))

    ;; xhtml
    'opa-highlights-xhtml

    ;; variable declarations
    ;(list
    ; (concat "\\<\\(const\\|var\\)\\>\\|" js-basic-type-re)
    ; (list "\\(\\w+\\)[ \t]*\\([=;].*\\|\\<in\\>.*\\|,\\|/[/*]\\|$\\)"
	;   nil
	;   nil
	;   '(1 font-lock-variable-name-face)))

    ;; function call
    ;; must come after types since list('a) is a type
    (cons (concat "\\(" ident-regexp "\\(\\." ident-regexp "\\)*\\)(") '(1 font-lock-function-name-face))

    ;; dot call
    ;; must come after function call
    (cons (concat "\\." ident-regexp) '(0 font-lock-constant-face))

    ;; formal parameters
    (list
     "\\<function\\>\\([ \t]+\\w+\\)?[ \t]*([ \t]*\\w"
     (list "\\(\\w+\\)\\([ \t]*).*\\)?"
	   '(backward-char)
	   '(end-of-line)
	   '(1 font-lock-variable-name-face)))
    
    ;; continued formal parameter list
    (list
     "^[ \t]*\\w+[ \t]*[,)]"
     (list "\\w+"
	   '(if (save-excursion (backward-char) (js-inside-param-list-p))
		(backward-word 1) 
	      (end-of-line))
	   '(end-of-line)
	   '(0 font-lock-variable-name-face)))
))
  "Level three font lock.")

(defconst js-font-lock-keywords
  '(js-font-lock-keywords-3 js-font-lock-keywords-1 js-font-lock-keywords-2 js-font-lock-keywords-3)
  "See `font-lock-keywords'.")

(defconst js-font-lock-syntactic-keywords
  '(("[=(][ \t\n]*\\(/\\)[^/*]\\(.*?[^\\]\\)?\\(/\\)" (1 '(7)) (3 '(7)))
    ("##\\(register\\|extern-type\\)[^\n]*" 0 " ")
    )
  "Highlighting of regular expressions. See also the variable
  `font-lock-keywords'.")


;; --- Indentation ---

(defconst js-possibly-braceless-keyword-re
  (regexp-opt
   '("else" "if" "try" "with")
   'words)
  "Regular expression matching keywords that are optionally
  followed by an opening brace.")

(defconst js-indent-operator-re
  (concat "[-+*/<>=&^|?:.]\\([^-+*/]\\|$\\)")
  "Regular expression matching operators that affect indentation
  of continued expressions.")


(defun js-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not a comma)."
  (let ((res
         (progn
           ;(pnt "js-looking-at-operator-p")
           (save-match-data
             (and (looking-at js-indent-operator-re)
                  (or (not (looking-at ":"))
                      (save-excursion
                        (and (js-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                             (looking-at "?")))))))))
    ;(message (concat "js-looking-at-operator-p" " res " (if res "t" "nil")))
    res))


(defun js-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    ;(pnt "js-continued-expression-p 1")
    (or (js-looking-at-operator-p)
        (and (js-re-search-backward "\n" nil t)
	     (progn 
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js-end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\<while\\>")
	(if (save-excursion 
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion 
	      (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
	  (js-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (js-re-search-forward 
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun js-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (save-excursion
    ;(message "start js-ctrl-statement-indentation")
    (back-to-indentation)
    (when (save-excursion
            (and (not (looking-at "[{]"))
                 (progn
                   (js-re-search-backward "[[:graph:]]" nil t)
                   (forward-char)
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w")
                   (looking-at js-possibly-braceless-keyword-re))
                 (not (js-end-of-do-while-loop-p))))
      (save-excursion
        ;(message "js-ctrl-statement-indentation succeed")
        (goto-char (match-beginning 0))
        (+ (current-indentation) opa-js-indent-level)))))


(defun js-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    ;(message "js-proper-indentation")
    (back-to-indentation)
    (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
          (continued-expr-p (js-continued-expression-p)))
      (cond (ctrl-stmt-indent)
            ((nth 1 parse-status)
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                 (progn
                   ;(pnt "open bracket at end of line")
                   (skip-syntax-backward " ")
                   (when (= (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          ;(pnt "current column 1")
                          (current-column))
                         (continued-expr-p
                          ;(pnt "continued expr")
                          (+ (current-column) (* 2 opa-js-indent-level)
                             opa-js-expr-indent-offset))
                         (t
                          ;(pnt "not continued expr")
                          ;(message (concat "currentcol " (int-to-string (current-column))))
                          (+ (current-column) opa-js-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               ;(pnt "current column 1")
               (current-column)))
	    (continued-expr-p (+ opa-js-indent-level 
                                 opa-js-expr-indent-offset))
            (t 0)))))


(defun b2s (s)
  (cond ((booleanp s) (if s "t" "nil"))
        ((numberp s) (int-to-string s))
        ((stringp s) s)
        ((sequencep s) "sequence")
        (t (type-of s))))

(defun opa-js-indent-line ()
  "Indent the current line as Opa-Js source text."
  (interactive)
  ;(message "opa-js-indent-line")
  (with-syntax-table js-ident-as-word-syntax-table
    (let ((parse-status 
	   (save-excursion (parse-partial-sexp (point-min) (point-at-bol))))
	  (offset (- (current-column) (current-indentation))))
      (cond ((nth 4 parse-status)
              (let ((base (save-excursion
                            (re-search-backward "/\\*" (point-min) t)
                            (current-column))))
                (indent-line-to (+ base 1)))
	      (when (> offset 0) (forward-char offset)))
            ((not (nth 8 parse-status))
;              (message (concat "indent"
;                               " depth " (b2s (nth 0 parse-status))
;                               " innerlist " (b2s (nth 1 parse-status))
;                               " sexplast " (b2s (nth 2 parse-status))
;                               " instring " (b2s (nth 3 parse-status))
;                               " incomment " (b2s (nth 4 parse-status))
;                               " afterquote " (b2s (nth 5 parse-status))
;                               " minparen " (b2s (nth 6 parse-status))
;                               " abcomment " (b2s (nth 7 parse-status))
;                               " lstcomstr " (b2s (nth 8 parse-status))
;                               " intdata " (b2s (nth 9 parse-status))
;                               ))
             (indent-line-to (js-proper-indentation parse-status))
             (when (> offset 0) (forward-char offset)))))))
  

;; --- Filling ---

;; FIXME: It should be possible to use the more sofisticated function
;; `c-fill-paragraph' in `cc-cmds.el' instead. However, just setting
;; `fill-paragraph-function' to `c-fill-paragraph' does not work;
;; inside `c-fill-paragraph', `fill-paragraph-function' evaluates to
;; nil!?

(defun js-backward-paragraph ()
  "Move backward to start of paragraph. Postcondition: Point is at
beginning of buffer or the previous line contains only whitespace."
  (forward-line -1)
  (while (not (or (bobp) (looking-at "^[ \t]*$")))
    (forward-line -1))
  (when (not (bobp)) (forward-line 1)))


(defun js-forward-paragraph ()
  "Move forward to end of paragraph. Postcondition: Point is at
end of buffer or the next line contains only whitespace."
  (forward-line 1)
  (while (not (or (eobp) (looking-at "^[ \t]*$")))
    (forward-line 1))
  (when (not (eobp)) (backward-char 1)))
 

(defun js-fill-block-comment-paragraph (parse-status justify)
  "Fill current paragraph as a block comment. PARSE-STATUS is the
result of `parse-partial-regexp' from beginning of buffer to
point. JUSTIFY has the same meaning as in `fill-paragraph'."
  (let ((offset (save-excursion 
                  (goto-char (nth 8 parse-status)) (current-indentation))))
    (save-excursion
      (save-restriction
        (narrow-to-region (save-excursion 
                            (goto-char (nth 8 parse-status)) (point-at-bol))
                          (save-excursion 
			    (goto-char (nth 8 parse-status))
			    (re-search-forward "*/")))
        (narrow-to-region (save-excursion 
                            (js-backward-paragraph)
                            (when (looking-at "^[ \t]*$") (forward-line 1))
                            (point))
                          (save-excursion 
                            (js-forward-paragraph) 
                            (when (looking-at "^[ \t]*$") (backward-char))
                            (point)))
        (goto-char (point-min))
        (while (not (eobp))
          (delete-horizontal-space)
          (forward-line 1))
        (let ((fill-column (- fill-column offset))
              (fill-paragraph-function nil))
          (fill-paragraph justify))

        ;; In Emacs 21.4 as opposed to CVS Emacs 22,
        ;; `fill-paragraph' seems toadd a newline at the end of the
        ;; paragraph. Remove it!
        (goto-char (point-max))
        (when (looking-at "^$") (backward-delete-char 1))

        (goto-char (point-min))
        (while (not (eobp))
          (indent-to offset)
          (forward-line 1))))))


(defun js-sline-comment-par-start ()
  "Return point at the beginning of the line where the current
single-line comment paragraph starts."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp)) 
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line -1))
    (unless (bobp) (forward-line 1))
    (point)))


(defun js-sline-comment-par-end ()
  "Return point at end of current single-line comment paragraph."
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp)) 
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line 1))
    (unless (bobp) (backward-char))
    (point)))


(defun js-sline-comment-offset (line)
  "Return the column at the start of the current single-line
comment paragraph."
  (save-excursion 
    (goto-line line)
    (re-search-forward "//" (point-at-eol))
    (goto-char (match-beginning 0))
    (current-column)))


(defun js-sline-comment-text-offset (line)
  "Return the column at the start of the text of the current
single-line comment paragraph."
  (save-excursion
    (goto-line line)
    (re-search-forward "//[ \t]*" (point-at-eol))
    (current-column)))


(defun js-at-empty-sline-comment-p ()
  "Return non-nil if inside an empty single-line comment."
  (and (save-excursion
         (beginning-of-line)
         (not (looking-at "^.*//.*[[:graph:]]")))
       (save-excursion
         (re-search-backward "//" (point-at-bol) t))))

         
(defun js-fill-sline-comments (parse-status justify)
  "Fill current paragraph as a sequence of single-line comments.
PARSE-STATUS is the result of `parse-partial-regexp' from
beginning of buffer to point. JUSTIFY has the same meaning as in
`fill-paragraph'."
  (when (not (js-at-empty-sline-comment-p))
    (let* ((start (js-sline-comment-par-start))
           (start-line (1+ (count-lines (point-min) start)))
           (end (js-sline-comment-par-end))
           (offset (js-sline-comment-offset start-line))
           (text-offset (js-sline-comment-text-offset start-line)))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*//[ \t]*" nil t)
            (replace-match "")
            (forward-line 1))
          (let ((fill-paragraph-function nil)
                (fill-column (- fill-column text-offset)))
            (fill-paragraph justify))

          ;; In Emacs 21.4 as opposed to CVS Emacs 22,
          ;; `fill-paragraph' seems to add a newline at the end of the
          ;; paragraph. Remove it!
          (goto-char (point-max))
          (when (looking-at "^$") (backward-delete-char 1))

          (goto-char (point-min))
          (while (not (eobp))
            (indent-to offset)
            (insert "//")
            (indent-to text-offset)
            (forward-line 1)))))))
  

(defun js-trailing-comment-p (parse-status)
  "Return non-nil if inside a trailing comment. PARSE-STATUS is
the result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion 
    (when (nth 4 parse-status)
      (goto-char (nth 8 parse-status))
      (skip-chars-backward " \t")
      (not (bolp)))))


(defun js-block-comment-p (parse-status)
  "Return non-nil if inside a block comment. PARSE-STATUS is the
result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion 
    (save-match-data
      (when (nth 4 parse-status)
        (goto-char (nth 8 parse-status))
        (looking-at "/\\*")))))


(defun opa-js-fill-paragraph (&optional justify)
  "If inside a comment, fill the current comment paragraph.
Trailing comments are ignored."
  (interactive)
  (let ((parse-status (parse-partial-sexp (point-min) (point))))
    (when (and (nth 4 parse-status) 
               (not (js-trailing-comment-p parse-status)))
      (if (js-block-comment-p parse-status)
          (js-fill-block-comment-paragraph parse-status justify)
        (js-fill-sline-comments parse-status justify))))
  t)


;; --- Imenu ---

(defconst js-imenu-generic-expression 
  (list
   (list
    nil 
    "function\\s-+\\(\\(\\w\\|\\s_\\)+\\)\\s-*("
    1))
  "Regular expression matching top level procedures. Used by imenu.")


;; --- Main Function ---

;;;###autoload
(defun opa-js-mode ()
  "Major mode for editing Opa-Js source text.

Key bindings:

\\{opa-js-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map opa-js-mode-map)
  (set-syntax-table opa-js-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'opa-js-indent-line)

  (unless (featurep 'xemacs)
    (set (make-local-variable 'font-lock-defaults) 
         (list js-font-lock-keywords 
               nil nil '((?$ . "w") (?_ . "w")) nil
               '(font-lock-syntactic-keywords . js-font-lock-syntactic-keywords))))

  (set (make-local-variable 'parse-sexp-ignore-comments) t) 

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'fill-paragraph-function) 
       'opa-js-fill-paragraph)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression)
       js-imenu-generic-expression)
  (imenu-add-to-menubar "Functions")

  (setq major-mode 'opa-js-mode)
  (setq mode-name "Opa-Js")
  (run-hooks 'opa-js-mode-hook))


(provide 'opa-js-mode)
;;; opa-js-mode.el ends here
