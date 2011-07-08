;;; opa-mode.el --- Major mode for editing OPA programs

;; To automatically use this mode on .opa files, add the following to your
;; .emacs:
;;  (autoload 'opa-mode "/usr/share/opa/emacs/opa-mode.el" "OPA editing mode." t)
;;  (add-to-list 'auto-mode-alist '("\\.opa$" . opa-mode))


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defcustom opa-indent-offset 2
  "*Amount of offset per level of indentation"
  :type 'integer
  :group 'opa)

(defface opa-font-stop-face
  '((t
     (:foreground "White")
     (:background "Red")))
  "Opa font stop face"
  :group 'opa-faces)

(defface opa-font-ppdebug-face
  '((t :weight bold))
  "Opa face for ppdebug directives"
  :group 'opa-faces)

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
  '((t :foreground "orangered3"))
  "Opa face for directives"
  :group 'opa-faces)


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar bident-regexp "[a-zA-Z0-9_]*")
(defvar ident-regexp (concat "[a-zA-Z_']" bident-regexp))
(defvar type-ident-regexp (concat "[a-zA-Z_']" bident-regexp))
(defvar dotted-type-ident-regexp (concat "\\(" ident-regexp "\\.\\)*" type-ident-regexp))

(defconst opa-intdent-keywords-cont
  '("then" "else" "with" "end" "true" "false" "open"))
(defconst opa-intdent-keywords (append opa-intdent-keywords-cont
  '("type" "if" "match" "do" "parser" "xml_parser" "database" "server" "rec" "and" "as" "css" "db" "with" "val"
    "import" "import-plugin" "package")))
(defconst opa-directives
  '("xml" "typeval" "static_content_directory" "static_resource_directory" "static_source_content" "static_binary_content" "static_include_directory" "catch" "client" "fail" "typeof" "lazy" "lazy_record" "thread_context" "with_thread_context" "throw" "track" "wrap" "unwrap" "callcc" "uncps" "atomic" "js_ident" "expand" "spawn" "wait" "server" "unsafe_cast" "toplevel" "assert" "opensums" "publish" "publish_async" "both" "prefer_client" "prefer_server" "prefer_both" "both_implem" "abstract" "private" "public" "package" "nonexpansive" "async" "compiletime" "sliced_expr" "may_cps" "llarray" "specialize" "specialize_strict" "server_private" "opacapi" "stringifier" "xmlizer" "serializer" "comparator" "deprecated" "todo"))

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

(defun match-opa-nonfun-type (limit)
  (cond
   ((match-opa-tuple-type limit)
    t)
   ((match-opa-simple-type limit)
    t)))

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
(defun match-opa-type (limit)
  (let ((before-first-comma (match-opa-arrow-lhs limit)))
    (if (and (looking-at " *-> *") ;; if you see an arrow, parse it an a type
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

(defun match-opa-type-annotation (limit)
  (when (re-search-forward ": *\\|@typeval( *" limit t)
    (goto-char (match-end 0))
    (let ((start (if (eq (char-after (match-beginning 0)) ?:)
                     (match-beginning 0); fontifying the ':'
                   (match-end 0))))
      (when (match-opa-type limit)
        (let ((end (match-end 0)))
          (set-match-data (list start end))
          (<= end limit))))))

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
      (if (looking-at ":\s*")
          (goto-char (match-end 0))
        (if (or (is-in-comment-zone) (is-in-type-zone))
            (forward-char)
          (setq continue nil))))))

(defun match-opa-binding-aux (limit)
  ;; matching a.b.c so that in {r with a.b.c = 1} a.b.c highlighted in pink
  (if (re-search-forward (concat "\\<" ident-regexp "\\>" "\\(\\.\\<" ident-regexp "\\>\\)*") limit t)
      (let ((begin (match-beginning 0)))
        (skip-coercion-and-comments limit)
        (if (and (looking-at "\s*\\(=\\)\\([a-zA-Z_0-9 ]\\|$\\)") (< (point) limit))
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

(defvar opa-font-lock-keywords
    (list
     ;; bypasses (must be done before other rules in case the bypass contains weird chars, eg: %%+%%)
     (cons "%%[^%]*%%" '(0 font-lock-string-face))

     (cons "`[^`]*`" '(0 'opa-font-raw-ident-face))

     ;; directives
     (cons (concat "@" (regexp-opt opa-directives 'words)) '(0 'opa-font-directive-face))

     ;; keywords
     (cons (concat "\\(" (regexp-opt opa-intdent-keywords 'words) "\\)" "\\([ \n\t]\\|[^(]\\|$\\)") 1)

     ;; ppdebug
     (cons "#<[^>]*>" '(0 'opa-font-ppdebug-face))

     ;; type annotation
     (cons #'match-opa-type-annotation '(0 font-lock-type-face))

     ;; let-in
     '(match-opa-binding . (0 font-lock-builtin-face keep))

     ;; operators, separators
     (cons (concat "[+*{};\\]") 0)

     ;; database value declaration
     (cons "^\\(db\\) */" 1)

     ;; subrule name (parser)
     ;;(cons "\\([a-zA-Z]+\\)\\(=\\)" '((1 font-lock-comment-face) (2 font-lock-builtin-face)))
     (cons "[a-zA-Z_]+=" '(0 font-lock-builtin-face))

     ;; function call
     (cons (concat "\\(" ident-regexp "\\(\\." ident-regexp "\\)*\\)(") '(1 font-lock-function-name-face))

     ;; dot call
     (cons (concat "\\." ident-regexp) '(0 font-lock-constant-face))

     ;; some more operators
     (cons "\\(->\\|<-\\|-\\)" 0)

     ;; invalid spaces between function name and open parenthesis
     ;(cons "[a-zA-Z0-9_)]\\(  *\\)([^*]" '(1 'opa-font-stop-face))
     '(match-invalid-space . (1 'opa-font-stop-face))

     ;; opa-doc directives
     (cons 'match-opa-doc-directive '(0 'opa-font-doc-directive-face t))
     (cons 'match-opa-doc-title '(1 'opa-font-doc-title-face t))

     ;; xhtml
     'opa-highlights-xhtml

;     (cons (concat (mapconcat 'identity '("[,.:=]") "\\|")) '(0 font-lock-builtin-face))
     )
  "Additional expressions to highlight in OPA mode.")
(put 'opa-mode 'font-lock-defaults '(opa-font-lock-keywords))

(make-variable-buffer-local 'opa-indent-offset)

;; same as tuareg mode, /** for documentation
(defun opa-font-lock-syntactic-face-function (state)
  (if (nth 3 state) font-lock-string-face
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          font-lock-doc-face
        font-lock-comment-face))))


;; Major mode boilerplate
(defmacro inc(a) (list 'setq a (list '1+ a)))
(defmacro dec(a) (list 'setq a (list '1- a)))

;; define a mode-specific abbrev table for those who use such things
(defvar opa-mode-abbrev-table nil
  "Abbrev table in use in `opa-mode' buffers.")
(define-abbrev-table 'opa-mode-abbrev-table nil)

(defvar opa-mode-hook nil
  "*Hook called by `opa-mode'.")

(defvar opa-mode-syntax-table nil
  "Syntax table used in `opa-mode' buffers.")
(if opa-mode-syntax-table
    nil
  (setq opa-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" opa-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" opa-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" opa-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" opa-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" opa-mode-syntax-table)
  (modify-syntax-entry ?\} "){" opa-mode-syntax-table)
  ;; double quote are string delimiters
  (modify-syntax-entry ?\" "\"" opa-mode-syntax-table)

  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\* "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\- "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\< "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\= "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\> "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\| "."  opa-mode-syntax-table)

  (modify-syntax-entry ?\; "."  opa-mode-syntax-table)
  (modify-syntax-entry ?\= "."  opa-mode-syntax-table)

  ; / is first character of comment start
  ; and last character of comment end
  (modify-syntax-entry ?\/ ". 124b" opa-mode-syntax-table)
  ; * is second character of comment start,
  ; and first character of comment end
  (modify-syntax-entry ?*  ". 23n" opa-mode-syntax-table)
  ; end-of-line finishes // comment
  (modify-syntax-entry 10  "> b" opa-mode-syntax-table)


  )

(defvar opa-mode-syntax-table-xhtml nil
  "Syntax table used in `opa-mode' buffers for xhtml constructs.")
(if opa-mode-syntax-table-xhtml
    nil
  (setq opa-mode-syntax-table-xhtml (make-syntax-table))
  (setq i 0) (while (< i 256)
               (modify-syntax-entry i "." opa-mode-syntax-table-xhtml)
               (inc i))
  )


;;;###autoload
(defun opa-mode ()
  "Major mode for editing OPA files"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
;  (make-local-variable 'indent-region-function)
;  (make-local-variable 'indent-line-function)
  ;;
  (set-syntax-table opa-mode-syntax-table)

  ; don't know what this does exactly, but the javascript-mode does
  ; that and it seems to make ?_ a word constituent for the purpose of
  ; fontification but not for moving the point (awesome!)
  (set (make-local-variable 'font-lock-defaults)
       (list opa-font-lock-keywords
             nil nil '((?_ . "w")) nil))

  (setq major-mode             'opa-mode
        mode-name              "OPA"
        local-abbrev-table     opa-mode-abbrev-table
        font-lock-syntactic-face-function 'opa-font-lock-syntactic-face-function
        paragraph-separate     "^[ \t]*$"
        paragraph-start        "^[ \t]*$"
        require-final-newline  t
        comment-start          "// "
        comment-end            ""
        comment-start-skip     "// *"
        comment-column         40
        indent-tabs-mode       nil
;	indent-region-function 'opa-indent-region
;	indent-line-function   'opa-indent-line
        )
  (use-local-map opa-mode-map)
  ;; Run the mode hook.
  (if opa-mode-hook (run-hooks 'opa-mode-hook))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various useful functions

(defun buffer-substring1 (pos)
  (buffer-substring pos (1+ pos)))

(defun is-in-xhtml-zone (p)
  (eq (get-text-property p 'syntax-table) opa-mode-syntax-table-xhtml))

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
  (put-text-property beg end 'syntax-table opa-mode-syntax-table-xhtml)
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation

(require 'compile)
(require 'ansi-color)
(defun opa-set-compile-command ()
  "Hook to set compile-command locally, unless there is a Makefile"
  (interactive)
  (unless (or (null buffer-file-name)
              (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (setq command "opa")
    (progn
      (make-local-variable 'compile-command)
      (setq compile-command (concat command " " filename))))
  )

(add-hook 'opa-mode-hook 'opa-set-compile-command)

(define-compilation-mode opa-compile-mode "opa-compile" "opa-compile-mode"
  (set (make-local-variable 'compilation-disable-input) t)
  (make-local-variable 'compilation-error-alist)
  (setq compilation-error-regexp-alist
        '(
          ;the format of syntax errors (to be unified)
          ("In \\(.+\\) \\[\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\) | global chars=[0-9]+-[0-9]+\\]" 1 (2 . 4) (3 . 5) 1)

          ;this fallback one presents the advantage that it does also match Ocaml errors and back-traces
          ("[Ff]ile \\\"?\\([^ \\\"]*?\\)\\\"?, line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" 1 2 (3 . 4) 1)
          ;corresponds to FilePos.to_string
          ("[Ff]ile \\\"?\\([^ \\\"]*?\\)\\\"?, line [0-9]+, characters [0-9]+-[0-9]+, (\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\) | [0-9]+-[0-9]+)" 1 (2 . 4) (3 . 5) 1)
          )
        )
  (set (make-local-variable 'compilation-first-column) 1)
  (add-hook 'compilation-filter-hook
            (lambda () (ansi-color-apply-on-region (point-min-marker) (point))))
  )

(defun opa-compile (command &optional ignored)
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (setq-default compilation-directory default-directory)
  (compilation-start command 'opa-compile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Launch/Stop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar opa-mode-map nil "Keymap for opa-mode")

(setq opa-mode-map (make-sparse-keymap))
(define-key opa-mode-map "\C-c\C-c" 'opa-compile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu

(define-key opa-mode-map [menu-bar]
    (make-sparse-keymap))
  (let ((opa-menu-map (make-sparse-keymap "OPA")))
    (define-key opa-mode-map [menu-bar opa]
      (cons "OPA" opa-menu-map))
    (define-key opa-menu-map [opa-kill]
      '("Stop application" . opa-stop))
    (define-key opa-menu-map [opa-start]
      '("Launch application" . opa-start))
    (define-key opa-menu-map [opa-compile]
      '("Compile this file" . compile))
    (define-key opa-menu-map [separator]
      '("--" ))
    (define-key opa-menu-map [goto-home-page]
      '("Open documentation" .
        (lambda ()
          (interactive)
          (browse-url "http://www.mlstate.com/wiki/admin/opa_documentation"))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; below is currently unused, work-in-progress only


(defvar opa-syntax-error nil)

(defun opa-set-syntax-error-face-once ()
  (if (null opa-syntax-error) ()
    (put-text-property (car opa-syntax-error) (cdr opa-syntax-error) 'face 'opa-font-stop-face)
    (setq opa-syntax-error nil)
    ))

(defun opa-indent-line (&optional arg)
  "Fix the indentation of the current line according to OPA rules.

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
         (syntax-error nil)
         (need (opa-compute-indentation)))
    (if (or (/= ci need) syntax-error)
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to need)

          (setq opa-syntax-error (if syntax-error (cons (- (point) need) (point)) nil))
          ))
    ))

(defun opa-backward-pattern ()
  ; TODO: handle comma for "x, y -> ..."
  (backward-sexp))

(defun backward-if-non-meaningful ()
    (while (looking-at "[ \t\n]") (backward-char))
    (while (is-in-comment-zone) (backward-char))
    (while (looking-at "[ \t\n]") (backward-char)))

(defun backward-until-outdent ()
  (let ((indent (current-indentation)))
    (while (progn (forward-line -1) (>= (current-indentation) indent)))
    (move-to-column indent)))

(defun previous-meaningful-end-of-line ()
  (beginning-of-line) (backward-char) (backward-if-non-meaningful))

(defun is-value ()
  (or (looking-at "[a-zA-Z0-9_)}]\\|\\]")
      (is-in-xhtml-zone)))

(defun count-values (beg end)
  (let ((cnt 0))
    (save-excursion
      (goto-char end) (end-of-line) (backward-if-non-meaningful)
      (if (< end (point)) (previous-meaningful-end-of-line))

      (while (<= beg (point))
        ;(message (concat "count-values looking at " (int-to-string (point))))
        (if (is-value) (inc cnt))
        (previous-meaningful-end-of-line)
        ;(message (concat "count-values is now at " (int-to-string (point))))
      ))
    ;(message (concat "count-values gives " (int-to-string cnt) " between " (int-to-string (point)) " and " (int-to-string last)))
    cnt))

(defconst opa-statement-re
  "\\([;=(){}]\\|\\[\\|\\]\\|->\\|\\bmatch\\b\\)")

(defun opa-classify-statement ()
  (cond
   ((looking-at ";") 'semi-colon)
   ((looking-at "=") 'def)
   ((looking-at "->") 'ano-func)
   ((looking-at "[)}]\\|\\]") 'close-paren)
   ((looking-at "[({]\\|\\[") 'open-paren)
   ((looking-at "match\\b") 'match)
   ((looking-at "end\\b") 'end)
   ((looking-at "[a-zA-Z0-9_]") 'value)
   ))

(defadvice opa-classify-statement (around opa-classify-statement-around)
  (let ((kind (ad-do-it)))
    ;(message (concat "opa-classify-statement at pos " (int-to-string (point)) " gives " (pp-to-string kind)))
    kind))

(defun opa-find-first-statement-forward (beg end)
  ;(message (concat "opa-find-first-statement-forward between " (int-to-string beg) " and " (int-to-string end)))
  (save-excursion
    (goto-char beg)
    (let ((stop nil))
      (while (and (null stop)
                  (re-search-forward opa-statement-re end t))
        (goto-char (match-beginning 0))
        (cond
         ((looking-at "[({]\\|\\[")
          ; using forward-sexp to handle (...), [...], {...} (we must not look into them)
          (forward-sexp))

         (t
          (setq stop (opa-classify-statement)
          ))))
      ;(message (concat "opa-find-first-statement-forward found " (pp-to-string stop)))
      stop)))

(defun opa-find-statement-backward ()
  ;(message (concat "we found an implicit ';' (was looking at " (int-to-string (point)) ")"))
  (let ((last (point))
        (deep 0)
        (stop nil))
    (while (and (null stop)
                (re-search-backward opa-statement-re nil t))
      (setq deep (+ deep (count-values (point) last)))
      (setq last (point))
      (cond
       ((looking-at "[)}]\\|\\]")
        ; using backward-sexp to handle (...), [...], {...} (we must not look into them)
        (forward-char 1) (backward-sexp))

       ((looking-at "[({]\\|\\[")
        ; we found unclosed, we can't go further. If deep is not 0, it mean syntax error in the code
        (setq stop 'open-paren) (forward-char 1))

       ((looking-at ";")
        (inc deep))

       (t
        (dec deep)
        (if (> deep 0) ()
          (setq stop (opa-classify-statement))
          ))))

    ;(message (concat "opa-find-statement-backward stopped at " (int-to-string (point)) " (deep is " (int-to-string deep) ")"))

    stop
    ))

;(toggle-debug-on-error)


(defun opa-compute-indentation-from-here (current-indentation-pos current-eol current-kind previous-kind)
  (cond ((or (eq previous-kind 'operator) (eq previous-kind 'match))
         (opa-backward-pattern) (+ (current-indentation) opa-indent-offset))
        ((eq previous-kind 'def)
         (opa-backward-pattern) (current-column))

        ((eq previous-kind 'ano-func)
         (opa-backward-pattern)
         (let ((current-statement-kind (opa-find-first-statement-forward current-indentation-pos current-eol)))
           (if (eq current-statement-kind 'def)
               (progn
                 ;(message (concat "point is " (int-to-string (point))))
                 (backward-until-outdent)
                 ;(message (concat "point is " (int-to-string (point))))
                 (end-of-line)
                 (opa-compute-indentation-from-here current-indentation-pos current-eol current-kind (opa-find-statement-backward))
                 )
             (current-column))))

        ((eq previous-kind 'open-paren)
         (if (or (eq current-kind 'value) (eq current-kind 'match) (eq current-kind 'open-paren))
             (setq syntax-error t))
         (current-column))
        ))

(defun opa-compute-indentation ()
  ;; implements all the rules for indentation computation.
  (save-excursion

    (let* ((current-indentation-pos (progn (back-to-indentation) (point)))
           (current-kind (opa-classify-statement))
           (current-eol (progn (end-of-line) (point)))
           (previous-kind (progn
               (previous-meaningful-end-of-line)
               (if (is-value)
                  ; we found an implicit ";"
                  (progn (forward-char) (opa-find-statement-backward))

                 ;(message (concat "we found an operator, simply indent (was looking at " (int-to-string (point)) ")"))
                 'operator))))

      (opa-compute-indentation-from-here current-indentation-pos current-eol current-kind previous-kind)
      )))


(defun opa-indent-region (start end &optional indent-offset)
  "Reindent a region of OPA code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `opa-indent-offset' will be
used.
"
  (interactive "*r\nP")			; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((opa-indent-offset (prefix-numeric-value
                             (or indent-offset opa-indent-offset)))
          (indents '(-1))		; stack of active indent levels
          (target-column 0)		; column to which to indent
          (base-shifted-by 0)		; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (opa-compute-indentation)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (eq (following-char) ?#)	; comment in column 1
              (looking-at "[ \t]*$"))	; entirely blank
          (setq target-column 0))
         (t				; new base line
          (if (> ci (car indents))	; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* opa-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))


(provide 'opa-mode)
;;; opa-mode.el ends here
