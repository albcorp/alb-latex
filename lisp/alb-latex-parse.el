;;;
;;; AlbLaTeX/lisp/alb-latex-parse.el
;;;
;;;     Copyright (C) 2000-2005, 2013 Andrew Lincoln Burrow
;;;
;;;     This library is free software; you can redistribute it and/or
;;;     modify it under the terms of the GNU General Public License as
;;;     published by the Free Software Foundation; either version 2 of
;;;     the License, or (at your option) any later version.
;;;
;;;     This library is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;;
;;;     You should have received a copy of the GNU General Public
;;;     License along with this library; if not, write to the Free
;;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;;     MA 02111-1307, USA.
;;;
;;;   - This recursive descent parser lacks a lexical analyzer --- it
;;;     relies on the value of point to record the state of the parser,
;;;     and uses regular expressions to recognise tokens.  Therefore,
;;;     lookahead is always available via the `looking-at' function, and
;;;     tokens are consumed by advancing point according to the match
;;;     data.
;;;
;;;     Since lexical analysis relies on regular expressions the parser
;;;     must be operating with `case-fold-search' set to nil.  This is
;;;     explicitly handled in `alb-LaTeX-parse-document-environment' and
;;;     `alb-LaTeX-parse-inclusion'.  Other file inclusion parsers must
;;;     follow this model, and clients using other entry points should
;;;     bind `case-fold-search' explicitly before calling the parser.
;;;
;;;   - Regexps and functions to parse LaTeX source in an emacs buffer.
;;;
;;;   - The parser recognizes a lexically restricted version of LaTeX,
;;;     intended to correspond to good practice.  It is sensitive to the
;;;     subtleties of whitespace in LaTeX, but cannot parse unbalanced
;;;     environments as occur in the newenvironment command.  Therefore,
;;;     it is best restricted to the document environment.
;;;
;;;   - The parser has two forms: functions named `alb-LaTeX-parse-*'
;;;     parse LaTeX source and construct parse trees as nested
;;;     association lists; and functions named `alb-LaTeX-collect-*'
;;;     recognize balanced LaTeX source units and pass parse trees
;;;     upwards.  Both forms return the first and last position matched.
;;;
;;;     The parse and collect forms are related by a context argument,
;;;     which is used to associate commands and environments with
;;;     specialised parse functions.  Thus, the basic parser can be
;;;     extended to parse particular commands or environments of
;;;     interest.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-latex-parse)



;;; *** REQUIRED FEATURES *****************************************************


(require 'tex-site)
(require 'reftex)



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-parser-context-standard
  '((commands . (("include" . alb-LaTeX-parse-inclusion)
                 ("input" . alb-LaTeX-parse-inclusion)
                 ("verb" . alb-LaTeX-collect-verb)
                 ("verb*" . alb-LaTeX-collect-verb)))
    (environs . (("verbatim" . alb-LaTeX-collect-verbatim)
                 ("verbatim*" . alb-LaTeX-collect-verbatim))))
  "Standard LaTeX context to handle file input and verbatim text.  Most
collector contexts should extend this standard context.

For a more detailed description of the context form see:
`alb-LaTeX-collect-balanced-braces'.

NB: This value should only associate 'commands and 'environs.")


;; Whitespace Regular Expressions.
;;
;; Match various types of whitespace in LaTeX source.

(defconst alb-LaTeX-re-whitespace-comment
  "%[^\n]*\n"
  "Regexp matching comment to end of line.  Note that TeX also removes
the following whitespace, hence: 'alb-LaTeX-re-whitespace-hidden'.

NB: The parser expects to consume 0 subexpressions!")


(defconst alb-LaTeX-re-whitespace-hidden
  (concat "\\(" alb-LaTeX-re-whitespace-comment "\\s-*\\)*")
  "Regexp matching hidden white space in LaTeX source, i.e., whitespace
not seen by LaTeX.

NB: The parser expects to consume 1 subexpression!")


(defconst alb-LaTeX-re-whitespace-removed
  (concat "\\(" alb-LaTeX-re-whitespace-comment "\\s-*\\)+")
  "Regexp matching hidden white space in LaTeX source that is actually
removed, i.e., whitespace not seen by LaTeX that actually exists in the
source code.  This pattern exists to allow functions to strip such
whitespace.

NB: The parser expects to consume 1 subexpression!")


(defconst alb-LaTeX-re-whitespace-horizontal
  (concat "\\s-*\\(\n\\s-*\\|" alb-LaTeX-re-whitespace-hidden "\\)?")
  "Regexp matching hidden and horizontal white space in LaTeX source,
i.e., whitespace seen as a single unit or less of inter-word whitespace.

NB: The parser expects to consume 2 subexpressions!")


;; Typeset Matter Regular Expressions.
;;
;; Match various types of non-whitespace matter in LaTeX source.

(defconst alb-LaTeX-re-matter-inside-structure
  (concat "\\([^{}\\%$]*" alb-LaTeX-re-whitespace-hidden "\\)*")
  "Regexp matching material other than commands or structures, i.e.,
everything that does not require attention while balancing braces, and
environments in LaTeX.

NB: The parser expects to consume 2 subexpressions!")


(defconst alb-LaTeX-re-matter-inside-brackets
  (concat "\\([^]{}\\%$]*" alb-LaTeX-re-whitespace-hidden "\\)*")
  "Regexp matching material other than commands, brackets, or
structures, i.e., everything that does not require attention while
balancing brackets in LaTeX.

NB: The parser expects to consume 2 subexpressions!")


;; Argument Type Regular Expressions.
;;
;; Match various stereotypical argument types in LaTeX source, and
;; enforce certain lexical conventions.

(defconst alb-LaTeX-re-type-identifier
  "\\([A-Za-z@]+[*]?\\)"
  "Regexp matching an identifier in LaTeX source.

NB: The parser expects to consume 1 subexpression, and for the
    identifier to be marked as a subexpression!")


(defconst alb-LaTeX-re-type-filename
  "\\([A-Za-z][0-9A-Za-z_+-]*\\)"
  "Regexp matching a filename specification in LaTeX source.  Filenames
are expected to be entered as basenames, free of both directories and
extensions.  They are also very lexically restricted to prevent strange
interactions with LaTeX in case of errors.

NB: The parser expects to consume 1 subexpression, and for the filename
    to be marked as a subexpression!")


;; Command Argument Seeking Regular Expressions.
;;
;; Match preceeding whitespace and the opening brace or bracket for
;; LaTeX command arguments.

(defconst alb-LaTeX-re-next-opt
  (concat alb-LaTeX-re-whitespace-horizontal "\\[")
  "Regexp matching the start of the next LaTeX command option.

NB: The last character matched is the opening bracket.")


(defconst alb-LaTeX-re-next-arg
  (concat alb-LaTeX-re-whitespace-horizontal "{")
  "Regexp matching the start of the next LaTeX command argument.

NB: The last character matched is the opening brace.")


;; Command Argument Regular Expressions.
;;
;; Match various stereotypical arguments in LaTeX source, and enforce
;; certain lexical conventions.

(defconst alb-LaTeX-re-arg-filename
  (concat "{" alb-LaTeX-re-whitespace-hidden
          alb-LaTeX-re-type-filename alb-LaTeX-re-whitespace-hidden "}" )
  "Regexp matching a LaTeX command argument containing a filename.  Note
that only hidden whitespace is allowed.

NB: The parser expects the filename to be the 2nd subexpression!")


;; Paired Symbols.
;;
;; Match the various paired symbols.  There is a regexp for each opening
;; and closing symbol.

(defconst alb-LaTeX-re-opening-brace
  "{"
  "Regexp matching the lefthand curly brace.")


(defconst alb-LaTeX-re-opening-math
  "\\(\\$\\|\\\\(\\|\\\\\\[\\)"
  "Regexp matching any of the commands that open math mode.")


(defconst alb-LaTeX-re-opening-document
  (concat "\\\\begin{document}")
  "Regexp matching the begin command marking the end of the preamble.")


(defconst alb-LaTeX-re-opening-environment
  (concat "\\\\begin{\\(" alb-LaTeX-re-type-identifier "\\)}")
  "Regexp matching a begin command, including the environment name but
not additional arguments.

NB: The parser expects the identifier to be the 1st subexpression!")


(defconst alb-LaTeX-re-opening-command
  (concat "\\\\\\("
          "\\\\[*]?"
          "\\|" "[ !\"#$%&'+,./:;<=>@^_`{}|~-]"
          "\\|" alb-LaTeX-re-type-identifier
          "\\)")
  "Regexp matching a command, but not its arguments.

NB: The parser expects the command name to be the 1st subexpression!")


(defconst alb-LaTeX-re-opening-bracket
  "\\["
  "Regexp matching the lefthand square bracket.")


(defconst alb-LaTeX-re-closing-brace
  "}"
  "Regexp matching the righthand curly brace.")


(defconst alb-LaTeX-re-closing-math
  "\\(\\$\\|\\\\)\\|\\\\\\]\\)"
  "Regexp matching any of the commands that close math mode.")


(defconst alb-LaTeX-re-closing-document
  (concat "\\\\end{document}")
  "Regexp matching the end command marking the end of the document.")


(defconst alb-LaTeX-re-closing-environment
  (concat "\\\\end{\\(" alb-LaTeX-re-type-identifier "\\)}")
  "Regexp matching an end command, including the environment name.

NB: The parser expects the identifier to be the 1st subexpression!")


(defconst alb-LaTeX-re-closing-bracket
  "\\]"
  "Regexp matching the righthand square bracket.")


(defconst alb-LaTeX-re-opening-command-item
  "\\\\item"
  "Regexp matching an item command, but not its optional argument.

NB: The parser expects to consume 0 subexpressions!")


(defconst alb-LaTeX-re-opening-command-end-of-line
  "\\\\\\\\[*]?"
  "Regexp matching an end of line command, but not its optional argument.

NB: The parser expects to consume 0 subexpressions!")


(defconst alb-LaTeX-re-opening-command-spacing
  (concat "\\\\\\("
          ",\\|thinspace\\|:\\|medspace\\|;\\|thickspace\\|quad\\|qquad"
          "\\|!\\|negthinspace\\|negmedspace\\|negthickspace"
          "\\|enspace"
          "\\)")
  "Regexp matching a spacing adjustment command without arguments.

NB: The parser expects to consume 1 subexpression!")


(defconst alb-LaTeX-re-opening-command-bracing
  (concat "\\\\\\("
          "left\\|right"
          "\\)")
  "Regexp matching a brace sizing command without arguments.

NB: The parser expects to consume 1 subexpression!")


(defconst alb-LaTeX-re-command-math-layout
  (concat "\\\\\\("
          "\\(begin\\|end\\){split}"
          "\\|begin{\\(aligned\\|gathered\\)}\\(\\[[tb]\\]\\)?"
          "\\|end{\\(aligned\\|gathered\\)}"
          "\\)"
          "\\|" alb-LaTeX-re-opening-command-end-of-line "\\|&")
  "Regexp matching commands including opening and closing of
environments where the command or environment affects only the layout of
math.")


(defconst alb-LaTeX-re-command-crossref
  (concat "\\\\\\("
          "label\\|index\\|glossary"
          "\\)")
  "Regexp matching commands for inserting cross references that do not
form part of the text.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-collect-balanced-braces (context)
  "Collect a balanced pair of braces in LaTeX source.

Point is advanced from the opening brace to the point immediately after
the closing brace.  The result is the collected list of parse results.
Each collector function appends these lists together.

CONTEXT is a two tiered alist controlling the behaviour of the collector
functions.  The value of CONTEXT maps LaTeX commands and environments to
specialised functions which parse these structures.  If a command is not
in CONTEXT, it is skipped and its arguments collected as balanced
braces/brackets.  If an environments is not in CONTEXT, the begin
command is treated as a command, and the environment is collected as a
balanced begin/end pair.

CONTEXT maps the symbols 'commands and 'environs into alists for
commands and environments.  These alists map identifiers to functions
taking the same arguments as this function, and returning results of the
same form.  See the contents of `alb-LaTeX-parser-context-standard' for
a simple example.  The recursive descent parser can pass symbols
downwards via CONTEXT to effect scoped modalities, and is free to
introduce other top level associations to CONTEXT.

CONTEXT determines the structure of the parser ouput --- the
`alb-LaTeX-collect-*' functions simply append the results of their
subparts.  Therefore, the parser output must be interpretted wrt the
documentation strings for parser functions in CONTEXT.

It is an error to call this function when point is not at a left brace."
  (if (looking-at alb-LaTeX-re-opening-brace)
      (let ((begin (match-beginning 0))
            (results nil))
        ;; Step over the opening brace.
        (goto-char (match-end 0))

        ;; Iterate until the closing brace is next.
        (while (and (looking-at alb-LaTeX-re-matter-inside-structure)
                    (goto-char (match-end 0))
                    (not (looking-at alb-LaTeX-re-closing-brace)))
          (cond
           ((looking-at alb-LaTeX-re-opening-brace)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-braces context))))
           ((looking-at alb-LaTeX-re-opening-math)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-math context))))
           ((looking-at alb-LaTeX-re-opening-environment)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-environment context))))
           ((looking-at alb-LaTeX-re-opening-command)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-command context))))
           (t
            (error "%s: %d: %d: Unbalanced brace"
                   (buffer-name) begin (point)))))

        ;; Consume the closing brace.
        (looking-at alb-LaTeX-re-closing-brace)
        (goto-char (match-end 0))

        ;; Return the accumulated results.
        results)
    (error "%s: %d: Not at brace" (buffer-name) (point))))



(defun alb-LaTeX-collect-balanced-brackets (context)
  "Collect a balanced pair of brackets in LaTeX source.

Point is advanced from the opening bracket to the point immediately
after the closing bracket.  See `alb-LaTeX-collect-balanced-braces' for
a description of the return value and argument.

It is an error to call this function when point is not at a left
bracket."
  (if (looking-at alb-LaTeX-re-opening-bracket)
      (let ((begin (match-beginning 0))
            (results nil))
        ;; Step over the opening brace.
        (goto-char (match-end 0))

        ;; Iterate until the closing bracket is next.
        (while (and (looking-at alb-LaTeX-re-matter-inside-brackets)
                    (goto-char (match-end 0))
                    (not (looking-at alb-LaTeX-re-closing-bracket)))
          (cond
           ((looking-at alb-LaTeX-re-opening-brace)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-braces context))))
           ((looking-at alb-LaTeX-re-opening-math)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-math context))))
           ((looking-at alb-LaTeX-re-opening-environment)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-environment context))))
           ((looking-at alb-LaTeX-re-opening-command)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-command context))))
           (t
            (error "%s: %d: %d: Unbalanced bracket"
                   (buffer-name) begin (point)))))

        ;; Consume the closing bracket.
        (looking-at alb-LaTeX-re-closing-bracket)
        (goto-char (match-end 0))

        ;; Return the accumulated results.
        results)
    (error "%s: %d: Not at bracket" (buffer-name) (point))))



(defun alb-LaTeX-collect-balanced-math (context)
  "Collect a balanced pair of symbols marking a math environment in
LaTeX source.

Point is advanced from the opening symbol to the point immediately after
the closing symbol.  See `alb-LaTeX-collect-balanced-braces' for
a description of the return value and argument.

It is an error to call this function when point is not at a math opening
symbol."
  (if (looking-at alb-LaTeX-re-opening-math)
      (let ((begin (match-beginning 0))
            (results nil))
        ;; Step over the opening brace.
        (goto-char (match-end 0))

        ;; Iterate until the closing math command is next.
        (while (and (looking-at alb-LaTeX-re-matter-inside-structure)
                    (goto-char (match-end 0))
                    (not (looking-at alb-LaTeX-re-closing-math)))
          (cond
           ((looking-at alb-LaTeX-re-opening-brace)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-braces context))))
           ((looking-at alb-LaTeX-re-opening-environment)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-balanced-environment context))))
           ((looking-at alb-LaTeX-re-opening-command)
            (setq results
                  (nconc results
                         (alb-LaTeX-collect-command context))))
           (t
            (error "%s: %d: %d: Unbalanced math mode"
                   (buffer-name) begin (point)))))

        ;; Ensure the math closing command matches the opening, then consume.
        (looking-at alb-LaTeX-re-closing-math)
        (if (or (and (char-equal ?$ (char-after begin))
                     (char-equal ?$ (char-after (point))))
                (and (char-equal ?\[ (char-after (+ begin 1)))
                     (char-equal ?\] (char-after (+ (point) 1))))
                (and (char-equal ?\( (char-after (+ begin 1)))
                     (char-equal ?\) (char-after (+ (point) 1)))))
            (goto-char (match-end 0))
          (error "%s: %d: %d: Unbalanced math mode"
                 (buffer-name) begin (point)))

        ;; Return the accumulated results.
        results)
    (error "%s: %d: Not at math mode opening" (buffer-name) (point))))



(defun alb-LaTeX-collect-balanced-environment (context)
  "Collect a balanced environment in LaTeX source.

Point is advanced from the opening begin command to the point
immediately after the closing end command.  See
`alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument.

It is an error to call this function when point is not at a begin
command."
  (if (looking-at alb-LaTeX-re-opening-environment)
      (let* ((begin (match-beginning 0))
             (start (match-end 0))
             (ename (match-string-no-properties 1))
             (envfn (cdr (assoc ename (cdr (assq 'environs context))))))
        (if envfn
            (progn (goto-char begin)
                   (apply envfn (list context)))
          (progn (goto-char start)
                 (let (results)
                   ;; Iterate until the closing environment command is next.
                   (while (and (looking-at
                                alb-LaTeX-re-matter-inside-structure)
                               (goto-char (match-end 0))
                               (not
                                (looking-at alb-LaTeX-re-closing-environment)))
                     (cond
                      ((looking-at alb-LaTeX-re-opening-brace)
                       (setq results
                             (nconc results
                                    (alb-LaTeX-collect-balanced-braces
                                     context))))
                      ((looking-at alb-LaTeX-re-opening-math)
                       (setq results
                             (nconc results
                                    (alb-LaTeX-collect-balanced-math
                                     context))))
                      ((looking-at alb-LaTeX-re-opening-environment)
                       (setq results
                             (nconc results
                                    (alb-LaTeX-collect-balanced-environment
                                     context))))
                      ((looking-at alb-LaTeX-re-opening-command)
                       (setq results
                             (nconc results
                                    (alb-LaTeX-collect-command context))))
                      (t
                       (error "%s: %d: %d: Unbalanced environment"
                              (buffer-name) begin (point)))))

                   ;; Ensure the \end command matches the environment,
                   ;; then consume.
                   (looking-at alb-LaTeX-re-closing-environment)
                   (if (string-equal ename (match-string-no-properties 1))
                       (progn (goto-char (match-end 0))
                              results)
                     (error "%s: %d: %d: Unbalanced environment"
                            (buffer-name) begin (point)))))))
    (error "%s: %d: Not at environment opening" (buffer-name) (point))))



(defun alb-LaTeX-collect-command (context)
  "Collect a command and its arguments in LaTeX source.

Point is advanced from the initial backslash to the point immediately
after the last argument.  See `alb-LaTeX-collect-balanced-braces' for a
description of the return value and argument.

It is an error to call this function when point is not at a command."
  (if (looking-at alb-LaTeX-re-opening-command)
      (let* ((begin (match-beginning 0))
             (cname (match-string-no-properties 1))
             (cmdfn (cdr (assoc cname (cdr (assq 'commands context))))))
        (if cmdfn
            (progn (goto-char begin)
                   (apply cmdfn (list context)))
          (progn (goto-char (match-end 0))
                 (let (results)
                   ;; Iterate until there are no further arguments.
                   (while (cond
                           ((looking-at alb-LaTeX-re-next-opt)
                            (goto-char (- (match-end 0) 1))
                            (setq results
                                  (nconc results
                                         (alb-LaTeX-collect-balanced-brackets
                                          context)))
                            t)
                           ((looking-at alb-LaTeX-re-next-arg)
                            (goto-char (- (match-end 0) 1))
                            (setq results
                                  (nconc results
                                         (alb-LaTeX-collect-balanced-braces
                                          context)))
                            t)))

                   ;; Return the accumulated results.
                   results))))
    (error "%s: %d: Not at command" (buffer-name) (point))))



(defun alb-LaTeX-collect-verb (context)
  "Collect the verb command and its specially delimited argument.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument.

It is an error to call this function when point is not at a command."
  (if (looking-at alb-LaTeX-re-opening-command)
      (let* ((begin (match-beginning 0))
             (start (+ (match-end 0) 1))
             (quote (buffer-substring-no-properties (- start 1) start)))
        (goto-char start)
        (if (re-search-forward (regexp-quote quote) (line-end-position) t)
            '()
          (error "%s: %d: Unbalanced verb command" (buffer-name) begin)))
    (error "%s: %d: Not at verb command" (buffer-name) (point))))



(defun alb-LaTeX-collect-verbatim (context)
  "Collect the verbatim environment.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (if (looking-at alb-LaTeX-re-opening-environment)
      (let* ((begin (match-beginning 0))
             (start (match-end 0))
             (quote (concat "\\end{" (match-string-no-properties 1) "}")))
        (goto-char start)
        (if (re-search-forward (regexp-quote quote) nil t)
            '()
          (error "%s: %d: Unbalanced verbatim environment"
                 (buffer-name) begin)))
    (error "%s: %d: Not at verbatim environment opening"
           (buffer-name) (point))))



(defun alb-LaTeX-collect-file (context)
  "Collect the remainder of the file.

Point is advanced to the last point in the file.  See
`alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (let (results)
    ;; Iterate until the end of file is met.
    (while (and (looking-at alb-LaTeX-re-matter-inside-structure)
                (goto-char (match-end 0))
                (not (equal (point) (point-max))))
      (cond
       ((looking-at alb-LaTeX-re-opening-brace)
        (setq results
              (nconc results
                     (alb-LaTeX-collect-balanced-braces context))))
       ((looking-at alb-LaTeX-re-opening-math)
        (setq results
              (nconc results
                     (alb-LaTeX-collect-balanced-math context))))
       ((looking-at alb-LaTeX-re-opening-environment)
        (setq results
              (nconc results
                     (alb-LaTeX-collect-balanced-environment context))))
       ((looking-at alb-LaTeX-re-opening-command)
        (setq results
              (nconc results
                     (alb-LaTeX-collect-command context))))
       (t
        (error "%s: %d: Confused by source" (buffer-name) (point)))))

    ;; Return the accumulated results.
    results))



(defun alb-LaTeX-parse-inclusion (context)
  "Parse the file inclusion command, and collect the contents of the
included file.

Point is advanced from the initial backslash to the point immediately
after the first argument.  The excursion is saved, so that the current
buffer and point in the included file are restored.  See
`alb-LaTeX-collect-balanced-braces' for a description of the argument.

The result is returned as a singleton alist RESULT associating the name
of this function with an alist recording the parse.  Within RESULT:
  (symbol-name 'alb-LaTeX-parse-inclusion) `assq's to the parse alist.
Within the parse alist:
  \"command\" `assoc's to the actual command name;
  \"at\" `assoc's to the position of the opening backslash;
  \"enter\" `assoc's to the expanded filename of the inclusion; and
  \"result\" `assoc's to the result of collecting the file.

It is an error to call this function when point is not at a command
followed by a file argument."
  (if (looking-at alb-LaTeX-re-opening-command)
      (let* ((begin (match-beginning 0))
             (cname (match-string-no-properties 1)))
        (if (and (goto-char (match-end 0))
                 (looking-at alb-LaTeX-re-next-arg)
                 (goto-char (- (match-end 0) 1))
                 (looking-at alb-LaTeX-re-arg-filename))
            (let* ((after (match-end 0))
                   (fname (reftex-locate-file
                           (match-string-no-properties 2)
                           TeX-default-extension
                           (TeX-master-directory) t))
                   (fbuff (find-file-noselect fname)))
              (goto-char after)
              (save-excursion
                (set-buffer fbuff)
                (let ((case-fold-search nil))
                  (save-excursion
                    (goto-char (point-min))
                    (list (cons (symbol-name 'alb-LaTeX-parse-inclusion)
                                (list (cons "command" cname)
                                      (cons "at" begin)
                                      (cons "enter" fname)
                                      (cons "result"
                                            (alb-LaTeX-collect-file
                                             context)))))))))
          (error "%s: %d: Missing inclusion argument" (buffer-name) (point))))
    (error "%s: %d: Not at inclusion command" (buffer-name) (point))))



(defun alb-LaTeX-parse-document-environment (context)
  "Parse the document environment in the TeX master file, and collect
the contents of the document environment.

The excursion is saved, so that the current buffer and point in the
master file are restored.  See `alb-LaTeX-collect-balanced-braces' for a
description of the argument.

The result is returned as a singleton alist RESULT associating the name
of this function with an alist recording the parse.  Within RESULT:
  (symbol-name 'alb-LaTeX-parse-document-environment) `assq's to the
  parse alist.
Within the parse alist:
  \"master\" `assoc's to the expanded filename of the master;
  \"at\" `assoc's to the position of the environment's opening
  backslash; and
  \"result\" `assoc's to the results of collecting the environment.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (let* ((mname (file-truename (TeX-master-file TeX-default-extension)))
         (mbuff (find-file-noselect mname)))
    (save-excursion
      (set-buffer mbuff)
      (let ((case-fold-search nil))
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward alb-LaTeX-re-opening-document nil t)
              (let* ((begin (match-beginning 0)))
                (goto-char (match-beginning 0))
                (list (cons (symbol-name 'alb-LaTeX-parse-document-environment)
                            (list (cons "master" mname)
                                  (cons "at" begin)
                                  (cons "result"
                                        (alb-LaTeX-collect-balanced-environment
                                         context))))))
            (error "%s: No document environment in master file" mname)))))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
