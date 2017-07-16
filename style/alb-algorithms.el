;;;
;;; alb-latex/style/alb-algorithms.el
;;;
;;;     Copyright (C) 1999-2006, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Customise AUCTeX and RefTeX for "alb-algorithms" LaTeX package.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-filenames)
(require 'alb-local-variables)
(require 'alb-latex-parse)



;;; *** VARIABLE DECLARATIONS *************************************************


(defconst alb-TeX-re-newIdent-list
  "\\\\albNew\\(Accessor\\|Procedure\\)Ident"
  "Regexp matching macros introducing new identifiers with the same
syntax as \\albNewAccessorIdent.

NB: `alb-TeX-re-newIdent' is designed under the premise that this
consumes 1 subexpression!")


(defconst alb-TeX-re-newIdent
  (list (concat                 ; Sub expressions:      Count   Total
         alb-TeX-re-newIdent-list                       ;  1     1
         alb-LaTeX-re-whitespace-horizontal             ;  2     3
         "{" alb-LaTeX-re-whitespace-hidden             ;  1     4
         "\\\\\\([a-zA-Z]+\\)"                          ;  1     5
         alb-LaTeX-re-whitespace-hidden "}"             ;  1     6
         alb-LaTeX-re-whitespace-horizontal             ;  2     8
         "{" alb-LaTeX-re-whitespace-hidden             ;  1     9
         "\\\\\\([a-zA-Z]+\\)"                          ;  1    10
         alb-LaTeX-re-whitespace-hidden "}"             ;  1    11
         alb-LaTeX-re-whitespace-horizontal             ;  2    13
         "{" alb-LaTeX-re-whitespace-hidden             ;  1    14
         "\\([a-zA-Z-]\\|\\\\_\\)+"                     ;  1    15
         alb-LaTeX-re-whitespace-hidden "}"             ;  1    16
         alb-LaTeX-re-whitespace-horizontal             ;  2    18
         "{" alb-LaTeX-re-whitespace-hidden             ;  1    19
         "\\([0-9]+\\)"                                 ;  1    20
         alb-LaTeX-re-whitespace-hidden "}")            ;  1    21
        (list 5 10 20)
        'alb-TeX-auto-newIdent-temp)
  "Regular expression to extract macros defined by \\albNewAccessorIdent
and \\albNewProcedureIdent commands.  It is appended to
`TeX-auto-regexp-list' during customisation.  The list of numbers refer
to the subexpressions --- see `alb-TeX-auto-newIdent-cleanup'.

This variable is used in AUCTeX file parsing.")


(defvar alb-TeX-auto-newIdent-temp
  nil
  "Temporary for macro information extracted from \\albNewAccessorIdent
and \\albNewProcedureIdent commands.  It is reset by
`alb-TeX-auto-newIdent-prepare' prior to parsing the buffer, and
processed by `alb-TeX-auto-newIdent-cleanup' after parsing the buffer.

This variable is used in AUCTeX file parsing.")


(defconst alb-LaTeX-re-accessor-call
  (concat "\\\\acc\\("
          "[A-Z]\\|"
          "[A-Z][A-Za-z]\\|"
          "[A-Z][A-Za-z]*\\([A-HJ-Za-z][A-Za-z]\\|I[A-Za-ce-z]\\)"
          "\\)\\>")
  "Regexp matching typeset procedure calls in LaTeX source.  The LaTeX
commands are recognised by the lexical conventions resulting from
`alb-TeX-arg-define-ident' with OP-PREFIX \"acc\".  The complexity
arises from having to avoid commands with an \"Id\" suffix.

NB: Standardised to match command sans arguments, and command name sans
    prefix by the 1st subexpression.")


(defconst alb-LaTeX-re-procedure-call
  (concat "\\\\prc\\("
          "[A-Z]\\|"
          "[A-Z][A-Za-z]\\|"
          "[A-Z][A-Za-z]*\\([A-HJ-Za-z][A-Za-z]\\|I[A-Za-ce-z]\\)"
          "\\)\\>")
  "Regexp matching typeset procedure calls in LaTeX source.  The LaTeX
commands are recognised by the lexical conventions resulting from
`alb-TeX-arg-define-ident' with OP-PREFIX \"prc\".  The complexity
arises from having to avoid commands with an \"Id\" suffix.

NB: Standardised to match command sans arguments, and command-name sans
    prefix by the 1st subexpression.")


(defconst alb-LaTeX-re-opening-command-algorithms
  (concat "\\\\\\("
          "albLet\\|albAssign\\|albReturn\\|albWhile\\|albForAll"
          "\\|albForSequence\\|albIf\\|albElseIf\\|\\albElse"
          "\\)")
  "Regexp matching a command that formats a statement in the
\"albAlgorithmic\" environment, but not its arguments.  Namely one of
\\albLet, \\albAssign, \\albReturn, \\albWhile, \\albForAll,
\\albForSequence, \\albIf, \\albElseIf, or \\albElse.

NB: The parser expects the command name to be the 1st subexpression!")


(defvar alb-LaTeX-line-counter
  0
  "Count numbers assigned to labels for \"albAlgorithmic\" environments.
This ensures dirty numbers are not reissued.  Reissuing a number could
make stale references hard to detect.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-TeX-arg-define-ident (optional op-prefix op-type op-mangler
                                          &optional given-phrase)
  "Prompt for and insert arguments to a call to a command to construct
an identifier and call macro for a given type of operation.  Generally
called immediately following an \\albNew*Ident command.

OPTIONAL is ignored (required by AUCTeX).
OP-PREFIX is the prefix used in the LaTeX macros for the class of
operators.  It prevents pollution of the LaTeX name space.
OP-TYPE is the name of the class of operators for use in prompting the
user.
OP-MANGLER is a function accepting a whitespace stripped, and hyphenated
string and returning an appropriate identifier for the class of
operator.
If GIVEN-PHRASE is supplied it is used to construct the identifier using
`alb-hyphenate-string-at-caps-and-gaps' and OP-MANGLER.  Otherwise, the
user is prompted.

This function customises AUCTeX.  It is called immediately after
inserting the macro name.  See documentation for `TeX-add-symbols'."
  (let* ((phrase (if (stringp given-phrase)
                     given-phrase
                   (read-from-minibuffer
                    (format "Phrase describing %s: " op-type))))
         (hyname (alb-hyphenate-string-at-caps-and-gaps phrase))
         (funcn-idnt (apply op-mangler (list hyname)))
         (macro-base (apply 'concat (split-string (capitalize hyname) "-")))
         (macro-idnt (concat op-prefix macro-base "Id"))
         (macro-call (concat op-prefix macro-base)))
    (if (or (assoc macro-idnt TeX-symbol-list)
            (assoc macro-call TeX-symbol-list))
        (error "\\%s or \\%s already exists" macro-idnt macro-call)
      (let ((ident (read-from-minibuffer
                    (format "Identifier for %s: " op-type)
                    funcn-idnt))
            (arity (read-from-minibuffer
                    (format "Arity of %s: " op-type)
                    "")))
        (if (not (wholenump (string-to-number arity)))
            (error "Arity \"%s\" is not a whole number" arity)
          (progn
            (TeX-add-symbols
             (list macro-idnt 0)
             (list macro-call (string-to-number arity)))
            (insert (format "{\\%s}{\\%s}{%s}{%s}"
                            macro-idnt macro-call ident arity))))))))



(defun alb-TeX-arg-compound-statement (optional st-type st-parm)
  "Prompt for and insert arguments to a compound statement in an
\"albAlgorithmic\" environment.

OPTIONAL is ignored (required by AUCTeX).
ST-TYPE is a string containing the type of compound statement.  It is
used to prompt the user.
ST-PARM is a list of strings naming the arguments to the compound
statement.  It is used to prompt the user to build the arguments to the
macro.

This function customises AUCTeX.  It is called immediately after
inserting the macro name.  See documentation for `TeX-add-symbols'."
  (while (car st-parm)
    (insert (concat "{"
                    (read-from-minibuffer
                     (format "%s statement %s: "
                             (capitalize st-type) (car st-parm)))
                    "}"))
    (setq st-parm (cdr st-parm)))
  (LaTeX-env-item "albBlock"))



(defun alb-TeX-auto-newIdent-prepare ()
  "Appended to `TeX-auto-prepare-hook' clears
`alb-TeX-auto-newIdent-temp' to allow definitions to be gathered.

This function customises AUCTeX."
  (setq alb-TeX-auto-newIdent-temp nil))



(defun alb-TeX-auto-newIdent-cleanup ()
  "Appended to `TeX-auto-cleanup-hook' moves symbols from
`alb-TeX-auto-newIdent-temp' to `TeX-auto-symbol'.  Each element in
`alb-TeX-auto-newIdent-temp' must be a triple of strings in the form
SYMBOL-ID, SYMBOL-CALL, NARG.

This function customises AUCTeX."
  (mapcar (function
           (lambda (entry)
             (set 'TeX-auto-symbol
                  (cons (list (nth 1 entry) (string-to-int (nth 2 entry)))
                        (cons (list (nth 0 entry) nil)
                              TeX-auto-symbol)))))
          alb-TeX-auto-newIdent-temp))



(defun alb-LaTeX-env-algorithm (environment &rest junk)
  "Insert an \"algorithm\" float ENVIRONMENT with prompting.  Prompt for
the procedure call macro, abbreviated caption, and caption.  Insert the
procedure call, \"albAlgorithmic\" environment, captions, and generate a
label.

This function customises AUCTeX."
  (let* ((float   (read-from-minibuffer
                   "(optional) Float to: " LaTeX-float))
         (macro   (completing-read
                   "Procedure call macro: \\" (TeX-symbol-list) nil t))
         (abrieve (read-from-minibuffer
                   "(optional) Abbreviated caption: "))
         (caption (read-from-minibuffer
                   "Caption: " abrieve)))
    (LaTeX-insert-environment
     environment
     (if (not (string-equal "" float)) (concat "[" float "]")))
    (insert "$")
    (TeX-insert-macro macro)
    (end-of-line)
    (insert "$")
    (end-of-line)
    (save-excursion
      (newline-and-indent)
      (if (string-equal "" abrieve)
          (progn (insert (concat "\\caption{" caption "}"))
                 (newline-and-indent))
        (progn (insert (concat "\\caption[" abrieve "]{%"))
               (newline-and-indent)
               (insert (concat caption "}"))
               (newline-and-indent)))
      (LaTeX-label environment))
    (LaTeX-env-item "albAlgorithmic")))



(defun alb-reftex-context-fn-algorithm (environment)
  "Determine a suitable context string for the enclosing \"algorithm\"
float environment ENVIRONMENT.  Search forward from the beginning of the
environment for the opening macro, which should be a procedure call
macro defined by \\albNewProcedureIdent.

This function customises RefTeX."
  (save-excursion
    (if (and (search-backward (concat "\\begin{" environment "}") nil t)
             (goto-char (match-end 0))
             (re-search-forward
              alb-LaTeX-re-procedure-call (+ (point) 400) t))
        ;; Environment contains a procedure call.
        (alb-hyphenate-string-at-caps-and-gaps
         (match-string-no-properties 1))
      ;; No procedure call found in the environment.
      "")))



(defun alb-LaTeX-parse-command-algorithms (context)
  "Parse the an \"albAlgorithmic\" statement command.

Collect the arguments of any of the following LaTeX commands:
\\albLet{VAR}{VAL}, \\albAssign{VAR}{VAL}, \\albReturn{VAL},
\\albWhile{COND}, \\albForAll{VAR}{SET},
\\albForSequence{VAR}{SEQUENCE}, \\albIf{COND}, \\albElseIf{COND}, and
\\albElse commands.

See `alb-LaTeX-collect-balanced-braces' for a description of the
argument.

The result is returned as a singleton alist RESULT associating the name
of this function with an alist recording the parse.  Within RESULT:
  (symbol-name 'alb-LaTeX-parse-command-algorithms) `assq's to the
  parse alist.
Within the parse alist:
  \"command\" `assoc's to the LaTeX command name,
  \"arguments\" `assoc's to the list of strings inside the LaTeX
  arguments, and
  \"at\" `assoc's to the position of the command's opening backslash.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (if (looking-at alb-LaTeX-re-opening-command-algorithms)
      (let* ((begin (match-beginning 0))
             (cname (match-string-no-properties 1))
             (arguments))
        ;; Collect list of arguments.
        (goto-char (match-end 1))
        (while (looking-at alb-LaTeX-re-next-arg)
          (let* ((arg-begin (match-beginning 0))
                 (arg-close (progn (alb-LaTeX-collect-balanced-braces context)
                                   (point)))
                 (arg-value (buffer-substring-no-properties
                             (+ arg-begin 1)
                             (- arg-close 1))))
            (setq arguments (append arguments (list arg-value)))))
        ;; Compile description of statement
        (list (cons (symbol-name 'alb-LaTeX-parse-command-algorithms)
                    (list (cons "command" cname)
                          (cons "arguments" arguments)
                          (cons "at" begin)))))
    (error "%s: Not at \"albAlgorithmic\" statement command")))



(defun alb-reftex-context-fn-albAlgorithmic-display (environment)
  "Determine a suitable context string for an item in an
\"albAlgorithmic\" environment, where the string is to be used in the
RefTeX table of contents buffer.

This function customises RefTeX."
  (save-excursion
    (let (display)
      (looking-at alb-LaTeX-re-matter-inside-structure)
      (goto-char (match-end 0))
      (if (looking-at alb-LaTeX-re-opening-command-algorithms)
          ;; Use parser to retrieve arguments to statement command.
          (let* ((parse (alb-LaTeX-parse-command-algorithms
                         alb-LaTeX-parser-context-standard))
                 (tree (assoc (symbol-name 'alb-LaTeX-parse-command-algorithms)
                              parse))
                 (name (cdr (assoc "command" tree)))
                 (args (cdr (assoc "arguments" tree))))
            (setq display
                  (cond
                   ((equal "albLet" name)
                    (concat "LET " (nth 0 args) " <-- " (nth 1 args)))
                   ((equal "albAssign" name)
                    (concat (nth 0 args) " <-- " (nth 1 args)))
                   ((equal "albReturn" name)
                    (concat "RETURN " (nth 0 args)))
                   ((equal "albWhile" name)
                    (concat "WHILE " (nth 0 args)))
                   ((equal "albForAll" name)
                    (concat "FOR ALL " (nth 0 args) " IN " (nth 1 args)))
                   ((equal "albForSequence" name)
                    (concat "FOR " (nth 0 args) " IN " (nth 1 args)))
                   ((equal "albIf" name)
                    (concat "IF " (nth 0 args)))
                   ((equal "albElseIf" name)
                    (concat "ELSE IF " (nth 0 args)))
                   ((equal "albElse" name)
                    "ELSE"))))
        ;; Use parser to retrieve region of math, environment, or command.
        (let ((start (point)))
          (cond
           ((looking-at alb-LaTeX-re-opening-math)
            (alb-LaTeX-collect-balanced-math
             alb-LaTeX-parser-context-standard))
           ((looking-at alb-LaTeX-re-opening-environment)
            (alb-LaTeX-collect-balanced-environment
             alb-LaTeX-parser-context-standard))
           ((looking-at alb-LaTeX-re-opening-command)
            (alb-LaTeX-collect-command alb-LaTeX-parser-context-standard)))
          (setq display
                (buffer-substring-no-properties start (point)))))
      ;; Purge display of spacing adjustments, bracing adjustments,
      ;; layout structures, and comments.
      (while (string-match alb-LaTeX-re-opening-command-spacing display)
        (setq display (replace-match " " nil t display)))
      (while (string-match alb-LaTeX-re-opening-command-bracing display)
        (setq display (replace-match " " nil t display)))
      (while (string-match alb-LaTeX-re-command-math-layout display)
        (setq display (replace-match " " nil t display)))
      (while (string-match alb-LaTeX-re-whitespace-removed display)
        (setq display (replace-match "" nil t display)))
      ;; Return the purged text.
      display)))



(defun alb-reftex-context-fn-albAlgorithmic-label (environment)
  "Determine a suitable context string for an item in an
\"albAlgorithmic\" environment, where the string is to be used in the
construction of the label.

This function customises RefTeX."
  (let ((case-fold-search nil))
    (save-excursion
      (cond
       ((and (re-search-backward "\\\\\\(begin\\|end\\){algorithm}" nil t)
             (equal (match-string-no-properties 1) "begin")
             (goto-char (match-end 0))
             (re-search-forward
              alb-LaTeX-re-procedure-call (+ (point) 200) t))
        ;; The algorithm is within a float containing a typeset
        ;; procedure call.
        (concat (alb-hyphenate-string-at-caps-and-gaps
                 (match-string-no-properties 1))
                "-"
                (number-to-string (set 'alb-LaTeX-line-counter
                                       (+ alb-LaTeX-line-counter 1)))))
       ((and (search-backward (concat "\\begin{" environment "}") nil t)
             (goto-char (match-beginning 0))
             (re-search-backward
              alb-LaTeX-re-procedure-call (- (point) 200) t))
        ;; The algorithm is preceded by a typeset procedure call.
        (concat (alb-hyphenate-string-at-caps-and-gaps
                 (match-string-no-properties 1))
                "-"
                (number-to-string (set 'alb-LaTeX-line-counter
                                       (+ alb-LaTeX-line-counter 1)))))
       (t
        ;; The algorithm is unnamed.
        (number-to-string (set 'alb-LaTeX-line-counter
                               (+ alb-LaTeX-line-counter 1))))))))



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new symbols, environments, and
;;     symbol extractors.

(TeX-add-style-hook "alb-algorithms"
 (function
  (lambda ()
    (TeX-add-symbols
     '("albListOfAlgorithms" 0)
     '("albNewAccessorIdent"
       (alb-TeX-arg-define-ident "acc" "accessor" downcase))
     '("albNewProcedureIdent"
       (alb-TeX-arg-define-ident "prc" "procedure" capitalize))
     '("albLet" "Variable" "Value")
     '("albAssign" "Variable" "Value")
     '("albWhile"
       (alb-TeX-arg-compound-statement "while" ("condition")))
     '("albForAll"
       (alb-TeX-arg-compound-statement "for" ("variable" "set")))
     '("albForSequence"
       (alb-TeX-arg-compound-statement "for" ("index" "sequence")))
     '("albIf"
       (alb-TeX-arg-compound-statement "if" ("condition")))
     '("albElseIf"
       (alb-TeX-arg-compound-statement "else if" ("condition")))
     '("albElse"
       (alb-TeX-arg-compound-statement "else" ()))
     '("albReturn" "Return value"))
    (LaTeX-add-environments
     '("algorithm" alb-LaTeX-env-algorithm)
     '("algorithm*" alb-LaTeX-env-algorithm)
     '("albAlgorithmic" LaTeX-env-item)
     '("albAlgorithmic*" LaTeX-env-item)
     '("albBlock" LaTeX-env-item))
    (TeX-auto-add-regexp
     alb-TeX-re-newIdent)
    (if (featurep 'reftex)
        (progn
          ;; Maintain counter for unique line number labels in algorithms
          (add-hook 'local-write-file-hooks
                    (function
                     (lambda ()
                       (if (< 0 alb-LaTeX-line-counter)
                           (alb-update-file-local-variable
                            'alb-LaTeX-line-counter
                            alb-LaTeX-line-counter)))))

          ;; Specify new label types for algorithm floats and line numbers
          (reftex-add-label-environments
           '(("algorithm"          ?a "alg:%f:" "~\\ref{%s}"
              ("\\\\caption[[{]" . alb-reftex-context-fn-algorithm)
              (regexp "Algorithm[s]?"))
             ("algorithm*"         ?a nil       nil
              ("\\\\caption[[{]" . alb-reftex-context-fn-algorithm))
             ("albAlgorithmic"     ?j "ln:%f:"  "~\\ref{%s}"
              (alb-reftex-context-fn-albAlgorithmic-display
               . alb-reftex-context-fn-albAlgorithmic-label)
              (regexp "Line[s]?"))
             ("albAlgorithmic*"    ?j "ln:%f:"  "~\\ref{%s}"
              (alb-reftex-context-fn-albAlgorithmic-display
               . alb-reftex-context-fn-albAlgorithmic-label))))

          ;; Auto generate labels for algorithm floats and line numbers
          (set (make-local-variable 'reftex-insert-label-flags)
               (cons (concat (car reftex-insert-label-flags) "aj")
                     (cdr reftex-insert-label-flags))))))))



;;   + Gang AUCTeX buffer parsing hooks to process symbols extracted
;;     from \albNewAccessorIdent and \albNewProcedureIdent macros.
;;
;;     While these hooks are globally active once a single buffer uses
;;     ``alb-algorithms'', parsing is only performed on buffers actually
;;     using ``alb-algorithms''.  For buffers not using ``alb-algorithms''
;;     the processing involved in these hooks is minimal.

(add-hook 'TeX-auto-prepare-hook 'alb-TeX-auto-newIdent-prepare)
(add-hook 'TeX-auto-cleanup-hook 'alb-TeX-auto-newIdent-cleanup)



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
