;;;
;;; AlbLaTeX/style/alb-proofs.el
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
;;;   - Customise AUCTeX and RefTeX for "alb-proofs" LaTeX package.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-local-variables)



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-assertion-counter
  0
  "Count numbers assigned to steps in a structured proof.  This ensures
dirty numbers are not reissued.  Reissuing a number could make stale
references hard to detect.")



(defvar alb-LaTeX-assumption-counter
  0
  "Count numbers assigned to assumptions in a structured proof.  This
ensures dirty numbers are not reissued.  Reissuing a number could make
stale references hard to detect.")


(defconst alb-LaTeX-re-opening-command-assumption
  (concat "\\\\\\("
          "albAssume\\|albSuppose"
          "\\)\\>")
  "Regexp matching a command that formats a statement in the
\"albAssumptions\" or \"albSuppositions\" environment, but not its
arguments.  Namely \\albAssume or \\albSuppose.

NB: The parser expects the command name to be the 1st subexpression!")


(defconst alb-LaTeX-re-opening-command-assertion
  (concat "\\\\\\("
          "albAssert\\|albAssertBase\\|albAssertInduction"
          "\\|albQED\\|albQEDbyContradiction\\|albQEDbyInduction"
          "\\)\\>")
  "Regexp matching a command that formats a statement in the
\"albAssertions\" environment, but not its arguments.  Namely one of
\\albAssert, \\albAssertBase, \\albAssertInduction, \\albQED,
\\albQEDbyContradiction, or \\albQEDbyInduction.

NB: The parser expects the command name to be the 1st subexpression!")




;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-parse-command-assumption (context)
  "Parse an \"albAssumptions\" statement command.

Collect the arguments of any of the following LaTeX commands:
\\albAssume{STATEMENT}, or \\albSuppose{STATEMENT}.

See `alb-LaTeX-collect-balanced-braces' for a description of the
argument.

The result is returned as a singleton alist RESULT associating the name
of this function with an alist recording the parse.  Within RESULT:
  (symbol-name 'alb-LaTeX-parse-command-assumption) `assq's to the
  parse alist.
Within the parse alist:
  \"command\" `assoc's to the LaTeX command name,
  \"arguments\" `assoc's to the list of strings inside the LaTeX
  arguments, and
  \"at\" `assoc's to the position of the command's opening backslash.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (if (looking-at alb-LaTeX-re-opening-command-assumption)
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
        (list (cons (symbol-name 'alb-LaTeX-parse-command-assumption)
                    (list (cons "command" cname)
                          (cons "arguments" arguments)
                          (cons "at" begin)))))
    (error "%s: Not at an \"albAssumptions\" command")))



(defun alb-LaTeX-parse-command-assertion (context)
  "Parse an \"albAssertions\" statement command.

Collect the arguments of any of the following LaTeX commands:
\\albAssert{STATEMENT}, \\albAssertBase{CASE},
\\albAssertInduction{INDUCTION}, \\albQED, \\albQEDbyContradiction, or
\\albQEDbyInduction.

See `alb-LaTeX-collect-balanced-braces' for a description of the
argument.

The result is returned as a singleton alist RESULT associating the name
of this function with an alist recording the parse.  Within RESULT:
  (symbol-name 'alb-LaTeX-parse-command-assertion) `assq's to the
  parse alist.
Within the parse alist:
  \"command\" `assoc's to the LaTeX command name,
  \"arguments\" `assoc's to the list of strings inside the LaTeX
  arguments, and
  \"at\" `assoc's to the position of the command's opening backslash.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (if (looking-at alb-LaTeX-re-opening-command-assertion)
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
        (list (cons (symbol-name 'alb-LaTeX-parse-command-assertion)
                    (list (cons "command" cname)
                          (cons "arguments" arguments)
                          (cons "at" begin)))))
    (error "%s: Not at \"albAssertions\" command")))



(defun alb-reftex-context-fn-assumption-display (environment)
  "Determine a suitable context string for an item in an
\"albAssumptions\" environment, where the string is to be used in the
RefTeX table of contents buffer.

This function customises RefTeX."
  (save-excursion
    (let (display)
      (looking-at alb-LaTeX-re-matter-inside-structure)
      (goto-char (match-end 0))
      (if (looking-at alb-LaTeX-re-opening-command-assumption)
          ;; Use parser to retrieve arguments to statement command.
          (let* ((parse (alb-LaTeX-parse-command-assumption
                         alb-LaTeX-parser-context-standard))
                 (tree (assoc (symbol-name 'alb-LaTeX-parse-command-assumption)
                              parse))
                 (name (cdr (assoc "command" tree)))
                 (args (cdr (assoc "arguments" tree))))
            (setq display
                  (cond
                   ((equal "albAssume" name)
                    (concat "LET " (nth 0 args)))
                   ((equal "albSuppose" name)
                    (concat "SUPPOSE " (nth 0 args))))))
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



(defun alb-reftex-context-fn-assertion-display (environment)
  "Determine a suitable context string for an item in an
\"albAssertions\" environment, where the string is to be used in the
RefTeX table of contents buffer.

This function customises RefTeX."
  (save-excursion
    (let (display)
      (looking-at alb-LaTeX-re-matter-inside-structure)
      (goto-char (match-end 0))
      (if (looking-at alb-LaTeX-re-opening-command-assertion)
          ;; Use parser to retrieve arguments to statement command.
          (let* ((parse (alb-LaTeX-parse-command-assertion
                         alb-LaTeX-parser-context-standard))
                 (tree (assoc (symbol-name 'alb-LaTeX-parse-command-assertion)
                              parse))
                 (name (cdr (assoc "command" tree)))
                 (args (cdr (assoc "arguments" tree))))
            (setq display
                  (cond
                   ((equal "albAssert" name)
                    (concat "ASSERT " (nth 0 args)))
                   ((equal "albAssertBase" name)
                    (concat "ASSERT BASE " (nth 0 args)))
                   ((equal "albAssertInduction" name)
                    (concat "ASSERT INDUCTION " (nth 0 args)))
                   ((equal "albQED" name)
                    "Q.E.D.")
                   ((equal "albQEDbyContradiction" name)
                    "Q.E.D. BY CONTRADICTION")
                   ((equal "albQEDbyInduction" name)
                    "Q.E.D. BY MATHEMATICAL INDUCTION"))))
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



(defun alb-reftex-context-fn-assertion-label (environment)
  "Determine a suitable context string for the latest proof step.

This function customises RefTeX."
  (number-to-string (set 'alb-LaTeX-assertion-counter
                         (+ alb-LaTeX-assertion-counter 1))))



(defun alb-reftex-context-fn-assumption-label (environment)
  "Determine a suitable context string for the latest proof assumption.

This function customises RefTeX."
  (number-to-string (set 'alb-LaTeX-assumption-counter
                         (+ alb-LaTeX-assumption-counter 1))))



;;; *** ACTIONS ***************************************************************


;;   + Set the assertion and assumption counter variables to buffer
;;     local.

(make-variable-buffer-local 'alb-LaTeX-assertion-counter)
(make-variable-buffer-local 'alb-LaTeX-assumption-counter)



;;   + Gang AUCTeX style hook to import new symbols, environments, and
;;     RefTeX label types.

(TeX-add-style-hook "alb-proofs"
 (function
  (lambda ()
    (TeX-add-symbols
     '("albAssert" "Assertion")
     '("albAssertBase" "Base Assertion")
     '("albAssertInduction" "Inductive Assertion")
     '("albAssume" "Assumption")
     '("albQED")
     '("albQEDbyContradiction")
     '("albQEDbyInduction")
     '("albSuppose" "Supposition"))
    (LaTeX-add-environments
     '("albProof")
     '("albSketch")
     '("albAssumptions" LaTeX-env-item)
     '("albAssertions" LaTeX-env-item))
    (if (featurep 'reftex)
        (progn
          ;; Maintain counter for unique line number labels in proofs
          (add-hook 'local-write-file-hooks
                    (function
                     (lambda ()
                       (if (< 0 alb-LaTeX-assertion-counter)
                           (alb-update-file-local-variable
                            'alb-LaTeX-assertion-counter
                            alb-LaTeX-assertion-counter))
                       (if (< 0 alb-LaTeX-assumption-counter)
                           (alb-update-file-local-variable
                            'alb-LaTeX-assumption-counter
                            alb-LaTeX-assumption-counter)))))

          ;; Specify new label types for proofs
          (reftex-add-label-environments
           '(("albAssumptions"      ?l "as:%f:"  "~\\ref{%s}"
              (alb-reftex-context-fn-assumption-display
               . alb-reftex-context-fn-assumption-label))
             ("albAssertions"       ?m "pf:%f:"  "~\\ref{%s}"
              (alb-reftex-context-fn-assertion-display
               . alb-reftex-context-fn-assertion-label))))

          ;; Auto generate labels for proofs
          (set (make-local-variable 'reftex-insert-label-flags)
               (cons (concat (car reftex-insert-label-flags) "lm")
                     (cdr reftex-insert-label-flags))))))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
