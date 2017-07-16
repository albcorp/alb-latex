;;;
;;; alb-latex/style/alb-theorems.el
;;;
;;;     Copyright (C) 2000-2003, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Customise AUCTeX and RefTeX for "alb-theorems" LaTeX package.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-local-variables)
(require 'alb-latex-parse)



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-theorem-counter
  0
  "Count numbers assigned to labels for theorem environments.  This
ensures dirty numbers are not reissued.  Reissuing a number could make
stale references hard to detect.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-env-theorem (environment)
  "Insert a theorem environment ENVIRONMENT.  Prompt the user for the
name of the theorem and include a non-nil result as an optional argument
to the environment.

This function customises AUCTeX."
  (let* ((theorem-type (if (string-equal "alb" (substring environment 0 3))
                           (capitalize (substring environment 3))
                         (capitalize environment)))
         (theorem-name (read-from-minibuffer
                        (concat theorem-type " name: "))))
    (if (string-equal "" theorem-name)
        (LaTeX-insert-environment environment)
      (LaTeX-insert-environment environment (concat "[" theorem-name "]")))
    (LaTeX-label environment)
    (newline-and-indent)))



(defun alb-reftex-context-fn-theorem-display (environment)
  "Determine a suitable context string for the enclosing theorem
environment ENVIRONMENT, where the string is to be used in the RefTeX
table of contents buffer.

This function customises RefTeX."
  (save-excursion
    (search-backward (concat "\\begin{" environment "}") nil t)
    (goto-char (match-end 0))
    (cond
     ((looking-at alb-LaTeX-re-next-opt)
      ;; The theorem is named. Purge name of comments
      (let* ((start (match-end 0))
             (finish (progn (goto-char (- start 1))
                            (alb-LaTeX-collect-balanced-brackets
                             alb-LaTeX-parser-context-standard)
                            (- (point) 1)))
             (text (buffer-substring-no-properties start finish)))
        (while (string-match alb-LaTeX-re-whitespace-removed text)
          (setq text (replace-match "" nil t text)))
        text))
     (t
      ;; The theorem is unnamed. Purge environment of crossrefs and comments
      (let* ((start (progn (looking-at alb-LaTeX-re-whitespace-horizontal)
                           (goto-char (match-end 0))
                           (while (looking-at alb-LaTeX-re-command-crossref)
                             (alb-LaTeX-collect-command
                              alb-LaTeX-parser-context-standard)
                             (looking-at alb-LaTeX-re-whitespace-horizontal)
                             (goto-char (match-end 0)))
                           (point)))
             (finish (progn (search-forward
                             (concat "\\end{" environment "}") nil t)
                            (goto-char (match-beginning 0))
                            (- (point) 1)))
             (text (buffer-substring-no-properties start finish)))
        (while (string-match alb-LaTeX-re-whitespace-removed text)
          (setq text (replace-match "" nil t text)))
        text)))))



(defun alb-reftex-context-fn-theorem-label (environment)
  "Determine a suitable context string for the enclosing theorem
environment ENVIRONMENT, where the string is to be used in the
construction of the label.

This function customises RefTeX."
  (save-excursion
    (search-backward (concat "\\begin{" environment "}") nil t)
    (goto-char (match-end 0))
    (cond
     ((looking-at alb-LaTeX-re-next-opt)
      ;; The theorem is named. Purge name of comments
      (let* ((start (match-end 0))
             (finish (progn (goto-char (- start 1))
                            (alb-LaTeX-collect-balanced-brackets
                             alb-LaTeX-parser-context-standard)
                            (- (point) 1)))
             (text (buffer-substring-no-properties start finish)))
        (while (string-match alb-LaTeX-re-whitespace-removed text)
          (setq text (replace-match "" nil t text)))
        text))
     (t
      ;; The theorem is unnamed. Supply number
      (number-to-string (set 'alb-LaTeX-theorem-counter
                             (+ alb-LaTeX-theorem-counter 1)))))))



;;; *** ACTIONS ***************************************************************


;;   + Set the theorem counter variables to buffer local.  These
;;     variables are saved in the file's local variable list.

(make-variable-buffer-local 'alb-LaTeX-theorem-counter)



;;   + Gang AUCTeX style hook to import new symbols, environments, and
;;     RefTeX label types.

(TeX-add-style-hook "alb-theorems"
 (function
  (lambda ()
    (TeX-add-symbols
     '("albDRef" TeX-arg-ref)
     '("albTRef" TeX-arg-ref)
     '("albLRef" TeX-arg-ref)
     '("albRRef" TeX-arg-ref)
     '("albCRef" TeX-arg-ref)
     '("albTheoremSeparator"))
    (LaTeX-add-environments
     '("albDefinition" alb-LaTeX-env-theorem)
     '("albTheorem" alb-LaTeX-env-theorem)
     '("albLemma" alb-LaTeX-env-theorem)
     '("albRemark" alb-LaTeX-env-theorem)
     '("albCorollary" alb-LaTeX-env-theorem)
     '("albPropositions" LaTeX-env-item))
    (if (featurep 'reftex)
        (progn
          ;; Override TeX symbol bindings to use RefTeX features
          (TeX-add-symbols
           '("albDRef" (alb-TeX-arg-ref "d") ignore)
           '("albTRef" (alb-TeX-arg-ref "u") ignore)
           '("albLRef" (alb-TeX-arg-ref "v") ignore)
           '("albRRef" (alb-TeX-arg-ref "w") ignore)
           '("albCRef" (alb-TeX-arg-ref "x") ignore))

          ;; Maintain counter for unique theorems labels
          (add-hook 'local-write-file-hooks
                    (function
                     (lambda ()
                       (if (< 0 alb-LaTeX-theorem-counter)
                           (alb-update-file-local-variable
                            'alb-LaTeX-theorem-counter
                            alb-LaTeX-theorem-counter)))))

          ;; Specify new label types for theorem environments
          (reftex-add-label-environments
           '(("albDefinition"      ?d "def:%f:" "~\\ref{%s}"
              (alb-reftex-context-fn-theorem-display
               . alb-reftex-context-fn-theorem-label)
              (regexp "Definition[s]?"))
             ("albTheorem"          ?u "thm:%f:" "~\\ref{%s}"
              (alb-reftex-context-fn-theorem-display
               . alb-reftex-context-fn-theorem-label)
              (regexp "Theorem[s]?"))
             ("albLemma"            ?v "lem:%f:" "~\\ref{%s}"
              (alb-reftex-context-fn-theorem-display
               . alb-reftex-context-fn-theorem-label)
              (regexp "Lemma\\(ta\\)?"))
             ("albRemark"           ?w "rem:%f:" "~\\ref{%s}"
              (alb-reftex-context-fn-theorem-display
               . alb-reftex-context-fn-theorem-label)
              (regexp "Remark[s]?"))
             ("albCorollary"        ?x "cor:%f:" "~\\ref{%s}"
              (alb-reftex-context-fn-theorem-display
               . alb-reftex-context-fn-theorem-label)
              (regexp "Corollar\\(y\\|ies\\)"))))

          ;; Auto generate labels for theorem environments
          (set (make-local-variable 'reftex-insert-label-flags)
               (cons (concat (car reftex-insert-label-flags) "duvwx")
                     (cdr reftex-insert-label-flags))))))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
