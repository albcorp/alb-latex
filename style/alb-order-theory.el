;;;
;;; AlbOrderTheory/style/alb-order-theory.el
;;;
;;;     Copyright (C) 2000-2005 Andrew Lincoln Burrow
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
;;;   - Customise AUCTeX and RefTeX for "alb-order-theory" LaTeX package.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-local-variables)
(require 'alb-latex-parse)



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-equation-counter
  0
  "Count numbers assigned to labels for equations.  This ensures dirty
numbers are not reissued.  Reissuing a number could make stale
references hard to detect.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-collect-equation (context)
  "Collect the equation upto the next end of line command or the end of
the environment.

See `alb-LaTeX-collect-balanced-braces' for a description of the return
value and argument."
  (let ((start (point))
	(results))
    ;; Iterate until a '\\' command or closing environment is next.
    (while (and (looking-at alb-LaTeX-re-matter-inside-structure)
		(goto-char (match-end 0))
		(not (looking-at alb-LaTeX-re-opening-command-end-of-line))
		(not (looking-at alb-LaTeX-re-closing-environment)))
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
	(error "%s: %d: %d: Unexpected parse error when collecting equation"
	       (buffer-name) begin (point)))))

    ;; Return the collected results.
    results))


(defun alb-reftex-context-fn-equation-display (environment)
  "Determine a suitable context string for an equation, where the string
is to be used in the RefTeX table of contents buffer.

This function customises RefTeX."
  (save-excursion
    (let* ((start (point))
	   (finish (progn (alb-LaTeX-collect-equation
			   alb-LaTeX-parser-context-standard)
			  (point)))
	   (text (buffer-substring-no-properties start finish)))
      ;; Purge 'source' of spacing adjustments, bracing adjustments,
      ;; layout structures, and comments.
      (while (string-match alb-LaTeX-re-opening-command-spacing text)
	(setq text (replace-match " " nil t text)))
      (while (string-match alb-LaTeX-re-opening-command-bracing text)
	(setq text (replace-match " " nil t text)))
      (while (string-match alb-LaTeX-re-command-math-layout text)
	(setq text (replace-match " " nil t text)))
      (while (string-match alb-LaTeX-re-whitespace-removed text)
	(setq text (replace-match "" nil t text)))
      ;; Return the purged text.
      text)))


(defun alb-reftex-context-fn-equation-label (environment)
  "Determine a suitable context string for an equation, where the string
is to be used in the construction of the label.

This function customises RefTeX."
  (number-to-string (set 'alb-LaTeX-equation-counter
			 (+ alb-LaTeX-equation-counter 1))))



;;; *** ACTIONS ***************************************************************


;;   + Set the equation counter variable to buffer local.

(make-variable-buffer-local 'alb-LaTeX-equation-counter)


;;   + Gang AUCTeX style hook to import new symbols, environments, and
;;     RefTeX label types.

(TeX-add-style-hook "alb-order-theory"
 (function
  (lambda ()
    (TeX-add-symbols
     '("albNot")
     '("albAnd")
     '("albOr")
     '("albSingleton" "Singleton element")
     '("albCardinality" "Set")
     '("albTuple" "Tuple sequence")
     '("albPowerset" "Set")
     '("albBinaryRelation" "Relation symbol" "Domain" "Range")
     '("albPartialMap" "Function symbol" "Domain" "Range")
     '("albTotalMap" "Function symbol" "Domain" "Range")
     '("albDomain" "Function symbol")
     '("albRange" "Function symbol")
     '("albCompose")
     '("albNaturals")
     '("albReals")
     '("albIncomparable")
     '("albCoveredBy")
     '("albCovers")
     '("albEquivClass" "Element")
     '("albDual" "Ordered set, or statement")
     '("albOver" "Ordered set" "Element")
     '("albUnder" "Ordered set" "Element")
     '("albMax" "Set")
     '("albMin" "Set")
     '("albHeads" "Set")
     '("albTails" "Set")
     '("albWidth" "Set")
     '("albHeight" "Set")
     '("albJoin")
     '("albJoinOf" "Set")
     '("albJoinInOf" "Ordered set" "Set")
     '("albMeet")
     '("albMeetOf" "Set")
     '("albMeetInOf" "Ordered set" "Set")
     '("albOrderEmbedding" "Function symbol" "Domain" "Range")
     '("albOrderIsomorphic")
     '("albDown" "Set")
     '("albUp" "Set")
     '("albDownInOf" "Ordered set" "Set")
     '("albUpInOf" "Ordered set" "Set")
     '("albLower" "Set")
     '("albUpper" "Set")
     '("albLowerInOf" "Ordered set" "Set")
     '("albUpperInOf" "Ordered set" "Set")
     '("albDownSets" "Ordered set")
     '("albRise" "Ordered set" "Element")
     '("albFall" "Ordered set" "Element")
     '("albDM" "Ordered set")
     '("albCD" "Ordered set")
     '("albExtenders" "Ordered set" "Consistent down-set")
     '("albRetractors" "Ordered set" "Consistent down-set")
     '("albBigOh" "Asymptote function")
     '("albBigOmega" "Asymptote function")
     '("albBigTheta" "Asymptote function"))
    (if (featurep 'reftex)
	(progn
	  (add-hook 'local-write-file-hooks
		    (function
		     (lambda ()
		       (if (< 0 alb-LaTeX-equation-counter)
			   (alb-update-file-local-variable
			    'alb-LaTeX-equation-counter
			    alb-LaTeX-equation-counter)))))
	  (setq reftex-label-alist
		(append reftex-label-alist
			'(("align"              ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("gather"             ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("multline"           ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("flalign"            ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("alignat"            ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("xalignat"           ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("xxalignat"          ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))
			  ("subequations"       ?e nil       nil
			   (alb-reftex-context-fn-equation-display
			    . alb-reftex-context-fn-equation-label))))))))))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
