;;;
;;; AlbFloatTools/style/alb-figures.el
;;;
;;;     Copyright (C) 2001-2005 Andrew Lincoln Burrow
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
;;;   - Customise AUCTeX and RefTeX for "alb-float-tools" LaTeX package.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-latex-parse)



;;; *** VARIABLE DECLARATIONS *************************************************


(defconst alb-LaTeX-re-label
  (concat			   ; Sub expressions:	Count	Total
   "\\\\label"						;  0	 0
   alb-LaTeX-re-whitespace-horizontal			;  2	 2
   "{" alb-LaTeX-re-whitespace-hidden			;  1	 3
   "\\([0-9A-Za-z:_-]+\\)"				;  1	 4
   alb-LaTeX-re-whitespace-hidden "}")			;  1	 5
  "Regular expression to extract label defined by \\label command.

This variable is used in AUCTeX file parsing.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-env-inflate (environment)
  "Insert an inflate environment ENVIRONMENT.  Search for a label within
the enclosing environment and include a non-nil result as an argument to
the environment.

This function customises AUCTeX."
  (let* ((case-fold-search nil)
	 (float-label (save-excursion
			(and (re-search-backward "\\\\begin\\>" nil t)
			     (goto-char (match-end 0))
			     (re-search-forward alb-LaTeX-re-label nil t)
			     (match-string-no-properties 4)))))
    (LaTeX-insert-environment environment (concat "{" float-label "}"))))



(defun alb-LaTeX-env-shelve (environment)
  "Insert a shelve environment ENVIRONMENT.  Prompt the user for the
alignment of the shelved items and include a non-nil result as an
optional argument to the environment.

This function customises AUCTeX."
  (let* ((alignment-chr (completing-read (TeX-argument-prompt
					  t nil "Alignment of shelved items")
					 '(("t") ("b") (""))
					 nil
					 t
					 ""))
	 (alignment-arg (if (not (string-equal "" alignment-chr))
			    (concat "[" alignment-chr "]"))))
    (LaTeX-insert-environment environment alignment-arg)
    (end-of-line 0)
    (delete-char 1)
    (delete-horizontal-space)
    (LaTeX-insert-item)))



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new symbols, environments, and
;;     RefTeX label types.

(TeX-add-style-hook "alb-float-tools"
 (function
  (lambda ()
    (TeX-add-symbols
     '("albCaptionStyleByMargin"
       TeX-arg-label "Style for left margin" "Style for right margin")
     '("albCaptionStyleBySide"
       TeX-arg-label "Style for left page" "Style for right page"))
    (LaTeX-add-environments
     '("albInflate" alb-LaTeX-env-inflate)
     '("albShelve" alb-LaTeX-env-shelve)))))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
