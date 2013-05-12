;;;
;;; AlbFloatTools/style/psfrag.el
;;;
;;;     Copyright (C) 2001-2003 Andrew Lincoln Burrow
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
;;;   - Override the provided AUCTeX customisation to provide detailed
;;;     prompting during insertion of psfrag environment.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-latex-read)
(require 'alb-latex-env)



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-TeX-arg-psfrag (optional &optional tag)
  "Prompt for and insert the arguments to a \\psfrag macro.  This
function inserts the complete set of arguments, and hence the OPTIONAL
parameter is meaningless.  This function exists to ensure that
`TeX-symbols-list' and `alb-TeX-arg-psfrags' can easily share behaviour.

If TAG is non-nil, use it as the value of the first argument rather than
prompting for the tag.

This function customises AUCTeX.  See: `alb-TeX-arg-alignment-point'."
  (if tag
      (insert "{" tag "}")
    (TeX-arg-string nil "Tag"))
  (TeX-parse-arguments
   '([alb-TeX-arg-alignment-point "LaTeX alignment point"]
     [alb-TeX-arg-alignment-point "PostScript alignment point"]
     ["LaTeX scaling"]
     ["LaTeX rotation"]
     "Replacement")))



(defun alb-TeX-env-psfrags (environment &rest junk)
  "Insert a psfrag ENVIRONMENT with prompting for the initial list of
replacements and the included eps file.

This function customises AUCTeX.  See: `alb-TeX-arg-psfrag'."
  (LaTeX-insert-environment environment)
  (let (tag)
    (while (and (setq tag
		      (read-from-minibuffer
		       (TeX-argument-prompt t nil "PSfrag tag")))
		(not (equal "" tag)))
      (insert "\\psfrag")
      (alb-TeX-arg-psfrag nil tag)
      (insert "%")
      (newline-and-indent))))



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new symbols, and environments.

(TeX-add-style-hook "psfrag"
 (function
  (lambda ()
    (TeX-add-symbols
     '("psfrag" alb-TeX-arg-psfrag))
    (LaTeX-add-environments
     '("psfrags" alb-TeX-env-psfrags)))))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
