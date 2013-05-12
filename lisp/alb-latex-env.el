;;;
;;; AlbAUCTeX/alb-latex-env.el
;;;
;;;     Copyright (C) 2000-2003 Andrew Lincoln Burrow
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
;;;   - Functions to insert formatted LaTeX environments.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-latex-env)



;;; *** REQUIRED FEATURES *****************************************************


(require 'tex-site)



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************



(defun alb-LaTeX-env-float (environment &rest junk)
  "Insert a float ENVIRONMENT with prompting.  Insert the \\caption with
an optional abbreviated form, and generate a \\label.

This hook is best used when the float label is derived from the caption.
If the label is derived from some other part of the environment, this
hook could cause problems.

This function customises AUCTeX."
  (let* ((float   (read-from-minibuffer
		   "(optional) Float to: " LaTeX-float))
	 (abrieve (read-from-minibuffer
		   "(optional) Abbreviated caption: "))
	 (caption (read-from-minibuffer
		   "Caption: " abrieve)))
    (LaTeX-insert-environment
     environment
     (if (not (string-equal "" float)) (concat "[" float "]")))
    (save-excursion
      (if (string-equal "" abrieve)
	  (progn (insert (concat "\\caption{" caption "}"))
		 (newline-and-indent))
	(progn (insert (concat "\\caption[" abrieve "]{%"))
	       (newline-and-indent)
	       (insert (concat caption "}"))
	       (newline-and-indent)))
      (LaTeX-label environment))))



(defun alb-LaTeX-env-table (environment &rest junk)
  "Insert a table ENVIRONMENT with prompting.  Call
`alb-LaTeX-env-float' to insert the \\caption with an optional
abbreviated form, and generate a \\label.  Then insert a tabular
environment.

This function customises AUCTeX."
  (alb-LaTeX-env-float environment)
  (LaTeX-environment-menu "tabular"))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
