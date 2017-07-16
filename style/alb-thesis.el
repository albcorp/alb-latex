;;;
;;; alb-latex/style/alb-thesis.el
;;;
;;;     Copyright (C) 2003, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Customise AUCTeX for "alb-thesis" LaTeX class.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new symbols, environments, and RefTeX
;;     label types.

(TeX-add-style-hook "alb-thesis"
 (function
  (lambda ()
    (TeX-run-style-hooks "book")
    (TeX-add-symbols
     '("printindex")
     '("printglossary")))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
