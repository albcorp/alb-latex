;;;
;;; AlbFloatTools/style/graphicx.el
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
;;;     prompting during insertion of graphics files.
;;;



;;; *** PROVIDED FEATURE ******************************************************



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-latex-read)



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new symbols, and environments.

(TeX-add-style-hook "graphicx"
 (function
  (lambda ()
    (TeX-add-symbols
     '("includegraphics"
       [alb-TeX-arg-keyvalues
	"\\includegraphics option"
	alb-LaTeX-keyvalues-includegraphics]
       alb-TeX-arg-graphic-file)))))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
