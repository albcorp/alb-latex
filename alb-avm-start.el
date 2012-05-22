;;;
;;; AlbAVM/alb-avm-start.el
;;;
;;;     Copyright (C) 2012 Andrew Lincoln Burrow
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
;;;   - Site start code to advertise SiaReportLayout to AUCTeX.
;;;



;;; *** AMMENDED LOAD PATH ****************************************************



;;; *** REQUIRED FEATURES *****************************************************



;;; *** IMMEDIATE CONFIGURATION ***********************************************



;;; *** DEFERRED CONFIGURATION ************************************************


(defconst alb-latex-avm-style-dirname
  (convert-standard-filename (concat (directory-file-name
                                      (file-name-directory load-file-name))
                                     "/style"))
  "Directory containing hand-generated AUCTeX style files for AlbAVM.")


(add-hook
 'LaTeX-mode-hook
 (function
  (lambda ()
    ;; Extend TeX-style-private
    (setq TeX-style-private
          (cons alb-latex-avm-style-dirname TeX-style-private))))
 'append)





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
