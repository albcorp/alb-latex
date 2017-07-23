;;;
;;; alb-latex/lisp/alb-latex-find-file.el
;;;
;;;     Copyright (C) 2003-2008, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Functions to construct skelton LaTeX files on first visit.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-latex-find-file)



;;; *** REQUIRED FEATURES *****************************************************


(require 'alb-filenames)



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-find-file (string)
  "Prompt for a string from which to construct the basename of a file.
Then open the file with a .tex extension."
  (interactive "sTitle: ")
  (let* ((bname (alb-construct-basename string))
         (fname (concat default-directory bname ".tex")))
    (if (file-exists-p fname)
        ;; File already exists --- let find-file report errors opening
        (find-file fname)
      (progn
        ;; New file -- load and build a rudimentary header
        (find-file fname)
        (insert "% ")
        (newline)
        (insert (concat "% :Precis: " string))
        (newline)
        (insert (concat "% :Authors: " user-full-name))
        (newline)
        (insert (concat "% :Contact: " user-mail-address))
        (newline)
        (insert (concat "% :Copyright: " (format-time-string "%Y")
                        " " user-full-name))
        (newline)
        (insert "% ")
        (newline 3)
        (forward-line -1)))))



(defun alb-LaTeX-find-letter (string)
  "Prompt for the name of the recipient from which to construct
the basename of a letter file.  Then open the file with a .tex
extension."
  (interactive "sTo: ")
  (let* ((addressee (combine-and-quote-strings (split-string string) "-"))
         (today (format-time-string "%Y-%m-%d"))
         (fname (concat "correspondence-to-" addressee "-" today ".tex")))
    (if (file-exists-p fname)
        ;; File already exists --- let find-file report errors opening
        (find-file fname)
      (progn
        ;; New file -- load and build a rudimentary header
        (find-file fname)
        (insert "% ")
        (newline)
        (insert (concat "% :Precis: Letter to " string))
        (newline)
        (insert (concat "% :Authors: " user-full-name))
        (newline)
        (insert (concat "% :Contact: " user-mail-address))
        (newline)
        (insert "% ")
        (newline 3)
        (forward-line -1)))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
