;;;
;;; alb-latex/lisp/alb-local-variables.el
;;;
;;;     Copyright (C) 2000-2005, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Emacs lisp functions to manipulate local variables lists.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-local-variables)



;;; *** REQUIRED FEATURES *****************************************************


(eval-when-compile (require 'cl))



;;; *** VARIABLE DECLARATIONS *************************************************


(defconst alb-re-local-variables
  (concat "^\\(.+\\)Local Variables:[ \t]*\n"
          "\\(\\(\\1[0-9A-Za-z_-]+:[ \t]+.*\n\\)*\\)"
          "\\1End:[ \t]*\n")
  "Regexp matching the file local variables.  The first subexpression
matches the prefix used for the rest of the entries.  The second
subexpression matches the lines containing file local variables.  The
fourth subexpression matches the endline.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-update-file-local-variable (variable value)
  "Update the local variable list in the file.  If no local variable
list is present, then do nothing since the prefix will not be known.
If VARIABLE is present, then Set VARIABLE to VALUE.  Otherwise, insert
VARIABLE in the list with VALUE."
  (if (and (buffer-file-name) (not buffer-read-only))
      (save-excursion
        (goto-char (point-max))
        (if (re-search-backward alb-re-local-variables (- (point) 3000) t)
            (let* ((case-fold-search nil)
                   (prefix (match-string-no-properties 1))
                   (begin (match-beginning 2))
                   (end (match-end 2))
                   (varline (and (goto-char begin)
                                 (re-search-forward
                                  (concat "^" (regexp-quote prefix)
                                          "\\(" (symbol-name variable) "\\):"
                                          "[ \t]+.*")
                                  end t)
                                 (list (match-beginning 0) (match-end 0)))))
              (if varline
                  ;; Delete existing entry and leave point at start of line.
                  (progn (delete-region (nth 0 varline) (nth 1 varline))
                         (goto-char (nth 0 varline)))
                ;; Newline before end marker and leave point at start of line.
                (progn (goto-char end)
                       (end-of-line 0)
                       (newline)))
              (insert prefix (symbol-name variable) ": "
                      (prin1-to-string value)))))))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
