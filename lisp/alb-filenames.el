;;;
;;; alb-latex/lisp/alb-filenames.el
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
;;;   - Emacs lisp functions to construct filenames.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-filenames)



;;; *** REQUIRED FEATURES *****************************************************


(eval-when-compile (require 'cl))



;;; *** VARIABLE DECLARATIONS *************************************************



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-construct-abbrev (input separtr-re illegal-re ignore-words
                                   words-max chars-max
                                   separator)
  "Convert INPUT (a sentence) to a suitable abbreviation.  In
particular, an abbreviation for the basename of a file.

SEPARTR-RE      Regexp used to split the string into words.
ILLEGAL-RE      Regexp used to recognise illegal characters.
IGNORE-WORDS    List of words to be removed from the string.
WORDS-MAX       Maximum number of words to use.
CHARS-MAX       Maximum number of characters in the output.
SEPARATOR       String separating different words in the output.

Copied in spirit from \"reftex.el\"."
  (let ((init-words (split-string input separtr-re))
        final-words
        word
        (abbrev-re  (concat
                     "\\`\\("
                     (make-string 4 ?.) ; Minimum chars to include.
                     "[^aeiou]*"        ; Chars before abbrev point in word.
                     "\\)"
                     "[aeiou]"          ; Chars after  abbrev point in word.
                     (make-string 1 ?.) ; Minimum chars to exclude.
                     )))
    ;; Remove words from the ignore list or with funny characters.
    (while (setq word (pop init-words))
      (setq word (downcase word))
      (cond
       ((member word ignore-words)
        )
       ((string-match illegal-re word)
        (setq word (replace-match "" nil nil word))
        (while (string-match illegal-re word)
          (setq word (replace-match "" nil nil word)))
        (push word final-words))
       (t
        (push word final-words))))
    (setq final-words (nreverse final-words))

    ;; Restrict number of words.
    (if (> (length final-words) words-max)
        (setcdr (nthcdr (1- words-max) final-words) nil))

    ;; Abbreviate words.
    (setq final-words
          (mapcar
           (function
            (lambda (w) (if (string-match abbrev-re w)
                            (match-string 1 w)
                          w)))
           final-words))

    ;; Construct string with separators.
    (setq input
          (mapconcat 'identity final-words separator))

    ;; Shorten if still too long.
    (setq input
          (if (> (length input) chars-max)
              (substring input 0 chars-max)
            input))))



(defun alb-construct-basename (input)
  "Use the builtin parameters to construct, from INPUT, a string
suitable as the basename of a file."
  (alb-construct-abbrev
   input
   "[-_~ \t\n\r,;]+"                    ; Separators in input.
   "[^-_a-zA-Z0-9+]"                    ; Illegal characters in basename.
   '("the" "on" "in" "off" "a" "for"    ; Ignored words.
     "by" "of" "and" "is" "to" "with")
   5 30                                 ; Maximum number of words/chars.
   "_"))                                ; Separator string.



(defun alb-hyphenate-string-at-caps (input)
  "Hyphenate INPUT where the case changes from lower to upper case."
  (let ((case-fold-search nil)
        (pos 0))
    (while (and (< pos (length input))
                (string-match "\\(.\\)\\([A-Z]\\)" input pos))
      (setq pos (match-end 0))
      (setq input (replace-match "\\1-\\2" t nil input)))
    input))



(defun alb-hyphenate-string-at-caps-and-gaps (input)
  "Hyphenate INPUT where the case changes from lower to upper case, or
there is whitespace."
  (let ((case-fold-search nil)
        (pos 0))
    ;; Strip off initial whitespace.
    (if (string-match "[ \f\t\n\r\v]*" input)
        (setq input (substring input (match-end 0))))

    ;; Replace whitespace or caps-change by hyphen in rest of string.
    (while (< pos (length input))
      (cond ((string-match "\\(.\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]\\)"
                           input pos)
             (setq pos (+ (match-end 1) 1))
             (setq input (replace-match "\\1-\\2" t nil input)))
            ((string-match "\\(.\\)\\([A-Z]\\)" input pos)
             (setq pos (+ (match-end 1) 1))
             (setq input (replace-match "\\1-\\2" t nil input)))
            (t
             (setq pos (length input)))))

    input))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
