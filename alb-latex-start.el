;;;
;;; AlbLaTeX/alb-latex-start.el
;;;
;;;     Copyright (C) 2000-2013 Andrew Lincoln Burrow
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
;;;   - Site start code for alb emacs lisp to support working with `AlbLaTeX`
;;;     stylesheets in `AUCTeX`.
;;;

;;
;;
;; AMMENDED LOAD PATH
;; ---------------------------------------------------------------------------
;;

(setq load-path
      (cons (concat (file-name-directory load-file-name) "lisp")
            load-path))

;;
;;
;; IMMEDIATE CONFIGURATION
;; ---------------------------------------------------------------------------
;;

(defconst alb-latex-style-dirname
  (convert-standard-filename (concat (directory-file-name
                                      (file-name-directory load-file-name))
                                     "/style"))
  "Directory containing hand-generated AUCTeX style files for AlbLaTeX.")

;; Set label construction rules and information on environments for
;; \label-\ref use to buffer local.  This allows AUCTeX style
;; information to further customise AUCTeX on a per buffer level
;; according to the packages used:

;;  - automate construction for section, figure, table, and equation
;;    labels.
(make-variable-buffer-local 'reftex-insert-label-flags)
(setq-default reftex-insert-label-flags
              '("sfte" "ft"))

;;  - include the file basename in the section, figure, table, and
;;    equation labels
(make-variable-buffer-local 'reftex-label-alist)
(setq-default reftex-label-alist
              '((nil ?s "%S%f:")
                (nil ?f "fig:%f:")
                (nil ?t "tab:%f:")
                (nil ?e "eq:%f:")))

;; Set the parameters for reftex abbreviations
(setq reftex-derive-label-parameters
      '(5 25 t t "-"
          ("a" "an" "the" "on" "in" "off" "as" "to" "for" "by" "of" "and"
           "is")
          t))
(setq reftex-abbrev-parameters
      '(4 3 "^aeiou" "aeiou"))

;; Maintain all BibTeX entries in sorted order, but ignore string
;; entries
(setq bibtex-maintain-sorted-entries t)
(setq bibtex-sort-ignore-string-entries t)

;; Specify the layout of bibtex entries.
;; A text indentation of 18 puts the equal sign at 16, i.e., two tab stops
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 18)
(setq bibtex-contline-indentation 19)

;; Specify the formatting performed by function bibtex-clean-entry: strip down
;; to mandatory and used fields, remove delimiters from numeric fields,
;; indent, treat last comma according to bibtex-comma-after-last-field,
;; delimit fields according to bibtex-field-delimiters and
;; bibtex-entry-delimiters, and change case of entry and field names
(setq bibtex-entry-format
      '(opts-or-alts numerical-fields realign last-comma delimiters
                     unify-case))

;;
;;
;; DEFERRED CONFIGURATION
;; ---------------------------------------------------------------------------
;;

(add-hook
 'after-init-hook
 (function
  (lambda ()
    ;; Make 'alb-LaTeX-find-file' available before AUCTeX starts
    (require 'alb-latex-find-file)))
 'append)

(add-hook
 'LaTeX-mode-hook
 (function
  (lambda ()
    ;; Extend TeX-style-private
    (setq TeX-style-private
          (cons alb-latex-style-dirname TeX-style-private))

    ;; Start in reftex minor mode
    (turn-on-reftex)

    ;; Make regexp isearch wrap around line break and tab whitespace
    (setq search-whitespace-regexp "[ \t\r\n]+")

    ;; Untabify all LaTeX buffers on save
    (setq indent-tabs-mode nil)
    (setq TeX-auto-untabify t)
    (setq whitespace-check-buffer-indent nil)

    ;; Use alb extensions to the AUCTeX TeX-arg-* commands
    (require 'alb-latex-read)

    ;; Replace file argument symbols with versions restricted to private
    ;; files, that use the information in '${TEXINPUTS}' and '${BIBINPUTS}'
    (TeX-add-symbols
     '("include" alb-TeX-arg-latex-file ignore)
     '("input" alb-TeX-arg-latex-file ignore)
     '("bibliography" alb-TeX-arg-bibtex-file ignore))

    ;; Use alb extensions to the AUCTeX LaTeX-env-* commands
    (require 'alb-latex-env)

    ;; Replace float environments with versions that prompt for abbreviated
    ;; captions.  This improves the label generation and is important for
    ;; lists of figures and tables
    (LaTeX-add-environments
     '("figure" alb-LaTeX-env-float ignore)
     '("figure*" alb-LaTeX-env-float ignore)
     '("table" alb-LaTeX-env-table ignore)
     '("table*" alb-LaTeX-env-table ignore))))
 'append)

(add-hook
 'bibtex-mode-hook
 (function
  (lambda ()
    ;; Set the fill column to allow entries to extend to end of line
    (setq fill-column 78)))
 'append)

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
