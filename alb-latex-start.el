;;;
;;; alb-latex/alb-latex-start.el
;;;
;;;     Copyright (C) 2000-2013, 2017 Andrew Lincoln Burrow
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
;;;   - Site start code for alb emacs lisp to support working with
;;;     `AlbLaTeX` stylesheets in `AUCTeX`
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
;; REQUIRED FEATURES
;; ---------------------------------------------------------------------
;;

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

;;
;;
;; DEFERRED CONFIGURATION
;; ---------------------------------------------------------------------
;;

(autoload 'alb-LaTeX-find-file "alb-latex-find-file"
  "Prompt for a string from which to construct the basename of a file.
Then open the file with a .tex extension." t)

(autoload 'alb-LaTeX-find-letter "alb-latex-find-file"
  "Prompt for the name of the recipient from which to construct the
basename of a letter file. Then open the file with a .tex extension." t)

(eval-after-load 'tex
  ;; Put the style files from this package at the front of `TeX-style-path`
  '(add-to-list 'TeX-style-path alb-latex-style-dirname))

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
