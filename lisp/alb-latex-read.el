;;;
;;; alb-latex/lisp/alb-latex-read.el
;;;
;;;     Copyright (C) 2000-2006, 2013, 2017 Andrew Lincoln Burrow
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
;;;   - Functions to read LaTeX objects from the minibuffer.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-latex-read)



;;; *** REQUIRED FEATURES *****************************************************


(require 'tex-site)
(require 'reftex)



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-keyvalues-includegraphics
  '(("bb" "bounding box x_min y_min x_max y_max")
    ("bbllx" "bounding box x_min")
    ("bblly" "bounding box y_min")
    ("bburx" "bounding box x_max")
    ("bbury" "bounding box y_max")
    ("natwidth" "bounding box natural width")
    ("natheight" "bounding box natural height")
    ("hiresbb" "whether to search for hires bounding box"
     "true" "true" "false")
    ("viewport" "bounding box relative lx ly ux uy")
    ("trim" "bounding box trims lx ly ux uy")
    ("angle" "rotation angle")
    ("origin" "rotation origin"
     "Bl" "tl" "tr" "tc" "bl" "br" "bc" "Bl" "Br" "Bc" "cl" "cr" "cc")
    ("scale" "scale factor")
    ("width" "scaled graphic width")
    ("height" "scaled graphic height")
    ("totalheight" "graphic height after rotation")
    ("keepaspectratio" "whether to preserve aspect ratio"
     "true" "true" "false")
    ("clip" "whether to clip bounding box"
     "true" "true" "false")
    ("draft" "whether to use draft mode"
     "true" "true" "false"))
  "The available key-value pairs for the graphicx package
\\includegraphics command.

This variable customises AUCTeX.  See: `alb-LaTeX-read-keyvalue'.")



(defvar alb-LaTeX-graphic-extensions
  '("pdf" "png" "jpg")
  "Graphic file extensions known to LaTeX.  These values should match
those reported by \"kpsewhich\".

NB: the value of this variable is the major difference between LaTeX and
pdfLaTeX.  In particular, pdfLaTeX expects a PDF file, but can also read
PNG and JPEG files.

This variable customises AUCTeX.  See: `alb-LaTeX-read-graphic-file'.")



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-read-alignment-point (optional &optional prompt)
  "Read an alignment point specification or empty string from the minibuffer.

If OPTIONAL, modify the prompt, and allow a blank value.

If PROMPT is non-nil, use it as the prompt.  Otherwise, use a default.

This function customises AUCTeX, via `alb-TeX-arg-alignment-point' and
others."
  (completing-read (TeX-argument-prompt optional prompt "Alignment point")
                   (append (if optional '(("")) '())
                           '(("tl") ("tr") ("tc")
                             ("bl") ("br") ("bc")
                             ("Bl") ("Br") ("Bc")
                             ("cl") ("cr") ("cc")))
                   nil
                   t
                   "Bl"))



(defun alb-LaTeX-read-keyvalue (optional &optional prompt keyvalues)
  "Read a graphicx style keyvalue pair from the minibuffer, and return
as a list.  Return nil if the user enters a blank key.

If OPTIONAL, modify the prompt, and allow a blank key to abort the
entry.

If PROMPT is non-nil, use it to prefix the prompts for the key and value
parts.  The remainder of the prompt is derived from KEYVALUES.
Otherwise, use a default suitable for the \\includegraphics command.

If KEYVALUES is non-nil, use it as the list of legal key-value pairings.
Otherwise, use a default suitable for the \\includegraphics command.
The alist maps each legal key to a description of the type of value ---
for each element, the car is a legal key, and the cdr is a list

    (&optional VPRMPT, VINIT, VLEGAL1, VLEGAL2, ...)

where VPRMPT is the additional prompt for the key's value, VINIT is the
initial value, and VLEGAL1, VLEGAL2, ... is the collection of legal
values.  If legal values are supplied, the value is restricted to one
such value.

This function customises AUCTeX, via `alb-TeX-arg-keyvalues'."
  (let* ((pre-prmpt (if prompt
                        prompt
                      "\\includegraphics option"))
         (pre-kvals (if keyvalues
                        keyvalues
                      alb-LaTeX-keyvalues-includegraphics))
         (key-prmpt (TeX-argument-prompt optional
                                         (concat pre-prmpt ", key")
                                         ""))
         (key (completing-read key-prmpt
                               (if optional
                                   (append '(("")) pre-kvals)
                                 pre-kvals)
                               nil t)))
    (if (not (equal key ""))
        (let* ((key-assoc (cdr (assoc key pre-kvals)))
               (val-prmpt (TeX-argument-prompt nil
                                               (concat pre-prmpt
                                                       ", "
                                                       (if (car key-assoc)
                                                           (car key-assoc)
                                                         "value"))
                                               ""))
               (val-initv (cadr key-assoc))
               (value (if (cddr key-assoc)
                          ;; Select one of the supplied legal values.
                          (completing-read val-prmpt
                                           (cddr key-assoc)
                                           nil
                                           t
                                           val-initv)
                        ;; Input an arbitrary string.
                        (read-from-minibuffer val-prmpt
                                              val-initv))))
          (list key value)))))



(defun alb-LaTeX-private-tex-search-path (type master)
  "Retrieve the list of non-system directories searched by TeX for TYPE
files.  Use MASTER to interpret relative directory links.

The function queries reftex for the search path, and is therefore
subject to the configuration of reftex.  By default reftex uses the
TEXINPUTS environment variable, and this function works best when
TEXINPUTS contains only private search paths.  It discards directories
which do not exist after expansion.

TYPE is the type of search either \"tex\" or \"bib\".

If MASTER is `t', assume the current buffer holds the master file, and
expand the search path to relative to the buffer's default directory;
'shared, elide relative directories from the search path; and a
filename, expand the search path relative to the directory part of the
filename.

Otherwise, if MASTER is a string but not a filename, raise an error to
warn the user.  The other possible values of `TeX-master' are nil and
'dwim.  In these cases, raise an error to warn that these value are
unsupported.

This function customises AUCTeX."
  (let* ((dname (cond
                 ((equal t master)
                  (file-name-directory (buffer-file-name)))
                 ((stringp master)
                  (cond                 ; String cases:
                   ((file-exists-p master)
                    (file-name-directory (expand-file-name master)))
                   ((file-exists-p (concat master "." TeX-default-extension))
                    (file-name-directory
                     (expand-file-name
                      (concat master "." TeX-default-extension))))
                   (t
                    (error "%s: TeX master file does not exist" master))))
                 ((symbolp master)      ; Symbol cases:
                  (cond
                   ((eq 'shared master)
                    nil)
                   (t
                    (error "Unsupported TeX master value"))))
                 ((null master)
                  (error "Unsupported TeX master value of nil"))))
         (spath (reftex-access-search-path type))
         (head (car spath))
         (tail (cdr spath))
         (result '()))
    (if dname
        ;; Expand the filenames on the search path with DNAME, and
        ;; discard unreadable directories.
        (while head
          (setq head (file-name-directory (expand-file-name head dname)))
          (if (file-readable-p head)
              (setq result (cons head result)))
          (setq head (car tail))
          (setq tail (cdr tail)))
      ;; Filter out relative filenames and unreadble directories
      ;; from the search path.
      (while head
        (if (and (not (char-equal (aref head 0) ?.))
                 (file-readable-p head))
            (setq result (cons (file-name-directory head) result)))
        (setq head (car tail))
        (setq tail (cdr tail))))
    ;; Return the list in the initial order.
    (nreverse result)))



(defun alb-LaTeX-private-tex-files (type re-extn strip)
  "Retrieve the list of filenames with the indicated extensions using
`TeX-master' to interpret relative directory links.  The private search
path is retrieved by `alb-LaTeX-private-tex-search-path'.  The listed
files are stripped of their extension unless STRIP evaluates to `nil'.

TYPE is the type of search, either \"tex\" or \"bib\".

RE-EXTN is a regular expression matching the extension without the full
stop, e.g., \"tex\", \"bib\", or \"epsi?\".

This function customises AUCTeX."
  (let* ((regxp (concat "^\\(.*\\)\\.\\(" re-extn "\\)$"))
         (files (apply 'append (mapcar (lambda (dir)
                                         (directory-files dir nil regxp))
                                       (alb-LaTeX-private-tex-search-path
                                        type TeX-master)))))
    (if strip
        ;; Return the filenames stripped of extensions and sorted.
        (sort (mapcar (lambda (fname)
                        (string-match regxp fname)
                        (substring fname (match-beginning 1) (match-end 1)))
                      files)
              'string-lessp)
      ;; Return the filenames sorted.
      (sort files
            'string-lessp))))



(defun alb-LaTeX-read-latex-file (optional &optional prompt)
  "Read the basename of a private LaTeX file, using `TeX-master' to
interpret relative directory links.  The private search path is
retrieved by `alb-LaTeX-private-tex-search-path'.

If OPTIONAL, modify the prompt, and allow a blank value.

If PROMPT is non-nil, use it as the prompt.  Otherwise, use a default.

This function customises AUCTeX."
  (completing-read (TeX-argument-prompt optional prompt "TeX file")
                   (mapcar 'list
                           (alb-LaTeX-private-tex-files
                            "tex" TeX-default-extension t))
                   nil
                   t))



(defun alb-LaTeX-read-bibtex-file (optional &optional prompt)
  "Read the basename of a private bibtex file, using `TeX-master' to
interpret relative directory links.  The private search path is
retrieved by `alb-LaTeX-private-tex-search-path'.

If OPTIONAL, modify the prompt, and allow a blank value.

If PROMPT is non-nil, use it as the prompt.  Otherwise, use a default.

This function customises AUCTeX."
  (let ((re-extn (mapconcat 'identity BibTeX-file-extensions "\\|")))
    (completing-read (TeX-argument-prompt optional prompt "BibTeX database")
                     (mapcar 'list
                             (alb-LaTeX-private-tex-files "bib" re-extn t))
                     nil
                     t)))



(defun alb-LaTeX-read-graphic-file (optional &optional prompt)
  "Read the basename of a private graphic file, using `TeX-master' to
interpret relative directory links.  The private search path is
retrieved by `alb-LaTeX-private-tex-search-path'.

If OPTIONAL, modify the prompt, and allow a blank value.

If PROMPT is non-nil, use it as the prompt.  Otherwise, use a default.

This function customises AUCTeX."
  (let ((re-extn (mapconcat 'identity alb-LaTeX-graphic-extensions "\\|")))
    (completing-read (TeX-argument-prompt optional prompt "Graphic file")
                     (mapcar 'list
                             (alb-LaTeX-private-tex-files "tex" re-extn nil))
                     nil
                     t)))



(defun alb-TeX-arg-alignment-point (optional &optional prompt)
  "Prompt for and insert a reference point argument as used in the
\\psfrag command.

If OPTIONAL, only insert if not empty, and then use square brackets.

If PROMPT is non-nil, use it as the prompt.  Otherwise,
`alb-LaTeX-read-alignment-point' supplies a default.

This function customises AUCTeX."
  (TeX-argument-insert (alb-LaTeX-read-alignment-point optional prompt)
                       optional))



(defun alb-TeX-arg-keyvalues (optional &optional prompt symbol)
  "Prompt for and insert a possibly empty sequence of graphicx style
keyvalue pairs.

If OPTIONAL, only insert if not empty, and then use square brackets.

If PROMPT is non-nil, use it to prefix the prompts for the key and value
parts.  The remainder of the prompt is derived from KEYVALUES.
Otherwise, `alb-LaTeX-read-keyvalue' supplies a default.

If SYMBOL is a non-nil symbol, use it as a pointer to the list of legal
key-value pairings.  Otherwise, `alb-LaTeX-read-keyvalue' supplies a
default.

This function customises AUCTeX."
  (let* ((keyvalues (if (symbolp symbol) (eval symbol)))
         (pair (alb-LaTeX-read-keyvalue optional prompt keyvalues)))
    (cond (pair
           (insert (if optional "[]" "{}"))
           (backward-char 1)
           (insert (car pair) "=" (cadr pair))
           (while (setq pair
                        (alb-LaTeX-read-keyvalue optional prompt keyvalues))
             (insert ", " (car pair) "=" (cadr pair)))
           (forward-char 1))
          ((and (null pair) (not optional))
           (insert "{}")))))



(defun alb-TeX-arg-latex-file (optional &optional prompt)
  "Prompt for and insert a latex file from the private search path.

If OPTIONAL, only insert if not empty, and then use square brackets.

If PROMPT is non-nil, use it as the prompt.  Otherwise,
`alb-LaTeX-read-latex-file' supplies a default.

This function customises AUCTeX."
  (TeX-argument-insert (alb-LaTeX-read-latex-file optional prompt)
                       optional))



(defun alb-TeX-arg-bibtex-file (optional &optional prompt)
  "Prompt for and insert a bibtex database from the private search path.

If OPTIONAL, only insert if not empty, and then use square brackets.

If PROMPT is non-nil, use it as the prompt.  Otherwise,
`alb-LaTeX-read-bibtex-file' supplies a default.

This function customises AUCTeX."
  (TeX-argument-insert (alb-LaTeX-read-bibtex-file optional prompt)
                       optional))



(defun alb-TeX-arg-graphic-file (optional &optional prompt)
  "Prompt for and insert a graphic file from the private search path.

If OPTIONAL, only insert if not empty, and then use square brackets.

If PROMPT is non-nil, use it as the prompt.  Otherwise,
`alb-LaTeX-read-graphic-file' supplies a default.

This function customises AUCTeX."
  (TeX-argument-insert (alb-LaTeX-read-graphic-file optional prompt)
                       optional))



(defun alb-TeX-arg-ref (optional type)
  "Prompt for and insert an existing label using the machinery of
RefTeX.

OPTIONAL is ignored (required by AUCTeX).
TYPE is a string containing the type of reference as understood by
'reftex-reference'.

This function customises AUCTeX.  It is called immediately after
inserting the macro name.  See documentation for `TeX-add-symbols'."
  (TeX-argument-insert (reftex-reference type t) nil))



;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
