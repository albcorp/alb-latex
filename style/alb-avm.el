;;;
;;; style/alb-avm.el
;;;
;;;   - Customise AUCTeX to provide guided recursive construction of typed
;;;     feature structures in attribute-value matrix (AVM) notation.
;;;
;;; $Id$
;;;
;;; $Log$
;;; Revision 1.1  1999/04/15 04:57:22  andrew
;;; Initial revision
;;;



;;; *** VARIABLE DECLARATIONS *************************************************


(defvar alb-LaTeX-avmType-history
  '()
  "Global history list of type labels entered during automated
construction of \"albAvm\" environments.")



(defvar alb-LaTeX-avmFeat-history
  '()
  "Global history list of feature labels entered during automated
construction of \"albAvm\" environments.")



;;; *** ACTIONS ***************************************************************


;;   + Gang AUCTeX style hook to import new environment.  Macros are not
;;     declared explcitly, but rather handled through the
;;     "alb-LaTeX-env-avm" function.

(TeX-add-style-hook "alb-avm"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("albAvm" alb-LaTeX-env-avm)))))



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-LaTeX-env-avm (environment)
  "Insert an AVM notation environment for a typed feature structure.
Recursively prompt the user for the substructures.  This function
customises AUCTeX."
  (LaTeX-insert-environment environment)
  (let ((tags (alb-LaTeX-sym-avm-body "" '())))
    (if (not (equal tags '()))
	(let ((first-lhs (read-from-minibuffer "First LHS tag: "
					       nil nil nil 'tags))
	      (first-rhs (read-from-minibuffer "First RHS tag: "
					       nil nil nil 'tags)))
	  (if (and (not (string-equal first-lhs ""))
		   (not (string-equal first-rhs "")))
	      (progn (insert "\\albAvmIneqtn{" first-lhs "}{" first-rhs "}")
		     (alb-LaTeX-sym-avmIneqtn tags)))))))



(defun alb-LaTeX-sym-avm-body (path tags)
  "Recursively insert AVM notation macros for a typed feature structure.
The recursion is controlled by user input.  PATH is the explicitly
represented path to the current substructure.  It is used to prompt the
user.  TAGS is the list of substructure tags seen so far.  It is used to
control the insertion of tag and reference macros.  This function
customises AUCTeX."
  (let ((tag (read-from-minibuffer (concat path "tag: "))))
    (if (not (string-equal tag ""))
	(if (member tag tags)
	    (alb-LaTeX-sym-avmRef tag tags)
	  (alb-LaTeX-sym-avmTag path tag tags))
      (alb-LaTeX-sym-avmType path tags))))



(defun alb-LaTeX-sym-avm-enter (tags)
  "Enter the AVM notation scope of the preceeding \"albAvmTag\" or
\"albAvmFeat\" macro.  TAGS is passed up without change.  This function
customises AUCTeX."
  (search-backward "}" nil t)
  (insert ?%)
  (newline-and-indent)
  tags)



(defun alb-LaTeX-sym-avm-leave (tags)
  "Leave the AVM notation scope of the enclosing \"albAvmTag\" or
\"albAvmFeat\" macro.  TAGS is passed up without change.  This function
customises AUCTeX."
  (search-forward "}" nil t)
  (newline-and-indent)
  tags)



(defun alb-LaTeX-sym-avmTag (path tag tags)
  "Insert AVM notation for the feature structure tag.  Recurse into the
feature structure.  PATH is the explicitly represented path to the
current substructure.  TAG is the label to be introduced at the current
substructure.  TAGS is the list of substructure tags seen so far.  It is
used to control the insertion of tag and reference macros.  This
function customises AUCTeX."
  (alb-LaTeX-sym-avm-leave
   (alb-LaTeX-sym-avmType
    path
    (alb-LaTeX-sym-avm-enter (and (null (insert "\\albAvmTag{" tag "}{}"))
				 (cons tag tags))))))



(defun alb-LaTeX-sym-avmRef (tag tags)
  "Insert AVM notation macro for a reference to a typed feature
structure.  TAG is used as the label on the reference.  This function
customises AUCTeX."
  (insert "\\albAvmRef{" tag "}")
  (newline-and-indent)
  tags)



(defun alb-LaTeX-sym-avmType (path tags)
  "Insert AVM notation macro for the feature structure type.  Recurse
into the list of features.  PATH is the explicitly represented path to
the current substructure.  TAGS is the list of substructure tags seen so
far.  It is used to control the insertion of tag and reference macros.
This function customises AUCTeX."
  (let ((type (read-from-minibuffer (concat path "type: ")
				    nil nil nil 'alb-LaTeX-avmType-history)))
    (if (not (string-equal type ""))
	(progn (insert "\\albAvmType{" type "}")
	       (newline-and-indent)
	       (alb-LaTeX-sym-avmFeat path tags))
      (progn (insert "\\albAvmType{universal}")
	     (newline-and-indent)
	     tags))))



(defun alb-LaTeX-sym-avmFeat (path tags)
  "Insert AVM notation macro for a feature value.  Recurse into the
feature value and then into the list of sibling features.  PATH is the
explicitly represented path to the current substructure.  TAGS is the
list of substructure tags seen so far.  It is used to control the
insertion of tag and reference macros.  This function customises
AUCTeX."
  (let ((feat (read-from-minibuffer (concat path "feat: ")
				    nil nil nil 'alb-LaTeX-avmFeat-history)))
    (if (not (string-equal feat ""))
	(progn (alb-LaTeX-sym-avmFeat
		path
		(alb-LaTeX-sym-avm-leave
		 (alb-LaTeX-sym-avm-body
		  (concat path feat " ")
		  (alb-LaTeX-sym-avm-enter
		   (and (null (insert "\\albAvmFeat{" feat "}{}"))
			tags))))))
      (progn tags))))



(defun alb-LaTeX-sym-avmIneqtn (tags)
  "Insert AVM notation macros for the inequations in an inequated typed
feature structure.  Recurse into the list of inequations.  TAGS is the
list of tagged substructures.  It is used as a history list.  This
function customises AUCTeX."
  (let ((next-lhs (read-from-minibuffer "Next LHS tag: "
					nil nil nil 'tags))
	(next-rhs (read-from-minibuffer "Next RHS tag: "
					nil nil nil 'tags)))
    (if (and (not (string-equal next-lhs ""))
	     (not (string-equal next-rhs "")))
	(progn (insert ", \\albAvmIneqtn{" next-lhs "}{" next-rhs "}")
	       (alb-LaTeX-sym-avmIneqtn tags))
      (progn (newline-and-indent)))))
