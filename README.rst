===============
README.AlbLaTeX
===============

:Precis: /albcorp/ LaTeX packages with supporting AUCTeX extensions
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2000-2010, 2012-2013 Andrew Lincoln Burrow
:Version: 0.9.1

--------
Overview
--------

*AlbLaTeX* is software to support technical writing in mathematics and
computer science.  It comprises LaTeX, Emacs lisp, and Makefile source
code.  The LaTeX component is a small collection of classes and styles
files to format theorems, proofs, and algorithms, and to layout floating
elements and documents.  The three document classes address documenting
LaTeX, keeping notes on research reading, and producing book length
monographs.  The Emacs lisp component extends AUCTeX and RefTeX to
support the editing of these document elements and types.  The Makefile
component provides dependency driven building of these documents the
case of a large complex monograph, and for the case of a series of
documents.  The software is accompanied by documentation in the form of
a small reference manual for each LaTeX class and style file that
includes notes on related features of the Emacs lisp and Makefile
components.

Status
======

The software has been in use for many years, but there are some
eccentricities in the documentation and installation that arise from its
development for personal use.  I hope to correct these over time.  I
expect to use the issue tracker.

------------
Installation
------------

To install *AlbLaTeX* simply place the contents of the repository in a
convenient location.  The source code is installed once LaTeX can find
the class and style files, Emacs can locate the emacs lisp files, and
AUXTeX can locate the style files.  These cases are covered in the
subsection on minimal installation.  Further configuration is suggested
in the subsection on additional configuration.

Minimal installation
====================

To install *AlbLaTeX* simply place the contents of the repository in a
convenient location.  The source code is installed once LaTeX can find
the class and style files, Emacs can locate the emacs lisp files, and
AUXTeX can locate the style files.  The repository contains helper
scripts to achieve these ends.

``alb-latex-profile.sh``
  This bourne shell script can be sourced from your login script.  It
  updates the environment variables ``TEXINPUTS`` and ``INDEXSTYLE`` so
  that LaTeX can locate the class and style files.

  One approach is to place the repository under the directory
  ``~/Config`` and add the following fragment to a login script such as
  ``~/.bash_profile``::

      # Set LaTeX and MakeIndex to search the working directory and the
      # standard directories
      export TEXINPUTS=".:"
      export INDEXSTYLE=".:"

      # Run the personal profile scripts.  These enact various systems
      # pulled in as repositories under the "${HOME}/Config" directory
      for i in ${HOME}/Config/*/*-profile.sh ; do
          if [ -r $i ]; then
              source $i
          fi
      done
      unset i

  Change the filename ``${HOME}/Config`` to suit your personal
  directory structure.

``alb-latex-start.el``
  This emacs lisp code can be sourced from your personal emacs init
  file.  It updates the load path, and AUCTeX style path.

  Again, one approach is to place the repository under the directory
  ``~/Config`` and add the following fragment to ``~/.emacs``::

      ;;
      ;;
      ;; SCRIPTED CONFIGURATION
      ;; ---------------------------------------------------------------
      ;;

      (mapcar 'load
              (file-expand-wildcards "~/Config/*/*-start.el" t))

   Change the filename ``${HOME}/Config`` to suit your personal
   directory structure.

Additional configuration
========================

The above installation steps are the least obtrusive to make the
software available within LaTeX and Emacs.  However, there are steps you
may wish to take to enjoy additional features.  Some of these are noted
below as code snippets to add to ``~.emacs`` or set using the customize
tools in emacs.

- Automate construction for section, figure, table, and equation
  labels::

      (setq-default reftex-insert-label-flags
                    '("sfte" "ft"))

- Include the file basename in the section, figure, table, and
  equation labels::

    (setq-default reftex-label-alist
                  '((nil ?s "%S%f:")
                    (nil ?f "fig:%f:")
                    (nil ?t "tab:%f:")
                    (nil ?e "eq:%f:")))

--------------
LaTeX packages
--------------

The collection of LaTeX style files and classes comprise a small set of
document layouts for technical writing.  These are supplemented by
macros for formatting mathematics and computer algorithms.

`alb-latex.cls`
  Document layout for documentation of the /albcorp/ LaTeX packages

  See ``alb-latex.pdf`` for further information.

`alb-journal.cls`
  LaTeX book design for a research journal

  See ``alb-journal.pdf`` for further information.

`alb-thesis.cls`
  LaTeX book design for a thesis

  See ``alb-thesis.pdf`` for further information.

`alb-float-tools.sty`
  LaTeX style file for typesetting float material and captions

  Provides environments and commands for typesetting float material and
  captions.  These include: an environment to typeset float material
  across the page including the marginpar allocation; a list environment
  to flow subcaptioned items across the available width; and commands to
  select the caption style according to the page on which a float falls.
  See ``alb-float-tools.pdf`` for further information.

`alb-order-theory.sty`
  Provides a collection of macros to make set and order theoretic
  mathematical constructs more readable and consistent in their
  formatting.  See ``alb-order-theory.pdf`` for further
  information.

`alb-graph-theory.sty`
  Provides a collection of macros to make graph theoretic mathematical
  constructs more readable and consistent in their formatting.  The
  collection is biased toward directed graphs.  See
  ``alb-graph-theory.pdf`` for further information.

`alb-theorems.sty`
  Provides a minimal collection of theorem type environments.  See
  ``alb-theorems.pdf`` for further information.

`alb-algorithms.sty`
  Provides environments and commands for typesetting algorithms and
  floats containing algorithms, including commands to define identifiers
  for accessor functions and procedures.  See
  ``alb-algorithms.pdf`` for further information.

`alb-proofs.sty`
  Provides the `albProof` environment which implements Lamport's
  structured proofs described in the paper:

    Leslie Lamport, *How to Write a Proof*, 1993.

  See ``alb-proofs.pdf`` for further information.

`alb-avm.sty`
  Provides environments and commands to typeset typed feature structures
  and inequated typed feature structures in attribute-value matrix (AVM)
  notation.  See ``alb-avm.pdf`` for further information.

------------------
Emacs lisp modules
------------------

The collection of emacs lisp modules support the LaTeX style files and
classes by exending AUCTeX and RefTeX and providing the following
additional capabilities.

- Generate file names from sentences
- Manipulate local variables lists
- Associate AUCTeX, and RefTeX with LaTeX source code
- Extend AUCTeX and RefTeX to ease the management of large projects
  including functions to create skeleton files from descriptive
  sentences, improve the lookup of filenames, and automate the use of
  labels.
- Extend AUCTeX to parse LaTeX via a simplified yet extensible parser
  with few additional restrictions on the form of LaTeX code.

-------------------
Makefiles for LaTeX
-------------------

The supplied makefiles automate the application of pdflatex, bibtex, and
makeindex to a complex LaTeX document, and to a collection of LaTeX
documents.  Both makefiles use detailed dependency information gathered
by parsing source files.  They are parameterised by a single variable
specifying the master LaTeX files.

``make\monograph\Makefile``
  A makefile parameterised by the make variable ``TEX_MASTER`` to accept
  a single document.  The directory also contains example LaTeX source
  code to demonstrate the features.

``make\serial\Makefile``
  A makefile parameterised by the make variable ``TEX_MASTERS`` to
  accept a series of documents.  The directory also contains example
  LaTeX source code to demonstrate the features.

.. Local Variables:
.. mode: rst
.. ispell-local-dictionary: "british"
.. End:
