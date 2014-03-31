================================
Style Guide for reStructuredText
================================

:Precis: How to use reStructuredText to document software
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2006, 2012, 2013, 2014 Andrew Lincoln Burrow
:License:
    Permission is granted to copy, distribute and/or modify this
    document under the terms of the GNU Free Documentation License,
    Version 1.3 or any later version published by the Free Software
    Foundation; with no Invariant Sections, no Front-Cover Texts, and no
    Back-Cover Texts.

    The latest version of this license is in
    http://www.gnu.org/copyleft/fdl.html

------------
Introduction
------------

The following style guide covers the use of reStructuredText to document
source code.  It covers the usage of reStructuredText for document
markup, how to refer to source code within a reStructuredText document,
and how to use reStructuredText to markup documentation within the
source code.  It is not a general writing style guide.  To simplify the
language it is written in the imperative as if instructing the user.

The best reference for the markup rules is `Quick reStructuredText`_.
Also of use is the `reStructuredText homepage`_.

.. _Quick reStructuredText:
   http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. _reStructuredText homepage:
   http://docutils.sourceforge.net/rst.html

---------------
Document Markup
---------------

How to markup the document title.  How to markup section headings.  How
to use reStructuredText lists.  How to insert a figure.  How to markup
tabular material.

Document Title
==============

Begin each file with a top level heading.  The text of this heading is
the title of the document.  The suggested markup for this heading is
described in the following section.  Follow the title with a field list.
The purpose is to provide a one-line summary, a contact point, and an
assertion of the authorship and copyright.  See the top of the document
for an example.

Section Markup
==============

reStructuredText parses a variety of patterns as headings, and allocates
a heading level based on the first occurrence of the pattern.  If the
intention is to mix reStructuredText from a number of sources, it is
important to fix these patterns.

A pattern of lines is recognised as a heading if it comprises a single
line of text either underlined, or overlined and underlined by repeats
of a single character section.  The heading above is an example.  The
patterns are allocated a heading level according to their occurrence in
the document.

Use the the hierarchy of heading patterns.  Notice the logical
progression from double to single underlining, and from over and
underlining to just underlining.  Configure the emacs ``rst-mode`` to
use this hierarchy, as it provides helpful commands to reformat section
headings.

**Document Title**
    At the start of the file markup the document title in the following
    manner::

        ==============
        Document Title
        ==============

    If the document contains chapter level headings, then it is possible
    to squeeze another level above.  Use the ``+`` character in place of
    the ``=`` character, and then use this pattern for the chapter level
    markup.

**Section Level**
    Divide the document into sections, and markup the section titles in
    the following manner::

        -------------
        Section Level
        -------------

    When marking up docstrings for classes, presume that the class
    docstring has been marked up to this level.  That is, use only
    heading levels below section in the class docstring.

**Subsection Level**
    Divide sections into subsections, and markup the subsection titles
    in the following manner::

        Subsection Level
        ================

    When marking up docstrings for functions, presume that the function
    docstring has been marked up to this level.  That is, use only
    heading levels below subsection in the function docstring.

**Subsubsection Level**
    If required, divide subsections into subsubsections, and markup the
    subsubsection titles in the following manner::

        Subsubsection Level
        -------------------

    Given the above, this is the heading level for sections within a
    function docstring.  In particular, the *Contract* section of the
    function docstring should use this pattern.

**Paragraph Level**
    If required, divide subsubsections into paragraphs and markup the
    paragraph titles in the following manner::

        Paragraph Level
        :::::::::::::::

    Note that reStructuredText does not allow the use of out-of-place
    heading levels.  For example, it is not possible to put a paragraph
    level directly under a section level.  At this low level of
    sectioning, this often works against the document intent, e.g.,
    where the paragraph heading is used to introduce an example.  One
    solution is to use description lists instead for these structures.

**Paragraph Level**

    Finally, if required, divide paragraphs into subparagraphs and
    markup the subparagraph titles in the following manner::

        Subparagraph Level
        ..................

    Note the comments for paragraphs above.

Bulleted Lists
==============

Create bulleted lists using the characters ``-``, ``+``, and ``*`` as
bullets.  Nest bulleted lists no further than 4 deep.  The simplest way
to keep track of nesting depth is to us ``-`` for the first level, ``+``
for the second level, and ``*`` for the third and fourth level of
bulleted list.

- First outer element

  + First inner element
  + Second inner element

    * The first inner, inner element
    * The second inner, inner element

  + Third inner element

    Note that adding inner lists or paragraphs, simply requires that
    additional blank lines be introduced.

    * The third inner, inner element

      * First at 4 deep
      * Second at 4 deep

Enumerated Lists
================

Create enumerated lists with auto-numbering using ``#.`` as the bullet.
Nest enumerated lists no further than 4 deep.

#. This is a numbered element
#. Another numbered element

   #. This is the indented list element
   #. Another indented list element

      #. This is the further indented list element
      #. Another further indented list element

         #. This is the even further indented list element
         #. Another even further indented list element

Create enumerated lists with special numbering requirements using the
numbering as the bullet, for example, ``a.`` or ``iv)``.  Note that it
is not possible to create compound numbering formats like 2.3.

1. This is a numbered element
2. Another numbered element

   a. This is the indented list element
   b. Another indented list element

        i) This is the further indented list element
        ii) Another further indented list element

            I. This is the even further indented list
            II. This is another even further indented list

Definition Lists
================

Create definition lists by setting the term on the first line of a
paragraph, and indenting the remainder of the paragraph.

Lorem ipsum
   dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor
   tellus.

   Donec
      pretium posuere tellus.

   Cum
      sociis natoque penatibus et magnis dis parturient montes, nascetur
      ridiculus mus.

      - Nulla posuere.
      - Donec vitae dolor.
      - Nullam tristique diam non turpis.

Options Lists
=============

Create options lists when documenting command options by using the
option as a bullet and leaving at least two spaces to the explanatory
text.

-a            command-line option "a"
-b file       options can have arguments
              and long descriptions
--long        options can be long also
--input=file  long options can also have
              arguments
/V            DOS/VMS-style options too

              - Nulla posuere.
              - Donec vitae dolor.
              - Nullam tristique diam non turpis.

Figures
=======

Use the ``figure`` directive to insert figures.  The directive accepts
the filename of an image, and fields including the scaling.

There is no simple way to manage the cross references, because the LaTeX
figure numbers are not accessible from within reStructuredText.  Instead
label the figure with a label that matches the one made by LaTeX, and
use that to reference the figure.  For example, see `Figure 1`_.  This
will require manual renumbering, but this is unlikely to be a problem
for field notes.

.. _`Figure 1`:

.. figure:: figures/small-poset.pdf
            :scale: 75 %

            Example of a diagram in PDF format

Tables
======

Use one of the advanced table directive to insert tables.  The
``csv-table`` directive accepts data in CSV format, and fields including
the header row and the relative column widths.  See the example in
`Table 1`_.  The ``list-table`` directive accepts data as a nested
bulleted list, and fields including the relative column widths.  See the
example in `Table 2`_.

There is no simple way to manage cross references to tables, because the
LaTeX table numbers are not accessible from within reStructuredText.
Instead label the table to match LaTeX, and use that label to reference
the table.  This will require manual renumbering, but this is unlikely
to be a problem for field notes.

.. _`Table 1`:

.. csv-table:: Example of a table from CSV data
   :header: "Treat", "Quantity", "Description"
   :widths: 15, 10, 30

   "Albatross", 2.99, "On a stick!"
   "Crunchy Frog", 1.49, "If we took the bones out, it wouldn't be
   crunchy, now would it?"
   "Gannet Ripple", 1.99, "On a stick!"

.. _`Table 2`:

.. list-table:: Example of a table from list data
   :widths: 15 10 30
   :header-rows: 1

   -
     + Treat
     + Quantity
     + Description
   -
     + Albatross
     + 2.99
     + On a stick!
   -
     + Crunchy Frog
     + 1.49
     + If we took the bones out, it wouldn't be crunchy, now would it?
   -
     + Gannet Ripple
     + 1.99
     + On a stick!

Whitespace and Line Breaking
============================

Whitespace is used to improve the readability of the unformatted
document.  The goal of the style guide rules is to provide for automatic
and simple insertion of whitespace so that undue changes do not occur in
document.

Remove all trailing whitespace.  Convert all leading whitespace to
spaces, and indent each block by 4 spaces.  Place a single blank line
between paragraphs and headings.

Break all lines to fit a 72 character line.

-----------------
About Source Code
-----------------

How to markup examples of source code within a reStructuredText
document.

Inline Literals
===============

Inline fragments of source code are marked up using the inline literal
syntax.  For example, this is ``inline`` source code.

Literal Blocks
==============

A paragraph containing only two colons indicates that the following
indented or quoted text is a literal block.

::

  Whitespace, newlines, blank lines, and all kinds of markup (like
  *this* or \this) is preserved by literal blocks.

  The paragraph containing only '::' will be omitted from the result.

The ``::`` may be tacked onto the very end of any paragraph. The ``::``
will be omitted if it is preceded by whitespace.  The ``::`` will be
converted to a single colon if preceded by text, like this::

  It's very convenient to use this form.

Literal blocks end when text returns to the preceding paragraph's
indentation.  This means that something like this is possible::

      We start here
    and continue here
  and end here.

Formatting long lines of literal text can present problems.  Break
literal blocks to ensure that line length does not exceed 68 characters,
excluding the initial blanks.  For example, consider the following LaTeX
fragment::

    % 68 characters
    % 012345678901234567890123456789012345678901234567890123456789ABCDEF
    \begin{albPropositions}
    \item Let $\approx$ be the binary relation such that $x \approx y$
      if and only if $x \lhd y$ and $y \lhd x$.  Then, $\approx$ is an
      equivalence relation.

    \item Let $C = \{ \albEquivClass{x} \mid x \in P \}$, and let $\leq$
      be the binary relation such that $\albEquivClass{x} \leq
      \albEquivClass{y}$ if and only if $x \lhd y$.  Then, $\leq$ is a
      partial order.
    \end{albPropositions}

------------------
Within Source Code
------------------

How to use reStructuredText to markup document strings within the source
code.  See the specific style guides for additional guidance on using
reStructuredText within a particular language.

Docstrings
==========

Place documentation in comments and docstrings according to the language
of the source code.  The placement of documentation within
implementation files must satisfy several constraints.

1. Documentation must be consistent to be effectively rendered.
2. Documentation must place the precis, authors, copyright, and license
   are visible as early as possible in the file.
3. Documentation must fit within the syntax of the host file type.
4. Documentation must not appear out of context in the outputs of the
   system.

Four contexts are considered here: XML documents, Python code, CSS code,
and JavaScript code.

**XML Documents**
    XML documents carry comments with special comment nodes with the
    following syntax::

        <!-- This is a comment.
        It must not contain sequences of dashes -->

    The restriction of the use of dashes restricts the available section
    markup as described below.

    Where the XML document is a template, some care needs to be taken to
    ensure the comment does not appear in the output.  This is
    particularly the case, because the comments, copyrights, etc.
    properly apply to the page template and not the rendered page.

**Python Code**
    Python code carries documentation in docstrings, as well as
    comments.  Inline comments are an important tool, but documentation
    should go in the module, class, and function docstrings.  There are
    no restrictions on the syntax within a docstring that are likely to
    effect reStructuredText.

    For example, this is the docstring of a function::

        Set the score from one to five of the comment

        Sets the score of the comment to an integer between 1 and 5.

        Contract
        --------

        pre::
            self.isCommentId(comment_id)
            isinstance(score, int) and 1 <= score <= 5


**CSS Code**
    CSS code uses C style comments.  Write as if the documentation
    parser recovers the content of all comments that begin with a blank
    line, namely, the comment opening is immediately followed by a new
    line.

    For example::

        /*
         * This is documentation,
         */

        /* but this is not considered documentation */

**JavaScript Code**
    JavaScript allows C++ style comments, which are preferred, because
    they are easier to parse and can be safely nested.  Documentation
    should be placed in comments to mimic the locations of Python
    docstrings, namely at the head of the file at the start of class and
    function definitions.

    Write as if the documentation parser recovers the content of all
    comments that begin in the leftmost column, and the content of all
    comments immediately after a class or function definition opening.

    For example::

        // ==============
        // Event Handlers
        // ==============

        function albResizeEditor(event) {
            // Resize `alb-edit-pane` iframe in response to event
            //
            // Retrieve `alb-edit-pane` element and compute height from window
            // height and height of header.  Does not work in IE.  See:
            // http://www.quirksmode.org/viewport/compatibility.html

            // Get the editor object and reset its height
            var editor = document.getElementById('alb-edit-pane');
            editor.height = ( self.innerHeight - editor.offsetTop );
        }

    is parsed to::

        ==============
        Event Handlers
        ==============

        ----------------------
        albResizeEditor(event)
        ----------------------

        Resize `alb-edit-pane` iframe in response to event

        Retrieve `alb-edit-pane` element and compute height from window
        height and height of header.  Does not work in IE.  See:
        http://www.quirksmode.org/viewport/compatibility.html

    note that the last comment is dropped.

Inline Comments
===============

Inline comments are those that will not be picked up by a documentation
generator, but are never the less important to the legibility of code.
In this context, use reStructuredText sparingly to markup for emphasis
and to clarify references to filenames and functions.  Use emphasis for
the titles of external documents being referred to, use stromg emphasis
to highlight a point, and use interpretted text for references to
filenames, modules, classes, and functions.

.. Local Variables:
.. mode: rst
.. ispell-local-dictionary: "british"
.. End:
