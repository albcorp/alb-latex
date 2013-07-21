================================
Style Guide for reStructuredText
================================

:Precis: How to use reStructuredText to document software
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2006, 2012 Andrew Lincoln Burrow
:License:
    Permission is granted to copy, distribute and/or modify this
    document under the terms of the GNU Free Documentation License,
    Version 1.3 or any later version published by the Free Software
    Foundation; with no Invariant Sections, no Front-Cover Texts, and no
    Back-Cover Texts.

    The latest version of this license is in
    http://www.gnu.org/copyleft/fdl.html

---------
XXX To Do
---------

- Match up structure between style guides and look for missing sections

- Talk about achieving homogenous lists in reStructuredText

  + Either make all elements in a list paragraph free, or make all
    elements contain paragraphs
  + Note how this is achieved

------------
Introduction
------------

This document is a template for documentation written in
reStructuredText.  It is both an example of the most common markup, and
a discussion of writing software documentation.  It includes suggestions
for the markup for sections, identifiers, literals, and example code,
and relates these suggestions to the generation of software
documentation.

The best reference for the markup rules is `Quick reStructuredText`_.
Also of use is the `reStructuredText homepage`_.

.. _Quick reStructuredText: \
   http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. _reStructuredText homepage: \
   http://docutils.sourceforge.net/rst.html

-----------------------------
Document Structure and Markup
-----------------------------

This section describes the suggested formats for headings, and the
initial text of the document.

Suggested Fields in the Opening Text
====================================

The opening text of the documentation should set out the copyright of
the file and briefly summarise its contents.  Therefore, the following
structure is suggested.

- Begin each file with a top level heading.  The text of this heading is
  the title of the document.  The suggested markup is described in the
  section.

- Follow the title with a field list.  The purpose is to provide a
  one-line summary, a contact point, and an assertion of the authorship
  and copyright.  See the top of the document for an example.

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

The following hierarchy of heading patterns is suggested.  Notice the
logical progression from double to single underlining, and from over and
underlining to just underlining.

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

Below subsubsections, use other strategies like description lists.  The
above list is an example of such markup.

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

Referring to Implementation
===========================

In documenting a piece of software, it will be necessary to refer to the
implementation.  If it is presumed that a script exists that parses code
to recover the main structure and accompanying inline documentation, how
does one make references to this documentation?

Manifest of Files
-----------------

The simplest solution is to refer to the documentation for a module as a
monolithic object.  This is most easily achieved by setting up a section
in the documentation that lists a manifest of files.

The following example shows how a manifest is marked up for two
documentation extracted from two python files ``ClassOne.py`` and
``ClassTwo.py``.  A named *external hyperlink* is recorded for each
implementation file, so that the document can easily refer to the
implementing file.

``ClassOne.py``
    Widgets for use in widget washing.

    See the documentation in `ClassOne`_

``ClassTwo.py``
    Additional widgets for use in widget washing.

    See the documentation in `ClassTwo`_

.. _ClassOne: ClassOne.html
.. _ClassTwo: ClassTwo.html

Location of Documentation
-------------------------

In case the code is parsed for documentation, this document is the head
of the documentation.  In particular, architectural overviews are
documented here.  In contrast, implementation details are documented in
the implementation files, especially when other implementations would be
consistent with the overall architecture.

Documentation must be place in comments and docstrings according to the
language of the source code.  The placement of documentation within
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
    should go in the file, class, and function docstrings.  There are no
    restrictions on the syntax within a docstring that are likely to
    effect reStructuredText.

    For example, this is the docstring of a function::

        Sets the score from one to five of the comment

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

        function siaResizeEditor(event) {
            // Resize `sia-edit-pane` iframe in response to event
            //
            // Retrieve `sia-edit-pane` element and compute height from window
            // height and height of header.  Does not work in IE.  See:
            // http://www.quirksmode.org/viewport/compatibility.html

            // Get the editor object and reset its height
            var editor = document.getElementById('sia-edit-pane');
            editor.height = ( self.innerHeight - editor.offsetTop );
        }

    is parsed to::

        ==============
        Event Handlers
        ==============

        ----------------------
        siaResizeEditor(event)
        ----------------------

        Resize `sia-edit-pane` iframe in response to event

        Retrieve `sia-edit-pane` element and compute height from window
        height and height of header.  Does not work in IE.  See:
        http://www.quirksmode.org/viewport/compatibility.html

    note that the last comment is dropped.

.. Local Variables:
.. mode: rst
.. ispell-local-dictionary: "british"
.. End:
