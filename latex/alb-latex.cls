%%%
%%% alb-latex/latex/alb-latex.cls
%%%
%%%     Copyright (C) 2002-2006, 2013, 2017 Andrew Lincoln Burrow
%%%
%%%     This program may be distributed and/or modified under the
%%%     conditions of the LaTeX Project Public License, either version
%%%     1.3 of this license or (at your option) any later version.
%%%
%%%     The latest version of this license is in
%%%
%%%         http://www.latex-project.org/lppl.txt
%%%
%%%     and version 1.3 or later is part of all distributions of LaTeX
%%%     version 2005/12/01 or later.
%%%
%%%     This work has the LPPL maintenance status 'author-maintained'.
%%%
%%%     This work consists of the files 'alb-algorithms.sty',
%%%     'alb-avm.sty', 'alb-latex.cls', 'alb-float-tools.sty',
%%%     'alb-graph-theory.sty', 'alb-journal.cls',
%%%     'alb-order-theory.sty', 'alb-proofs.sty', 'alb-theorems.sty',
%%%     'alb-thesis.cls', 'alb-algorithms.tex', 'alb-avm.tex',
%%%     'alb-latex.tex', 'alb-float-tools.tex', 'alb-graph-theory.tex',
%%%     'alb-journal.tex', 'alb-order-theory.tex', 'alb-proofs.tex',
%%%     'alb-theorems.tex', 'alb-thesis.tex',
%%%     'alb-journal-glossary.ist', 'alb-journal-index.ist',
%%%     'alb-thesis-glossary.ist', and 'alb-thesis-index.ist'.
%%%
%%%   - Set up the page layout, section formatting, and title page for
%%%     consistency across documentation for AlbLaTeX.  Provide a
%%%     titlepage environment with markup  command, commands for
%%%     typesetting LaTeX commands, and a collection of logo commands.
%%%
%%%   - See: 'alb-latex.pdf'.
%%%



\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{alb-latex}



%
% No options recognised.

\ProcessOptions


%
% Load base class.

\LoadClassWithOptions{article}


%
% Load required packages.

% Require an \ifthenelse{}{}{} command to tailor definitions for the
% class.
% See: 'ifthen.dvi'.
\RequirePackage{ifthen}

% Use the 'calc' package to simplify length expressions.
% See: 'calc.dvi'.
\RequirePackage{calc}

% Use the 'geometry' package to setup the page layout.
% See: 'geometry.dvi'.
\RequirePackage{geometry}

% Use the 'fancyhdr' package to setup the headers.
% See: 'fancyhdr.dvi'.
\RequirePackage{fancyhdr}



\makeatletter



%
% Page layout.
%
% The page is laid out according to the document class options, and the
% geometry set here.  The geometry must include a marginpar allocation,
% i.e., 'includemp' must be true.  The geometry must reverse the
% margins, i.e., 'reversemp' must be set.  Change the booleans in the
% document class options, i.e., select 'oneside' or 'twoside', and
% 'titlepage' or 'notitlepage'.

\geometry{%
  verbose, dvips,
  width=170mm, height=260mm, top=12mm,
  includehead, headheight=24pt, headsep=30pt, nofoot,
  includemp, reversemp, marginparwidth=25mm, marginparsep=6mm%
}


%
% Headers and footers.
%
% See: 'fancyhdr.dvi'.

\pagestyle{fancy}

% Place the headers and footers across the entire printable page.
\setlength{\headwidth}{\textwidth + \marginparsep + \marginparwidth}

% Capture the document title.
\newcommand{\alb@CurrentTitle}[0]{}

% Capture the section titles.
\newcommand{\alb@CurrentSection}[0]{}
\newcommand{\alb@CurrentSectionTitle}[0]{}
\renewcommand{\sectionmark}[1]{%
  \renewcommand{\alb@CurrentSection}{Section~\thesection}%
  \renewcommand{\alb@CurrentSectionTitle}{#1}%
}

% Define a plain style with empty headers and footers.
\fancypagestyle{plain}{%
  \fancyhf{}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{0pt}
}

% Define a style with document and section title on all pages, and with
% no footer.
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
  \fancyhead[LE]{\normalfont\sffamily\itshape\thepage}%
  \fancyhead[RE]{\normalfont\sffamily\itshape\alb@CurrentTitle}%
  \fancyhead[RO]{\normalfont\sffamily\itshape\thepage}%
  \fancyhead[LO]{\normalfont\sffamily\itshape\alb@CurrentSectionTitle}%
}{% ELSE ... oneside layout
  \fancyhead[L]{\normalfont\sffamily\itshape%
    \begin{minipage}[t]{0.5\headwidth}%
      \alb@CurrentTitle{}
    \end{minipage}%
  }
  \fancyhead[R]{\normalfont\sffamily\itshape%%
    \begin{minipage}[t]{0.475\headwidth}%
      \begin{flushright}%
        \alb@CurrentSectionTitle%
      \end{flushright}%
    \end{minipage}%
    \begin{minipage}[t]{3em}%
      \begin{flushright}%
        \thepage{}%
      \end{flushright}%
    \end{minipage}%
  }%
}


%
% Section heading formats.
%
% There are non-trivial tricks in the definitions of section heading
% styles using the @startsection macro.  The descriptions of the
% mandatory fields are therefore given here:
%   name        The name of the user level command.
%               E.g., `section'.
%   level       A number, denoting the depth of the section.
%               E.g., chapter=1, section = 2.
%   indent      The indentation of the heading from the left margin.
%   beforeskip  The absolute value gives the skip to leave above the
%               heading.  If the value is negative, then the paragraph
%               indent of the text following the heading is suppressed.
%   afterskip   If positive, this gives the skip to leave below the
%               heading, else it gives the skip to leave to the right of
%               a run-in heading.
%   style       Commands to set the style of the heading.

% Add section numbering down to subsubsections.
\setcounter{secnumdepth}{2}

% Sections headings are: sans-serif, bold, Large, upper/lowercase.
\renewcommand{\section}{\@startsection%
  {section}% Name
  {1}% Level
  {0mm}% Indent
  {-2\baselineskip}% Before skip
  {\baselineskip}% After skip
  {\normalfont\sffamily\bfseries\Large}% Style
}

% Subsections headings are: sans-serif, bold, large, upper/lowercase.
\renewcommand{\subsection}{\@startsection%
  {subsection}% Name
  {2}% Level
  {0mm}% Indent
  {-\baselineskip}% Before skip
  {0.5\baselineskip}% After skip
  {\normalfont\sffamily\bfseries\large}% Style
}

% Subsubsections are: sans-serif, bold, upper/lowercase.
\renewcommand{\subsubsection}{\@startsection%
  {subsubsection}% Name
  {3}% Level
  {0mm}% Indent
  {-\baselineskip}% Before skip
  {0.25\baselineskip}% After skip
  {\normalfont\sffamily\bfseries}% Style
}


%
% \albTitle command.
%
% Capture the title text in the macro \alb@CurrentTitle.
% This is used by the headers and footers, and the 'albTitlePage'
% document.

\newcommand{\albTitle}[1]{%
  \renewcommand{\alb@CurrentTitle}{#1}%
}


%
% 'albTitlePage' environment.
%
% Typeset a title page using the title string as recorded by \albTitle.
% The titlepage environment should be internally sectioned using the
% \albTitlePageSection{} command.

% The 'alb@TitlePage' environment formats the content of the
% 'albTitlePage' environment.
\newenvironment{alb@TitlePage}[0]{%
  % Flow the title across the margin allocation.  Assume that the
  % title occurs on an odd page.
  \begin{list}{}{%
      \setlength{\topsep}{0pt}%
      \setlength{\leftmargin}{0pt - \marginparsep - \marginparwidth}%
      \setlength{\listparindent}{\parindent}%
      \setlength{\itemindent}{0pt}%
      \setlength{\parsep}{\parskip}%
    }%
  \item {\sffamily\bfseries\LARGE \alb@CurrentTitle}%
  \end{list}%
  %
  % Start a list for the title page sections.
  \begin{list}{}{%
      \setlength{\leftmargin}{20mm}%
      \setlength{\topsep}{9mm}%
      \setlength{\itemsep}{9mm}%
      \setlength{\parsep}{0mm}%
    }%
}{%
  \end{list}%
}

% The 'albTitlePage' environment wraps the 'alb@TitlePage' environment
% according to whether the title information should appear on a separate
% page.
\ifthenelse{\boolean{@titlepage}}{% THEN ... use titlepage environment
  \newenvironment{albTitlePage}[0]{%
    \begin{titlepage}%
      \begin{alb@TitlePage}%
  }{%
      \end{alb@TitlePage}%
    \end{titlepage}%
  }
}{% ELSE ... elide titlepage environment
  \newenvironment{albTitlePage}[0]{%
    \thispagestyle{empty}%
    \begin{alb@TitlePage}%
  }{%
    \end{alb@TitlePage}%
  }
}


%
% \albTitlePageSection command.
%
% Command to section the title page information.
% Must be used inside the 'albTitlePage' environment.

\newcommand{\albTitlePageSection}[1]{%
  \item \textsf{\textbf{#1}}
}


%
% \albLaTeXCmd, \albLtxEnv, \albLtxOpt, \albLtxArg, \albLtxPrm commands.
%
% Commands to typeset LaTeX command and environment usage.

\newcommand{\albLtxCmd}[1]{%
  $\backslash\mathtt{#1}$%
}
\newcommand{\albLtxEnv}[1]{%
  $\backslash\mathtt{begin}\{\mathtt{#1}\}$%
}
\newcommand{\albLtxOpt}[1]{%
  $[\mathit{#1}]$%
}
\newcommand{\albLtxArg}[1]{%
  $\{\mathit{#1}\}$%
}
\newcommand{\albLtxPrm}[1]{%
  $\mathit{#1}$%
}


%
% \PDFLaTeX, \AUCTeX, \RefTeX, \albLogo commands.
%
% Typeset versions of PDFLaTeX, AUCTeX, RefTeX, and albcorp names.

\newcommand{\PDFLaTeX}[0]{PDF\LaTeX{}}
\newcommand{\AUCTeX}[0]{AUC\TeX{}}
\newcommand{\RefTeX}[0]{Ref\TeX{}}
\newcommand{\albLogo}[0]{%
  {\ttfamily\bfseries{a}\kern-.025em\lower.5ex\hbox{l}\kern-.125em{b}\@}%
}



\makeatother



%%% Local Variables:
%%% mode: latex
%%% TeX-master: t
%%% TeX-parse-self: nil
%%% TeX-auto-save: nil
%%% End:
