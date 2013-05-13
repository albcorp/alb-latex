%%%
%%% AlbLaTeX/latex/alb-journal.cls
%%%
%%%     Copyright (C) 2005-2008, 2013 Andrew Lincoln Burrow
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
%%%     'alb-avm.sty', 'alb-corp.cls', 'alb-float-tools.sty',
%%%     'alb-graph-theory.sty', 'alb-journal.cls',
%%%     'alb-order-theory.sty', 'alb-proofs.sty', 'alb-theorems.sty',
%%%     'alb-thesis.cls', 'alb-algorithms.tex', 'alb-avm.tex',
%%%     'alb-corp.tex', 'alb-float-tools.tex', 'alb-graph-theory.tex',
%%%     'alb-journal.tex', 'alb-order-theory.tex', 'alb-proofs.tex',
%%%     'alb-theorems.tex', 'alb-thesis.tex',
%%%     'alb-journal-glossary.ist', 'alb-journal-index.ist',
%%%     'alb-thesis-glossary.ist', and 'alb-thesis-index.ist'.
%%%
%%%   - Set up the page layout, and section formatting to suite a
%%%     research journal, where each dated entry is a chapter, and each
%%%     note is a paragraph section.
%%%
%%%   - See: 'alb-journal.pdf'.
%%%



\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{alb-journal}



%
% No options recognised.

\ProcessOptions


%
% Load base class.

\LoadClassWithOptions{book}


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

% Use the 'multicol' package for the indices.
% See: 'multicol.dvi'.
\RequirePackage{multicol}



\makeatletter



%
% Page layout.
%
% The page is laid out according to the document class options, and the
% geometry set here.  The geometry must include a marginpar allocation,
% i.e., 'includemp' must be true.  The geometry must not reverse the
% margins, i.e., 'reversemp' must not be set.  Change other booleans in
% the document class options, i.e., select 'oneside' or 'twoside', and
% 'titlepage' or 'notitlepage'.

\geometry{%
  verbose, dvips,
  inner=0.111\paperwidth,
  outer=0.222\paperwidth,
  top=0.111\paperwidth,
  bottom=0.222\paperwidth,
  ignorehead, headheight=12pt, headsep=0pt,
  ignorefoot, footskip=0.111\paperwidth,
  ignoremp, marginparwidth=0.111\paperwidth, marginparsep=0.037\paperwidth%
}


%
% Headers and footers.
%
% See: 'fancyhdr.dvi'.

% Use the defined fancy page style.
\pagestyle{fancy}

% Capture the chapter titles.
\newcommand{\alb@CurrentChapterTitle}[0]{}
\renewcommand{\chaptermark}[1]{%
  \renewcommand{\alb@CurrentChapterTitle}{#1}%
  \renewcommand{\alb@CurrentSectionTitle}{}%
}

% Capture the section titles.
\newcommand{\alb@CurrentSectionTitle}[0]{}
\renewcommand{\sectionmark}[1]{%
  \renewcommand{\alb@CurrentSectionTitle}{#1}%
}

% Plain page style: folio on the outer edge of the footer.
\fancypagestyle{plain}{%
  \fancyhf{}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{0pt}
  \ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
    \fancyfoot[LE]{\normalfont\sffamily\thepage}%
    \fancyfoot[RO]{\normalfont\sffamily\thepage}%
  }{% ELSE ... oneside layout
    \fancyfoot[R]{\normalfont\sffamily\thepage}%
  }
}

% Default page style: header in the outer margin, and the folio on the
% outer edge of the footer.
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
  \fancyfoot[LE]{\normalfont\sffamily\thepage}%
  \fancyfoot[RO]{\normalfont\sffamily\thepage}%
  \fancyhead[LE]{%
    \setlength{\unitlength}{\marginparwidth + \marginparsep}%
    \begin{picture}(0,0)
      \put(-1,0){%
        \parbox[t][0.25\textheight][b]{\marginparwidth}{%
          \raggedright\normalfont\sffamily\alb@CurrentChapterTitle%
        }%
      }%
    \end{picture}%
  }%
  \fancyhead[RO]{%
    \setlength{\unitlength}{\marginparsep}%
    \begin{picture}(0,0)
      \put(1,0){%
        \parbox[t][0.25\textheight][b]{\marginparwidth}{%
          \raggedleft\normalfont\sffamily\alb@CurrentChapterTitle%
        }%
      }%
    \end{picture}%
  }%
}{% ELSE ... oneside layout
  \fancyfoot[R]{\normalfont\sffamily\thepage}%
  \fancyhead[R]{%
    \setlength{\unitlength}{\marginparsep}%
    \begin{picture}(0,0)
      \put(1,0){%
        \parbox[t][0.25\textheight][b]{\marginparwidth}{%
          \raggedleft\normalfont\sffamily\alb@CurrentChapterTitle%
        }%
      }%
    \end{picture}%
  }%
}


%
% Chapter heading formats.
%
% There are non-trivial tricks in the definitions of section heading
% styles using the @startsection macro.  The descriptions of the
% mandatory fields are therefore given here:

% Redefine the command to build the unnumbered chapter titling.
\renewcommand{\@makeschapterhead}[1]{%
  \vspace*{2\baselineskip}%
  \begin{list}{}{%
      \setlength{\leftmargin}{0pt}%
      \setlength{\rightmargin}{0pt - \marginparsep - \marginparwidth}%
      \setlength{\labelwidth}{0pt}%
      \setlength{\labelsep}{0pt}%
      \setlength{\topsep}{0pt}%
    }%
  \item\raggedleft\normalfont\sffamily\bfseries\huge#1%
  \end{list}%
  \par\nopagebreak%
  \vspace{3\baselineskip}%
  \markboth{\MakeUppercase{#1}}{\MakeUppercase{#1}}%
}

% Redefine the command to build the numbered chapter titling.
\renewcommand{\@makechapterhead}[1]{%
  \vspace*{2\baselineskip}%
  \begin{list}{}{%
      \setlength{\leftmargin}{0pt}%
      \setlength{\rightmargin}{0pt - \marginparsep - \marginparwidth}%
      \setlength{\labelwidth}{0pt}%
      \setlength{\labelsep}{0pt}%
      \setlength{\topsep}{0pt}%
    }%
  \item\raggedleft\normalfont\sffamily\bfseries\huge#1%
  \end{list}%
  \par\nopagebreak%
  \vspace{3\baselineskip}%
  \markboth{\MakeUppercase{#1}}{\MakeUppercase{#1}}%
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

% Remove all section numbering.
\setcounter{secnumdepth}{-1}

% Sections headings are: sans-serif, bold, large, upper/lowercase.
\renewcommand{\section}{\@startsection%
  {section}% Name
  {1}% Level
  {0mm}% Indent
  {-2\baselineskip}% Before skip
  {1.0\baselineskip}% After skip
  {\normalfont\sffamily\bfseries\large}% Style
}

% Subsections headings are: sans-serif, small, uppercase.
\renewcommand{\subsection}{\@startsection%
  {subsection}% Name
  {2}% Level
  {0mm}% Indent
  {-1.5\baselineskip}% Before skip
  {0.5\baselineskip}% After skip
  {\normalfont\sffamily\small\MakeUppercase}% Style
}

% Subsubsections are: sans-serif, upper/lowercase.
\renewcommand{\subsubsection}{\@startsection%
  {subsubsection}% Name
  {3}% Level
  {0mm}% Indent
  {-1.0\baselineskip}% Before skip
  {0.5\baselineskip}% After skip
  {\normalfont\sffamily}% Style
}

% Paragraphs are: bold, flow into the next paragraph.
\renewcommand{\paragraph}{\@startsection%
  {paragraph}% Name
  {4}% Level
  {0mm}% Indent
  {-1.0\baselineskip}% Before skip
  {-1em}% After skip
  {\normalfont\bfseries}% Style
}


%
% 'albResearchNote{SOURCES}{KEYWORDS}' environment.
%
% Typeset a research note identified by a citation and tagged with a
% collection of keywords.  The first argument should contain \citet
% commands.  The second argument should contain \glossary commands.  The
% environment generates typeset output for \glossary commands in the
% second argument.  Use only simple glossary entries in this context and
% write phrases using ~ as a separator character.  The environment is a
% list environment.

% \alb@InvisibleGlossary, \alb@VisibleGlossary: Pair of macros locally
% bound inside 'albResearchNote' to cause glossary entry to be typeset
\def\alb@VisibleGlossary#1{%
  #1\alb@InvisibleGlossary{#1}%
}

% 'albResearchNote': Layout research note, and replace \glossary command
% with \alb@VisibleGlossary in typesetting second argument.
\newenvironment*{albResearchNote}[2]{%
  \vspace{3ex plus 1.5 ex minus 1 ex}%
  \noindent{\normalfont\sffamily #1}\par%
  \nopagebreak[4]%
  \noindent{%
    \let\alb@InvisibleGlossary=\glossary%
    \let\glossary=\alb@VisibleGlossary%
    \normalfont\sffamily\bfseries\footnotesize #2%
  }\par%
  \nopagebreak[4]%
  \vspace{0.25\baselineskip}\noindent\ignorespaces%
  \begin{trivlist}%
}{%
  \end{trivlist}%
}


%
% Keyword and citation indices.
%
% Use the LaTeX glossary and index systems to generate the keyword and
% citation indices respectively.  The raw indices are processed with
% MakeIndex.

% Define 'see' encapsulation command, e.g., \index{partial order|see{poset}}.
\newcommand{\see}[2]{\emph{see} #1}

% Define the \printglossary and \printindex commands to read the files
% generated by MakeIndex.
\newcommand{\printglossary}[0]{\@input@{\jobname.gls}}
\newcommand{\printindex}[0]{\@input@{\jobname.ind}}

% Define theglossary environment without a default heading.
\newenvironment{theglossary}[0]{%
  \begin{multicols}{2}%
    \let\item\@idxitem%
}{%
  \end{multicols}%
}

% Redefine theindex environment without a default heading.
\renewenvironment{theindex}[0]{%
  \begin{multicols}{2}%
    \let\item\@idxitem%
}{%
  \end{multicols}%
}



\makeatother



%%% Local Variables:
%%% mode: latex
%%% TeX-master: t
%%% TeX-parse-self: nil
%%% TeX-auto-save: nil
%%% End:
