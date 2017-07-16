%%%
%%% alb-latex/latex/alb-thesis.cls
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
%%%   - Set up the page layout, section formatting, and front and back
%%%     matter commands for a technical thesis.
%%%
%%%   - See: 'alb-thesis.pdf'.
%%%



\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{alb-thesis}



%
% Pass all options through.

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

% Use the 'caption' package to format captions.
% See: 'caption.pdf'.
\RequirePackage{caption}



\makeatletter



%
% Page layout.
%
% The page is laid out according to the document class options, and the
% geometry set here.  The geometry must include a marginpar allocation,
% i.e., 'includemp' must be true.  Change the booleans in the document
% class options, i.e., select 'oneside' or 'twoside', and select or omit
% 'reversemp'.  Set the lengths here, e.g., 'width', or 'headsep'.

\geometry{%
  verbose, dvips,
  top=25mm, bottom=30mm, left=35mm, right=15mm,
  headheight=5mm, headsep=7.5mm, footskip=15mm,
  includemp, marginparsep=7.5mm, marginparwidth=20mm%
}


%
% Headers and footers.
%
% Setup headers and footers using 'fancyhdr' package.
%
%   - Optimise appearance for non-reversed margins, and assume margin
%     paragraph space has been allocated.
%
%   - \leftmark refers to the chapter information, and \rightmark refers
%     to the section information.
%
%   - Place \alb@TheoremRefList contents in margin par via footers.

% Make the fancy page style the default.
\pagestyle{fancy}

% Place the headers and footers across the entire printable page.
\setlength{\headwidth}{\textwidth + \marginparsep + \marginparwidth}

% Capture the chapter and section titles (uncapitalised).
\ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
  \renewcommand{\chaptermark}[1]{%
    \markboth{#1\hfill\@chapapp~\thechapter}{}%
  }%
  \renewcommand{\sectionmark}[1]{%
    \markright{Section~\thesection\hfill#1}%
  }%
}{% ELSE ... oneside layout
  \renewcommand{\chaptermark}[1]{%
    \markboth{\@chapapp~\thechapter\hfill#1}{}%
  }%
  \renewcommand{\sectionmark}[1]{}%
}

% Reset \alb@TheoremRefList.
\let\alb@TheoremRefList=\relax

% Formatting for headers and footers.
\def\alb@HeaderAndFooterFormat{\normalfont\scshape}

% 'plain' page style: header is empty; footer is empty, and resets
% \alb@TheoremRefList.
\fancypagestyle{plain}{%
  \fancyhf{}%
  \renewcommand{\headrulewidth}{0pt}%
  \renewcommand{\footrulewidth}{0pt}%
  \ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
    \fancyfoot[C]{\let\alb@TheoremRefList=\relax}%
    % Verso footer contains the page number against the outer edge.
    \fancyfoot[LE]{%
      \alb@HeaderAndFooterFormat\thepage%
      \global\let\alb@TheoremRefList=\relax%
    }%
    % Recto footer contains the page number against the outer edge.
    \fancyfoot[RO]{%
      \alb@HeaderAndFooterFormat\thepage%
      \global\let\alb@TheoremRefList=\relax%
    }%
  }{% ELSE ... oneside layout
    % Recto footer contains the page number against the outer edge.
    \fancyfoot[R]{%
      \alb@HeaderAndFooterFormat\thepage%
      \global\let\alb@TheoremRefList=\relax%
    }%
  }%
}

% 'fancy' page style: header contains left and right marks, and page
% number; footer is empty, but inserts non-empty \alb@TheoremRefList
% into the margin, resets \alb@TheoremRefList.
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\ifthenelse{\boolean{@twoside}}{% THEN ... twoside layout
  % Verso header contains the \leftmark against the inside edge.
  \fancyhead[RE]{%
    \parbox[t]{\textwidth}{%
      \raggedright\alb@HeaderAndFooterFormat\leftmark%
    }%
  }%
  % Verso header contains the page number against the outer edge.
  \fancyhead[LE]{%
    \parbox[t]{\marginparwidth}{%
      \raggedright\alb@HeaderAndFooterFormat\thepage%
    }%
  }%
  % Verso footer inserts \alb@TheoremRefList against the bottom outer
  % edge.
  \fancyfoot[LE]{%
    \ifx \alb@TheoremRefList\relax%
      %
    \else%
      \setlength{\unitlength}{\footskip}%
      \begin{picture}(0,0)
        \put(0,1){%
          \parbox[b][\textheight][b]{\marginparwidth}{%
            \raggedright\alb@HeaderAndFooterFormat%
            \alb@ExpandTheoremRefList%
          }%
        }%
      \end{picture}%
      \rule{\marginparwidth}{0pt}%
      \global\let\alb@TheoremRefList=\relax%
    \fi%
  }%
  % Recto header contains the \rightmark against the inside edge.
  \fancyhead[LO]{%
    \parbox[t]{\textwidth}{%
      \raggedleft\alb@HeaderAndFooterFormat\rightmark%
    }%
  }%
  % Recto headers contain the page number against the outer edge.
  \fancyhead[RO]{%
    \parbox[t]{\marginparwidth}{%
      \raggedleft\alb@HeaderAndFooterFormat\thepage%
    }%
  }%
  % Recto footer inserts \alb@TheoremRefList against the bottom outer
  % edge.
  \fancyfoot[RO]{%
    \ifx \alb@TheoremRefList\relax%
      %
    \else%
      \setlength{\unitlength}{\footskip}%
      \begin{picture}(0,0)
        \put(0,1){%
          \parbox[b][\textheight][b]{\marginparwidth}{%
            \raggedleft\alb@HeaderAndFooterFormat%
            \alb@ExpandTheoremRefList%
          }%
        }%
      \end{picture}%
      \rule{\marginparwidth}{0pt}%
      \global\let\alb@TheoremRefList=\relax%
    \fi%
  }%
}{% ELSE ... oneside layout
  % Recto header contains the \leftmark against the outer text edge.
  \fancyhead[L]{%
    \parbox{\textwidth}{\raggedleft\normalfont\itshape\leftmark}%
  }%
  % Recto header contains the page number against the outer edge.
  \fancyhead[R]{%
    \textit{\thepage}%
  }%
}


%
% Chapter titles.
%
% Setup the headers to use the marginpar allocation.  Assume chapters
% always appear on right pages.  This is untrue if 'openany' mode is on,
% in which case you must apply the hack used for horizontal positioning
% of floats.

% Redefine the command to build the unnumbered chapter titling.
% Align the title body to the outer edge of the margin notes.
% Also call \markboth with uppercase chapter title, as for contents etc.
\ifthenelse{\boolean{@reversemargin}}{% THEN ... reverse margin layout
  \renewcommand{\@makeschapterhead}[1]{%
    \ifdefined\float@addtolists\float@addtolists{\protect\addvspace{10pt}}\fi%
    \vspace*{25pt}%
    \begin{list}{}{%
        \setlength{\leftmargin}{0pt - \marginparsep - \marginparwidth}%
        \setlength{\rightmargin}{0pt}%
        \setlength{\labelwidth}{0pt}%
        \setlength{\labelsep}{0pt}%
        \setlength{\topsep}{0pt}%
      }%
    \item\sloppy\raggedright\normalfont\bfseries\Huge#1%
    \end{list}%
    \par\nopagebreak%
    \vspace{40pt}%
    \markboth{\MakeUppercase{#1}}{\MakeUppercase{#1}}%
  }%
}{% ELSE .. non-reverse margin layout
  \renewcommand{\@makeschapterhead}[1]{%
    \ifdefined\float@addtolists\float@addtolists{\protect\addvspace{10pt}}\fi%
    \vspace*{25pt}%
    \begin{list}{}{%
        \setlength{\leftmargin}{0pt}%
        \setlength{\rightmargin}{0pt - \marginparsep - \marginparwidth}%
        \setlength{\labelwidth}{0pt}%
        \setlength{\labelsep}{0pt}%
        \setlength{\topsep}{0pt}%
      }%
    \item\sloppy\raggedleft\normalfont\bfseries\Huge#1%
    \end{list}%
    \par\nopagebreak%
    \vspace{40pt}%
    \markboth{\MakeUppercase{#1}}{\MakeUppercase{#1}}%
  }%
}

% Redefine the command to build the numbered chapter titling.
% Align the title body to the outer edge of the margin notes, and the
% label to the edge of the text and the same side.
\ifthenelse{\boolean{@reversemargin}}{% THEN ... reverse margin layout
  \renewcommand{\@makechapterhead}[1]{%
    \ifdefined\float@addtolists\float@addtolists{\protect\addvspace{10pt}}\fi%
    \vspace*{25pt}%
    \begin{flushleft}%
      \normalfont\bfseries%
      \Large\MakeUppercase{\chaptername}\quad\Huge\thechapter%
    \end{flushleft}%
    \vspace{15pt}%
    \begin{list}{}{%
        \setlength{\leftmargin}{0pt - \marginparsep - \marginparwidth}%
        \setlength{\rightmargin}{0pt}%
        \setlength{\labelwidth}{0pt}%
        \setlength{\labelsep}{0pt}%
        \setlength{\topsep}{0pt}%
      }%
    \item\sloppy\raggedright\normalfont\bfseries\Huge#1%
    \end{list}%
    \par\nopagebreak%
    \vspace{40pt}%
  }%
}{% ELSE .. non-reverse margin layout
  \renewcommand{\@makechapterhead}[1]{%
    \ifdefined\float@addtolists\float@addtolists{\protect\addvspace{10pt}}\fi%
    \vspace*{25pt}%
    \begin{flushright}%
      \normalfont\bfseries%
      \Large\MakeUppercase{\chaptername}\quad\Huge\thechapter%
    \end{flushright}%
    \vspace{15pt}%
    \begin{list}{}{%
        \setlength{\leftmargin}{0pt}%
        \setlength{\rightmargin}{0pt - \marginparsep - \marginparwidth}%
        \setlength{\labelwidth}{0pt}%
        \setlength{\labelsep}{0pt}%
        \setlength{\topsep}{0pt}%
      }%
    \item\sloppy\raggedleft\normalfont\bfseries\Huge#1%
    \end{list}%
    \par\nopagebreak%
    \vspace{40pt}%
  }%
}


%
% Float parameters.
%
% Control the assignment of floats to mixed text and float pages.

% Switch to a new page when more than 60% of page would be top floats.
\renewcommand{\topfraction}{0.6}
\renewcommand{\floatpagefraction}{0.6}

% Minimum fraction of page to allocate to text.
\renewcommand{\textfraction}{0.4}


%
% Caption style.
%
% Style the captions to stand apart from the main text.

% Switch to a new page when more than 60% of page would be top floats.
\captionsetup{%
  format=default,indention=0em,labelformat=simple,labelsep=quad,%
  justification=justified,singlelinecheck=on,%
  font=normalsize,labelfont=sc,textfont=rm%
}



\makeatother



%%% Local Variables:
%%% mode: latex
%%% TeX-master: t
%%% TeX-parse-self: nil
%%% TeX-auto-save: nil
%%% End:
