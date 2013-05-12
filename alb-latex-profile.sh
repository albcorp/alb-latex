#
# AlbLaTeX/alb-latex-profile.sh
#
#   - Profile script to enable alb LaTeX packages in LaTeX
#


# Extend the TEXINPUTS search path to include this package

export TEXINPUTS="${TEXINPUTS%:}:${HOME}/Config/AlbLaTeX/latex:"


# Extend INDEXSTYLE search path to include this package.

export INDEXSTYLE="${INDEXSTYLE%:}:${HOME}/Config/AlbLaTeX/bibtex:"
