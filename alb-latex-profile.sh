#
# AlbLaTeX/alb-latex-profile.sh
#
#   - Profile script to enable the alb LaTeX packages
#


# Extend the TEXINPUTS search path to include this package

export TEXINPUTS="${TEXINPUTS%:}:$(dirname ${BASH_SOURCE})/latex:"


# Extend INDEXSTYLE search path to include this package.

export INDEXSTYLE="${INDEXSTYLE%:}:$(dirname ${BASH_SOURCE})/latex:"
