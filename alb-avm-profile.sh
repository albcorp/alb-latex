#
# AlbAVM/alb-avm-profile.sh
#
#   - Profile script to enable AlbAVM packages in LaTeX.
#
# $Id$
#


# Extend TEXINPUTS search path to include this package.

export TEXINPUTS="${TEXINPUTS%:}:${HOME}/Config/AlbAVM:"
