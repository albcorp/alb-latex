#
# alb-latex/alb-latex-profile.sh
#
#   - Profile script to enable the alb LaTeX packages
#


# Determine where this script resides

SCRIPT_DIR="$(dirname "$(realpath "$BASH_SOURCE")")"


# Extend the TEXINPUTS search path to include this package

export TEXINPUTS="${TEXINPUTS%:}:${SCRIPT_DIR}/latex:"


# Extend INDEXSTYLE search path to include this package.

export INDEXSTYLE="${INDEXSTYLE%:}:${SCRIPT_DIR}/latex:"
