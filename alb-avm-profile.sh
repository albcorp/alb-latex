#! /bin/bash
# 
# AlbThesis/alb-thesis-profile.sh
# 
#   - LaTeX styles used within my PhD thesis.  
# 
# $Id$
# 


# Extend the TEXINPUTS search path to include this package.

if [ -z "${TEXINPUTS}" ] ; then
    export TEXINPUTS=".:${HOME}/Local/AlbThesis:"
else
    export TEXINPUTS="${TEXINPUTS%:}:${HOME}/Local/AlbThesis:"
fi
