#! /bin/bash
# 
# AlbThesis/alb-thesis-profile.sh
# 
#   - LaTeX styles used within my PhD thesis.  
# 
# $Id$
# 


# Extend the TEXINPUTS search path to include this package.

export TEXINPUTS="${TEXINPUTS%:}:${HOME}/Config/AlbThesis:"
