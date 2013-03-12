### ========================================================================
###  @SED-file{
###     author               = "Ulrik Vieth",
###     email                = "vieth@thphy.uni-duesseldorf.de",
###     filename             = "mp2mft.sed",
###     version              = "0.07",
###     date                 = "29 January 1996",
###     time                 = "01:05:16 MET",
###     codetable            = "ISO/ASCII",
###     checksum             = "45153 37 192 1531",
###     supported            = "yes",
###     keywords             = "MFT, MetaPost, pretty-printing",
###     abstract             = "",
###     docstring            = "",
###  }
### ========================================================================
#
# Convert METAPOST sources for MFT input:
#
# Present material in `btex ... etex' as a TeXnical MFT comment.
# The `btex ... etex' groups may not extend across line breaks.
# And there may be only one such group per line.
#
# This file is mostly based on the file "mft.sed" by Andreas Scherer
# <scherer@genesis.informatik.rwth-aachen.de>, but some changes and
# corrections have been made. TeX labels are now printed verbatim
# (as quoted strings) so that their information won't get lost.
#
# Usage:
#
#   sed -f mp2mft.sed < file.mp > file.mf
#   mft file[.mf] -s mp2[.mft]
#
# Code:
#
/verbatimtex/ s/\(.*\)verbatimtex[ ]\+\(.*\)[ ]\+etex\(.*\)/\1verbatimtex ... etex\3 %\\ \\TeX\\ code: |"\2"|/
/btex/ s/\(.*\)btex[ ]\+\(.*\)[ ]\+etex\(.*\)/\1btex ... etex\3 %\\ \\TeX\\ label: |"\2"|/
