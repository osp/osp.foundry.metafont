Prescius Gothic. The pgoth fonts TBD


    The pgothic and allpgoth packages provide Metafont fonts based on the 
Gothic Textura Prescius vel sine pedibus manuscript book-hand used 
from the 13th century AD. The font consists of majuscules, minuscules 
and digits, with some appropriate period punctuation marks. 
Both normal and bold versions are provided. 
This is one in a series of manuscript fonts.

    Access to the docmfp package is desireable but not essential.

    The distribution consists of the following files:
README (this file)
pgothic.ins
pgothic.dtx
tryfont.tex
allfont.tex

    The distribution is provided under the terms of the LaTeX
Project Public License (LPPL).

To install the package:
=======================

o run: latex pgothic.ins, which will generate:
       pgoth*.mf files
       *.fd files
       pgothic.sty
       allpgoth.sty
o Move the generated files to locations where LaTeX will find them.
    For example: the pgoth*.mf files to .../texmf/fonts/source/public/pgothic
                 the *.fd and *.sty files to /texmf/tex/latex/local/pgothic

    On a teTeX distribution you might also have to add lines like the 
    following to texmf/fontname/special.map:
      pgoth7.mf        public    pgothic
      pgoth10.mf       public    pgothic
      pgoth17.mf       public    pgothic
      pgothb7.mf       public    pgothic
      pgothb10.mf      public    pgothic
      pgothb17.mf      public    pgothic
    On a teTeX distribution remember to run texhash after moving the files.

    (I use a teTeX distribution and found that if I didn't add to the special.map
     file, then the generated tfm and pk files were put in directories corresponding
     to some vendor's font.) 

    Other LaTeX distributions might require procedures similar to those for teTeX.

To typeset the documentation:
=============================

o run: latex pgothic.dtx
o if you want an index then run: makeindex -s gind.ist pgothic
o run: latex pgothic.dtx
o Print pgothic.dvi for a hardcopy of the package manual 

    Note that the docmfp package is normally required for LaTeXing pgothic.dtx.
The package is available from CTAN in tex-archive/macros/latex/contrib/supported.
If you haven't got this and don't want to get it, then there are instructions
in the first part of the .dtx file about how to edit it to avoid the use of docmfp.
(docmfp is an extension of the doc package for describing METAFONT, or other
non-LaTeX, code.)


To check out your installation:
===============================

o run: latex tryfont  for a testpage of the fonts
o run: latex allfont  for a document typeset with the fonts

2002/12/30
Peter Wilson
peter.r.wilson@boeing.com
