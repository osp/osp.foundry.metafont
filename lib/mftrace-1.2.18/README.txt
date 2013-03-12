mftrace - Scalable Fonts for MetaFont
*************************************

1 mftrace - Scalable PostScript Fonts for MetaFont
**************************************************

1.1 Introduction
================

`mftrace' is a small Python program that lets you trace a TeX bitmap
font into a PFA or PFB font (A PostScript Type1 Scalable Font) or TTF
(TrueType) font.  It is licensed under the GNU GPL.

   Scalable fonts offer many advantages over bitmaps, as they allow
documents to render correctly at many printer resolutions. Moreover,
Ghostscript can generate much better PDF, if given scalable PostScript
fonts.

   Versions prior to 1.0.5 were called `pktrace'.

1.2 Download
============

   *
     `http://lilypond.org/download/sources/mftrace/mftrace-1.2.18.tar.gz'

   * GIT at `https://github.com/hanwen/mftrace'

1.3 Required
============

`mftrace' is a thin Python wrapper around some other programs that do
the real work: a tracing program and t1asm.  To run mftrace you need:
   * A tracing program:       autotrace >= 0.30 (see
     `http://autotrace.sourceforge.net'       or potrace (see
     `http://potrace.sourceforge.net').

     Potrace is recommended as it runs quicker than autotrace.

   * Python-2.2 or later. See `http://www.python.org/'

   * t1utils. See `http://www.lcdf.org/~eddietwo/type/'

   * TeX-your tex installation should include
        * kpsewhich,

        * MetaFont

1.4 Recommended
===============

   * A recent version (040215 or newer) of FontForge
     (http://fontforge.sourceforge.net). Some of `mftrace'
     functionality requires FontForge to be present on user's system.
     This includes rounding to integer, simplifying and autohinting
     font outlines, as well as generating any output formats except
     PFA, PFB and AFM. You should not request any of these features
     using `mftrace' options if you don't like your font to be run
     through FontForge (note that in this case you also have to
     explicitly specify `--noround' to disable rounding to integer).

   * Alternatively, you need GhostScript with its `printafm' utility,
     available somethere in your PATH. `mftrace' uses `printafm' to
     generate AFM files in case there is no need to process the font
     with FontForge.


1.5 Red Hat
===========

A RPM may be built by issuing

             rpmbuild -tb mftrace-VERSION.tar.gz

1.6 Debian GNU/Linux
====================

Users of Debian unstable (and Debian 3.0 when it is released) can
install all requirements by running (as root):

         apt-get install mftrace
   If you wish to also install the FontForge package to simplify and
autohint the font, then run the command

         apt-get install fontforge

1.7 Install
===========

Install the prerequite packages. Then run

         ./configure
         make install
   in the mftrace directory. Run as follows:

     mftrace cmr10

1.8 Invoking mftrace.
=====================

Command line options:
`--formats=LIST'
     A comma-separated list of formats to generate. Choices include:
     AFM,  PFA, PFB, TTF and SVG. Default is to generate a PFA file.
     Note that  `fontforge' (formerly called `pfaedit') needs to be
     installed  in order to generate any format except PFA or PFB. For
     generating AFM  you need either `fontforge' or `ghostscript'.

`-e,--encoding=ENC'
     Use encoding file ENC. Encoding files used by `mftrace' are
     basically in the  GhostScript/dvips format, but you may use a
     special  `.notavail' glyph name in order to tell mftrace not to
     process  a specific glyph. If this option is not specified,
     mftrace will try to  determine the encoding file automatically,
     from the encoding specified  in the TFM file.

`--glyphs=LIST'
     Only process glyphs in LIST, which is a  comma-delimited  list of
     decimal numbers or ranges.
            --glyphs 1-10,50,55,90-100

`--gffile=NAME'
     =  Take glyphs from file NAME.

`--grid GRIDSIZE'
     Set reciprocal grid size in em units multiplied by ratio
     magnification/1000.  For example `--grid 10 --magnification 1000'
     will round coordinates of control points to 1/10 of em unit.
     Useful simultaneously with `--noround'  option. Default GRIDSIZE
     value is 1, i. e. round to integer.

`-h,--help'
     help on options.

`-k,--keep'
     Retain all temporary files in the directory `mftrace.dir/'. This
     is useful for debugging problems.

`--keep-trying'
     Try to continue if external programs called by mftrace fail. If
     METAFONT  crashes with overflow errors, but nevertheless outputs a
     GF file, try to  process its output as is (useful for some buggy
     fonts, see below). If  potrace/autotrace fails to trace a specific
     character, first try it with  a less smoothed curve, and if that
     fails, skip the character.

     By default mftrace outputs `trace-bug-FONTNAME-NUMBER.pbm' and
     stops the process with a request to file a bugreport.

`--magnification'
     The magnification to use for the PFA file. The default is 1000. The
     larger the more precise the PFA file will be. However, when
     magnification is too large METAFONT can crash with overflow errors.

     Sadly, many MF fonts contain resolution checks
            if dots_per_inch * design_size > 1500:
              ...
     This check is susceptible to overflow errors.  Such code should be
     reported as a bug, and changed to
            if dots_per_inch > (1500 / design_size):
              ...

`--noround'
     Don't round coordinates of control points to integer values.
     Useful simultaneously  with `--grid' option. Disabled by default.

`-o,--output-base=FILE'
     Output to FILE.pfa or FILE.pfb.

`--simplify'
     Pass the font through FontForge for automatic  simplification and
     hinting.

`--tfmfile=FILE'
     Use FILE for the TFM file.  This file is needed to determine at
     what resolution to run MetaFont.

`-V,--verbose'
     Be verbose: print all commands as they are invoked. This is useful
     for debugging.

`-v,--version'
     Print version number

`--dos-kpath'
     Try to kludge up the paths coming from MikTeX for a cygwin
     environment. If this doesn't work, specify `--tfmfile' and
     `--encoding' manually.

`-w,--warranty'
     show warranty and copyright

`--potrace'
     use Potrace (default).

`--autotrace'
     use AutoTrace.

`-D,--define=SYMBOL=VALUE'
     Set the font info SYMBOL to the given VALUE. For example
     `-DFamilyName=Foo' sets the font family name to `Foo'.

     Mftrace tries to fill in sensible values for the FontName,
     FamilyName, FullName and Weight fields. It does so by guessing
     values for the CM font series. For other fonts, it tries to read
     an AFM file (which is not likely to exist). Suggestions for a more
     generic way to handle this are welcome.


   Mftrace uses kpathsea for finding fonts, so any kpathsea variable can
be used to fine-tune which files should be loaded.  For example, you
can set `MFINPUTS' to specify which paths to search for `.mf' files.

   Additional options may be passed to the backend program (potrace or
autotrace) with the `MFTRACE_BACKEND_OPTIONS' environment variable.

1.9 Discussion
==============

Why use `mftrace' over textrace (http://textrace.sourceforge.net)?
Textrace and mftrace are functionally similar. However, mftrace is
quicker, more cleanly written and can be installed using standard
methods. Additionally, textrace requires perl, ghostscript and dvips.

   How about MetaFog (http://www.truetex.com)? MetaFog operates
directly on the curves that generate the bitmap font, its outlines will
probably be smaller and better. However, MetaFog is a proprietary
product: its source code is not available, and it will only run on a
limited number of platforms.

   How about MetaType1 (ftp://bop.eps.gda.pl/pub/metatype1/)?
MetaType1 is an approach that puts severe constraints on what may be
done in a font program. It does not work for fonts containing overlaps
and shaped pens.

   How about FontForge (http://fontforge.sourceforge.net/) itself?
FontForge is an interactive editor, but it can be scripted. Since it
supports bitmap tracing and TeX bitmap fonts, it is possible to
duplicate the functionality of mftrace.  However, out of the box,
FontForge does not recognize TeX encodings.

1.10 Bugs and todo
==================

   * Environment variables containing relative directories,   such as
     MFINPUTS or TFMINPUTS, are not handled correctly.

   * Discuss fonts & copyright.

   * Submit `tfm.py' to www.python.org. `tfm.py'  is a python   module
     to parse Tex Font Metric file.

   Should you encounter any bug or problem, then please send a bugreport
to Han-Wen Nienhuys <hanwen@xs4all.nl>.

1.11 Author
===========

Han-Wen Nienhuys <hanwen@xs4all.nl>

1.12 Credits
============

Gf2pbm, the utility to convert a MetaFont GF file to a PBM file was
based on Paul Vojta's Xdvi. The license notice is reproduced below.

   Thanks to all bughunters and contributors: Andrey V. Panov, Geoffrey
Alan Washburn, Julian Gilbey (http://www.maths.qmul.ac.uk/~jdg/)
Gu"nther Spahlinger, Richard Mahoney, Stanislav Brabec, and Thomas
Bushnell BSG.

     Copyright (c) 1990-1999  Paul Vojta

     Permission is hereby granted, free of charge, to any person
     obtaining a copy of this software and associated documentation
     files (the "Software"), to deal in the Software without
     restriction, including without limitation the rights to use, copy,
     modify, merge, publish, distribute, sublicense, and/or sell copies
     of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be
     included in all copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     NONINFRINGEMENT.  IN NO EVENT SHALL PAUL VOJTA BE LIABLE FOR ANY
     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
     CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
     WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   The rest of the package mftrace  script itself is licensed under the
GNU General Public License (http://www.gnu.org/licenses/gpl.txt).

1.13 See also
=============

   * Type1 font specification
     (http://partners.adobe.com/asn/developer/pdfs/tn/T1Format.pdf)

   * Supplement to the Type1 specification
     (http://partners.adobe.com/asn/developer/pdfs/tn/5015.Type1_Supp.pdf).

