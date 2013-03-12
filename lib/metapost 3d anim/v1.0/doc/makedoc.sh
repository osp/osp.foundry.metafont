#! /bin/sh

mpt 3d
tex 3d \\end
dvips -Z -t a4 -o 3d.ps 3d.dvi

mpt poly
tex poly \\end
dvips -Z -t a4 -o poly.ps poly.dvi
