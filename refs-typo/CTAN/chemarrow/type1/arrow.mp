%
% arrow.mf produces new arrow heads e.g. for chemical reaction schemes
%
% Author: Thomas Schroeder schroeder@ictw.chemie.uni-karlsruhe.de
%
% Version: 0.5 February 1 2001
%
% arrow.mf is inspired by and based on cryst.mf version 1.0 by Ulrich Mueller
%



mode_setup; font_size 10pt#;
font_identifier:="arrow"; font_coding_scheme:="UNSPECIFIED";
ff:=0.82;
u#:=ff*0.10pt#;
br#:=108u#; ho#:=100u#; ul#:=32u#; % Breite, Hoehe, Unterlaenge
define_pixels(u,br,ho,ul);


beginchar(65,120u#,68u#,0);
"right arrow";
z1=(0u,33.2u); z2=(88u,33.2u); z3=(88u,27.7u); z4=(0,27.7u);
z5=(116u,30.7u); z6=(63u,7.2u); z7=(62u,9.2u);
z8=(76u,30.7u); z9=(62u,51.2u); z10=(63u,53.2u);
fill z10{dir -28}..{dir -15}z5..z5{dir -165}..{dir -152}z6--z7--z8--z9--cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 10);
endchar;


beginchar(66,120u#,68u#,0);
"up right harpoon";
z1=(0u,33.2u); z2=(88u,33.2u); z3=(88u,27.7u); z4=(0,27.7u);
z5=(116u,27.7u); z6=(63u,55u); z7=(62u,53u); z8=(74u,27.7u);
fill z6{dir -32}..{dir -22}z5--z8--z7--z6..cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 8);
endchar;

beginchar(67,120u#,68u#,0);
"down right harpoon";
z1=(0u,33.2u); z2=(88u,33.2u); z3=(88u,27.7u); z4=(0,27.7u);
z5=(116u,33.2u); z6=(63u,5u); z7=(62u,7u); z8=(76u,33.2u);
fill z6{dir 32}..{dir 22}z5--z8--z7--cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 8);
endchar;


beginchar(68,120u#,68u#,0);
"left arrow";
z1=(120u,33.2u); z2=(32u,33.2u); z3=(32u,27.7u); z4=(120u,27.7u);
z5=(4u,30.7u); z6=(57u,7.2u); z7=(58u,9.2u);
z8=(42u,30.7u); z9=(58u,51.2u); z10=(57u,53.2u);
fill z10{dir -152}..{dir -165}z5..z5{dir -15}..{dir -28}z6--z7--z8--z9--cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 10);
endchar;

beginchar(69,120u#,68u#,0);
"up left harpoon";
z1=(120u,33.2u); z2=(31u,33.2u); z3=(31u,27.7u); z4=(120u,27.7u);
z5=(4u,27.7u); z6=(57u,55u); z7=(58u,53u); z8=(46u,27.7u);
fill z5{dir 22}..{dir 32}z6--z7--z8--z5..cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 8);
endchar;


beginchar(70,120u#,68u#,0);
"down left harpoon";
z1=(120u,33.2u); z2=(31u,33.2u); z3=(31u,27.7u); z4=(120u,27.7u);
z5=(4u,33.2u); z6=(57u,5u); z7=(58u,7u); z8=(40u,33.2u);
%fill z5{dir 22}..{dir 32}z6--z7--z8--z5..cycle;
fill z5{dir -15}..{dir -28}z6--z7--z8--z5..cycle;
fill z1--z2--z3--z4--cycle;
% labels(range 1 thru 8);
endchar;

beginchar(71,74u#,68u#,0);
"new minus sign";
% z1=(2u,33.2u); z2=(72u,33.2u); z3=(72u,27.7u); z4=(2u,27.7u); z5=(0u,30.7u); z6=(74u,30.7u);
z2=(2u,33.2u); z3=(72u,33.2u); z5=(72u,27.7u); z6=(2u,27.7u); z1=(0u,30.7u); z4=(74u,30.7u);
% fill z2--z3--z5--z6--cycle;
fill z6..z1..z2--z3..z4..z5--z6--cycle;
% labels(range 1 thru 6);
endchar;

bye.
