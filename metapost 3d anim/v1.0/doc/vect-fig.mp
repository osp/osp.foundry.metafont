% Figures for the illustration of the metapost packages `3d.mp' and `poly.mp'
% Metapost
% Denis Roegel (roegel@loria.fr), 21,23,24,25,26,27/2/1997
%                                 11/3/1997
%                                 21/3/1997 (figure 17)
%                                 22/4/1997 (figure 10)

numeric u;
u=2cm;

ahangle:=20;


beginfig(1);
z1=origin;
z2-z1=(u,0);z4=.5[z1,z2];z3-z1=(z2-z1) rotated 60;
draw z1--z2--z3--cycle;draw z3--z4 dashed evenly;
label.ulft(btex $c$ etex,.5[z1,z3]);
label.rt(btex $h$ etex,.5[z3,z4]);
label.bot(btex $c$ etex,z4);
label.urt(btex $c$ etex,.5[z2,z3]);
endfig;

beginfig(2);
numeric cosan,sinan,an;
z1=origin;
sinan=1/sqrt(3);cosan=sqrt(1-sinan**2);
an=2*angle((cosan,sinan));
z2-z1=(u*sqrt(3)/2,0);
z3-z2=(z1-z2) rotated (-an);
draw z1--z2--z3--cycle;
draw .2[z2,z1]{up}..{dir(90-an)}(.2[z2,z3]);
label.ulft(btex $c$ etex,.5[z1,z3]);
label.bot(btex $c{\sqrt{3}\over2}$ etex,.5[z1,z2]);
label.urt(btex $c{\sqrt{3}\over2}$ etex,.5[z2,z3]);
label(btex $\alpha$ etex,.5[z2,.5[z1,z3]]);
endfig;

beginfig(3);
z1=origin;
z2-z1=(u,0);z4=.5[z1,z2];z3=(.5u,u/sqrt(2));
draw z1--z2--z3--cycle;draw z3--z4 dashed evenly;
label.ulft(btex $c{\sqrt{3}\over2}$ etex,.5[z1,z3]);
label.urt(btex $c{\sqrt{3}\over2}$ etex,.5[z2,z3]);
label.rt(btex $H$ etex,.5[z3,z4]);
label.bot(btex $c$ etex,z4);
endfig;

beginfig(4);
z1=origin;
z2-z1=(u,0) rotated 10;
z3-z2=(z2-z1) rotated 50; % angle sans signification
z4-z2=(.4(z2-z1)) rotated 150;
z5=.5[z2,z4];
draw z1--z2--z3--cycle;draw z1--z4--z3;
draw z2--z4;draw z1--z5--z3 dashed evenly;
label.ulft(btex $d$ etex,.5[z1,z3]);
label.bot(btex $c$ etex,.5[z1,z2]);
label.top(btex $h$ etex,.5[z1,z5]);
endfig;

beginfig(5);
z0=origin;
z1=u*dir(0);z2=u*dir(72);z3=u*dir(2*72);z4=u*dir(3*72);z5=u*dir(4*72);
draw z1--z2--z3--z4--z5--cycle;
draw z1--z3 dashed evenly;
draw z0--z4;
label.urt(btex $c$ etex,.5[z1,z2]);
label.top(btex $c$ etex,.5[z2,z3]);
label.urt(btex $d$ etex,.5[z1,z3]);
label.ulft(btex $r$ etex,.5[z0,z4]);
endfig;

beginfig(6);
numeric cosan,sinan,an;
z1=origin;
z2-z1=(u,0);
cosan=1-8/3*(cosd(36)**2);
sinan=sqrt(1-cosan**2);
an=angle((cosan,sinan));
z3-z2=(z2-z1) rotated (180-an);
draw z1--z2--z3--cycle;
draw (.2[z2,z1]){up}..{dir(90-an)}(.2[z2,z3]);
label.ulft(btex $2\cos(\pi/5)$ etex,.5[z1,z3]);
label.bot(btex $\sqrt{3}\over 2$ etex,.5[z1,z2]);
label.lrt(btex $\sqrt{3}\over 2$ etex,.5[z2,z3]);
label(btex $\alpha$ etex,.25[z2,.5[z1,z3]]);
endfig;

beginfig(7);
numeric a,b;
z1=origin;
z2-z1=(u,0);
z4-z2=(.5(z2-z1)) rotated 140;
z5=.5[z2,z4];
z3-z5=(z5-z1) rotated 40; % angle sans signification
z6-z5=.5(z5-z1);
a=angle(z3-z5);b=angle(z6-z5);
z7-z5=(.5[z5,z6]-z5) rotated (a-b);
draw z1--z2--z4--cycle;
draw z1--z5--z3 dashed evenly;
draw z5--z6 dashed evenly;
draw (.5[z5,z6]){up}..{dir(90+70)}z7;
label.top(btex $\alpha$ etex,z6);
label.lft(btex $o$ etex,z1);
label.ulft(btex $j$ etex,z4);
label.lrt(btex $i$ etex,z2);
label.top(btex $c$ etex,z3);
endfig;

beginfig(8);
z1=origin;
z9-z1=(u,0) rotated 20;
z2-z1=(.7u,0) rotated -25;
z3-z1=(0,.8u);z4-z1=3*(z9-z1);
z6-z4=2*(z3-z1);z5-z4=2*(z2-z1);
z8-z4=1.5(z6-z4+z5-z4);
z7-z1=.7(z8-z1);
z7-z13=z5-z4;z7-z12=z6-z4;
z11=(z7--z13) intersectionpoint (z4--z6);
z10=(z7--z12) intersectionpoint (z4--z5);
drawarrow z1--z3;drawarrow z1--z2;drawarrow z1--z9;
draw z6--z4--z5;
draw z9--z4 dashed evenly;
draw z1--z8 dashed evenly;
draw z7--z11 dashed withdots;
draw z7--z10 dashed withdots;
label.lft(btex $y_p$etex,z11);
label.bot(btex $x_p$etex,z10);
label.lft(btex $\overrightarrow{Obs_j}$ etex,.5[z1,z3]);
label.llft(btex $\overrightarrow{Obs_k}$ etex,.5[z1,z2]);
label.lrt(btex $\overrightarrow{Obs_i}$ etex,.5[z1,z9]);
label.bot(btex $Obs\_dist$ etex,.5[z1,z10]);
label.bot(btex $p$ etex,z7);
label.rt(btex $m$ etex,z8);
pickup pencircle scaled 2pt;
drawdot(z7);drawdot(z8);drawdot(z10);drawdot(z11);
pickup pencircle scaled 0.4pt;
label.llft(btex $Obs$ etex,z1);
label.rt(btex projection plane etex,z5);
endfig;

% 23/2/1997
beginfig(9);
z1=origin;
z2=(u,0) rotated 10;
z3-z2=z2-z1;
z4-z1=(0,.5u);z6-z4=2*(z4-z1);
z5-z2=z4-z1;
drawarrow z1--z3;drawarrow z1--z6;
drawarrow z1--z5;
drawarrow z4--z5 dashed evenly;
drawarrow z2--z5 dashed evenly;
pickup pencircle scaled 1pt;
drawarrow z1--z4;drawarrow z1--z2;
pickup pencircle scaled 0.4pt;
label.lft(btex $\overrightarrow{axis}$ etex,.5[z4,z6]);
label.lft(btex $\vec{h}$ etex,.5[z1,z4]);
label.bot(btex $\vec{a}$ etex,.5[z1,z2]);
label.ulft(btex $\vec{v}$ etex,.5[z1,z5]);
endfig;

% 23/2/1997
beginfig(10);
z1=origin;
z2-z1=(u,0);z4-z1=(0,u);z3-z1=(z2-z1) rotated 40;
z5-z2=.5(z2-z1);z6-z4=.5(z4-z1);
z7=(x1,y3);z8=(x3,y1);
drawarrow z1--z6;drawarrow z1--z5;
pickup pencircle scaled 1pt;
drawarrow z1--z4;drawarrow z1--z2;drawarrow z1--z3;
pickup pencircle scaled 0.4pt;
draw fullcircle scaled .15u shifted z1;
pickup pencircle scaled 2pt;
drawdot(z1);
pickup pencircle scaled 0.4pt;
draw z7--z3--z8 dashed evenly;
draw z2{up}..z3{dir(90+40)}..{left}z4;
drawarrow (.3z2){up}..(.3z3){dir(90+40)};
label.lft(btex $\vec{c}$ etex,z4);
label.llft(btex $\vec{b}$ etex,z1+(-.1u,-.1u));
label.bot(btex $\vec{a}$ etex,.5[z1,z2]);
label.ulft(btex $\vec{f}$ etex,.5[z1,z3]);
label.rt(btex $\alpha$ etex,.4u*dir(20));
endfig;

% 24/2/1997
beginfig(11);
z1=origin;
z2-z1=(u,0) rotated 10;
z3-z1=(z2-z1) rotated 30;
z4-z1=(z3-z1) rotated 30;
draw z1--z2;draw z1--z3;draw z1--z4;
drawarrow (.4z2){dir(90+10)}..(.4z3){dir(90+40)};
drawarrow (.4z3){dir(90+40)}..(.4z4){dir(90+70)};
label(btex $\alpha$ etex,.55u*dir(25));
label(btex $\alpha$ etex,.55u*dir(55));
label.llft(btex $o$ etex,z1);
label.bot(btex $i$ etex,z2);
label.lrt(btex $j$ etex,z3);
label.lft(btex $k$ etex,z4);
endfig;

% 24/2/1997 
% Euler angles
beginfig(12);
z1=origin;
z5-z1=(2u,0);z7-z1=(0,2u);z1-z2=2u*unitvector(dir(20));
z3-z1=1.5u*unitvector(dir(-40));
z4-z1=1.5u*unitvector(dir(-20));
z6-z1=2u*unitvector(dir(45));
z8-z1=2u*unitvector(dir(135));
drawarrow z1--z2;drawarrow z1--z5;drawarrow z1--z7;
drawarrow z1--z4;drawarrow z1--z6;drawarrow z1--z8;
drawarrow z1--z3 dashed evenly;
label.bot(btex $x$ etex,z2);
label.bot(btex $u$ etex,z3);
label.bot(btex $Obs_x$ etex,z4);
label.rt(btex $y$ etex,z5);
label.rt(btex $Obs_y$ etex,z6);
label.lft(btex $z$ etex,z7);
label.lft(btex $Obs_z$ etex,z8);
drawarrow (.2[z1,z2]){dir(-20)}..{dir(90-70)}(.2[z1,z3]);
drawarrow (.5[z1,z3]){dir(90-40)}..{dir(90-20)}(.5[z1,z4]);
drawarrow (.3[z1,z7]){dir(180)}..{dir(135+90)}(.3[z1,z8]);
label.top(btex $\theta$ etex,.4[z1,.5[z7,z8]]);
label.bot(btex $\psi$ etex,.4[z1,.5[z2,z3]]);
label.lrt(btex $\phi$ etex,.5[z1,.5[z3,z4]]);
endfig;

def ellipse_point(expr a,b,c,an)=
    c+(a*cosd(an)+b*sind(an))
  enddef;

% 25/2/1997
beginfig(13);
save a,b;
numeric v;
v=.5u;
pair a,b;
z0=origin;z1-z0=(v,0) rotated -45;z2-z0=(v,0) rotated 120;
z3-z1=(1.5v,0) rotated 10;z4-z2=(z3-z1) rotated 20;
a=z3-z1;b=z0-z1;
for i:=0 upto 4: z[10+i]=ellipse_point(a,b,z1,i*72);endfor;
a:=z4-z2;b:=z2-z0;
for i:=0 upto 4: z[20+i]=ellipse_point(a,b,.8[z0,z2],i*72);endfor;
draw z20--z21--z22--z12--z11--cycle;
draw z10--z11--z12--z13--z14--cycle;
z5=(z12--(z12+3*(z11-z12))) intersectionpoint (z10--(z10+3*(z0-z1)));
z6=.9[z12,z5]; % 20/3/1997
draw z20--z6--z10 dashed evenly;
draw z11--z6 dashed evenly;
label.rt(btex $A$ etex,z20);
label.rt(btex $B$ etex,z10);
label.urt(btex $O$ etex,z6);
endfig;

% 25/2/1997
beginfig(14);
z0=origin;
z1=u*dir(0);z2=u*dir(72);z3=u*dir(2*72);z4=u*dir(3*72);z5=u*dir(4*72);
z6=(z5--(z5+2*(z1-z5))) intersectionpoint (z2--(z2+2*(.5[z1,z5]-z0)));
draw z1--z2--z3--z4--z5--cycle;
draw z2--z4 dashed evenly;
draw z1--z6--z2 dashed evenly;
draw z0--z5;
label.top(btex $A$ etex,z2);
label.rt(btex $O$ etex,z6);
label.llft(btex $d_1$ etex,.5[z1,z2]);
label.ulft(btex $d_2$ etex,.5[z2,z4]);
label.urt(btex $d_3$ etex,.5[z2,z6]);
label.rt(btex $r$ etex,.5[z0,z5]);
endfig;

% 25/2/1997
beginfig(15);
numeric cosan,sinan,an;
z1=origin;
z2-z1=(u,0);
cosan=-cosd(72)/(2*sind(36)*sind(36));
sinan=sqrt(1/(sind(36)**2)*(1-1/(4*(sind(36)**2))));
an=180-angle((cosan,sinan));
z3-z2=(z2-z1) rotated an;
draw z1--z2--z3--cycle;
draw (.2[z2,z1]){up}..{dir(an-90)}(.2[z2,z3]);
label.ulft(btex $d_2$ etex,.5[z1,z3]);
label.bot(btex $d_3$ etex,.5[z1,z2]);
label.lrt(btex $d_3$ etex,.5[z2,z3]);
label(btex $\alpha$ etex,.25[z2,.5[z1,z3]]);
label.rt(btex $B$ etex,z3);
label.bot(btex $A$ etex,z1);
endfig;

% 27/2/1997 
% 
beginfig(16);
z1=origin;
z5-z1=(2u,0);z7-z1=(0,2u);z1-z2=2u*unitvector(dir(20));
z3-z1=1.7u*unitvector(dir(-30));
z4-z1=1.5u*unitvector(dir(-15));
z6-z1=2u*unitvector(dir(45));
z8-z1=2u*unitvector(dir(135));
z9-z1=2u*unitvector(dir(20));
z10-z1=2u*unitvector(dir(105));
drawarrow z1--z2;drawarrow z1--z5;drawarrow z1--z7;
drawarrow z1--z4;drawarrow z1--z6;drawarrow z1--z8;
drawarrow z1--z3 dashed evenly;
drawarrow z1--z9 dashed evenly;
drawarrow z1--z10 dashed evenly;
label.bot(btex $x$ etex,z2);
label.bot(btex $u$ etex,z3);
label.bot(btex $Obs_x$ etex,z4);
label.rt(btex $y$ etex,z5);
label.rt(btex $Obs_y$ etex,z6);
label.lft(btex $z$ etex,z7);
label.lft(btex $Obs_z$ etex,z8);
label.rt(btex $v$ etex,z9);
label.lft(btex $w$ etex,z10);
drawarrow (.2[z1,z2]){dir(-20)}..{dir(90-70)}(.2[z1,z3]);
drawarrow (.5[z1,z3]){dir(75)}..(.52[z1,z4]);
drawarrow (.2[z1,z9]){dir(90+20)}..{dir(90+45)}(.2[z1,z6]);
drawarrow (.3[z1,z7]){dir(180)}..{dir(105+90)}(.3[z1,z10]);
label.top(btex $\theta$ etex,.4[z1,.5[z7,z10]]);
label.bot(btex $\psi$ etex,.4[z1,.5[z2,z3]]);
label.rt(btex $\theta$ etex,.6[z1,.5[z3,z4]]);
label.urt(btex $\phi$ etex,.3[z1,.5[z6,z9]]);
endfig;

% 21/3/1997
beginfig(17);
z0=origin;z2-z0=(2u,0);z0-z1=(1.7u,0) rotated 30;
z3-z0=(0,2u);z4=.3[z0,z1];z5=.3[z0,z2];z6=.3[z0,z3];
for i:=0 upto 11:
    z[10+i]=2*cosd(i*30)*(z0-z4)+2*sind(i*30)*(z0-z5)+z6;
  endfor;
pickup pencircle scaled 1pt;
drawarrow z10{left}..z11..z12..z13{z4-z0}..z14..z15..z16{right}
     ..z17..z18..z19{z0-z4}..z20..z21..z10{left} dashed evenly;
drawarrow z0--z4;drawarrow z0--z5;drawarrow z0--z6;
pickup pencircle scaled .4pt;
drawarrow z0--z1;drawarrow z0--z2;drawarrow z0--z3;
label.lft(btex $x$ etex,z1);
label.rt(btex $y$ etex,z2);
label.top(btex $z$ etex,z3);
label.lrt(btex $\vec\imath$ etex,z4);
label.bot(btex $\vec\jmath$ etex,z5);
label.lft(btex $\vec k$ etex,z6);
endfig;

end
