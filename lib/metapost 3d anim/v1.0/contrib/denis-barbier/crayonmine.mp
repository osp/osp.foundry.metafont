% Definition des crayons de papier colores
% A utiliser avec 3d.mp 1.0
% D. Barbier
% Version 0.2 (5 novembre 1997)
%
% Changements :
%   la numerotation des points a change
%       1 : origine
%       2 : pointe de la mine
%       3 --> 4*nbppf+2 : autres points
%
% Ces changements modifient bien sur le resultat des animations.
%
% nbppf : nombre de points sur l extremite plate du crayon
% rapp  : rapport largeur du cylindre/rayon du cylindre
% lgmine: longueur de la mine/longueur du corps du crayon
% fcmine: fraction de la mine qui est occupee par le graphite
%
% Apres mise a l'echelle, le corps du crayon a un rayon 1,
% l'extremite plane a pour abscisse 0, le debut de la mine
% l'abscisse rapp et la pointe rapp*(1+lgmine)
%
%   Valeurs par defaut ; peuvent etre modifiees par
%   l utilisateur avant input crayonmine.
%
numeric nbppf,rapp,lgmine,fcmine;
    nbppf := 12;
    rapp  := 8;
    lgmine := 0.4;
    fcmine := 0.4;

def set_crayonmine_points(expr inst)=
  begingroup
  save an,rext,rint,abscmine; numeric an,rext,rint,abscmine;
% an   : angle entre 2 faces
% rext : rayon du corps du crayon
% rint : rayon de la mine de graphite
% abscmine: abscisse du debut de la mine
  an:=360/nbppf;
  rext := 1/(sind (an/2));
  rint := fcmine/(sind (an/2));
  abscmine := rapp*rext;
%   origine
  set_obj_point(1,0,0,0);
%   pointe de la mine
  set_obj_point(2,abscmine*(1+lgmine),0,0);
%    points sur le corps du crayon
  set_obj_point(3,0,-1,-rext);set_obj_point(4,abscmine,-1,-rext);
  set_obj_point(5,0, 1,-rext);set_obj_point(6,abscmine, 1,-rext);
  for i:=1 upto nbppf-2:
    new_face_point(5+2*i,2+2*i,4+2*i,3+2*i,an);
    new_face_point(6+2*i,1+2*i,4+2*i,3+2*i,an);
  endfor;
%    points sur la mine de graphite
  set_obj_point(3+2*nbppf,0,-fcmine,-rint);
  set_obj_point(4+2*nbppf,abscmine*(1+lgmine*(1-rint/rext)),-fcmine,-rint);
  set_obj_point(5+2*nbppf,0, fcmine,-rint);
  set_obj_point(6+2*nbppf,abscmine*(1+lgmine*(1-rint/rext)), fcmine,-rint);
  for i:=nbppf+1 upto 2*nbppf-2:
    new_face_point(5+2*i,2+2*i,4+2*i,3+2*i,an);
    new_face_point(6+2*i,1+2*i,4+2*i,3+2*i,an);
  endfor;
%  Facteur d'echelle
%  A la sortie, le rayon du corps du crayon est 1.
  scale_obj(inst,1/rext);
  endgroup
enddef;

vardef def_crayonmine(expr inst)=
  string tmpstr;
  new_obj_points(inst,4*nbppf+2);
  new_obj_faces(inst,3*nbppf+2);
  set_crayonmine_points(inst);
  for i=1 upto nbppf-1:
%     corps du crayon
    tmpstr := decimal (2+2*i) & "," & decimal (1+2*i) & "," &
              decimal (3+2*i) & "," & decimal (4+2*i);
    set_obj_face(i,tmpstr,tablecouleur[i]);
%     mine : bois
    j := i+nbppf;
    tmpstr := decimal (2+2*j) & "," & decimal (2+2*i) & "," &
              decimal (4+2*i) & "," & decimal (4+2*j);
    set_obj_face(j,tmpstr,"FFCC88");
%     mine : graphite
    tmpstr := decimal (2+2*j) & "," & decimal (4+2*j) & "," &
              decimal (2);
    set_obj_face(j+nbppf,tmpstr,"000000");
  endfor;
%  Il faut un traitement particulier quand on revient sur la 1ere arete.
  tmpstr := decimal (2+2*nbppf) & "," &
            decimal (1+2*nbppf) & ",3,4";
  set_obj_face(nbppf,tmpstr,tablecouleur[nbppf]);
  tmpstr := decimal (2+4*nbppf) & "," &
            decimal (2+2*nbppf) & ",4," & decimal(4+2*nbppf);
  set_obj_face(2*nbppf,tmpstr,"FFCC88");
  tmpstr := decimal (2+4*nbppf) & "," &
            decimal(4+2*nbppf) & "," & decimal(2);
  set_obj_face(3*nbppf,tmpstr,"000000");
%    face arriere du crayon : bois
  tmpstr := decimal(1+2*nbppf)
            for i := nbppf-1 downto 1: & "," & decimal (1+2*i) endfor;
  set_obj_face(3*nbppf+1,tmpstr,"FFCC88");
%    face arriere du crayon : graphite
  tmpstr := decimal(1+4*nbppf)
            for i := nbppf-1 downto 1: & "," & decimal (1+2*(i+nbppf)) endfor;
  set_obj_face(3*nbppf+2,tmpstr,"000000");
  %show_obj(inst);
enddef;

def show_obj(expr inst) =
    message "Nombre de points : " & decimal(obj_points_(inst));
    for i:=1 upto obj_points_(inst):
        show_point(decimal(i) & " : ",pnt(i))
    endfor;
    message "Nombre de faces : " & decimal(obj_faces_(inst));
    for i:=1 upto obj_faces_(inst):
        message "Face " & decimal(i) & " : " & face_points_[face(i)];
        message "Couleur : " & face_color_[face(i)];
    endfor;
enddef;
