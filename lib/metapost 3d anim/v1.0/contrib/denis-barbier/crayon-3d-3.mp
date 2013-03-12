% Example d'utilisation de 3d.mp
% Crayon a papier colores
% METAPOST
% D. Barbier  5 novembre 1997
% Utilise 3d.mp v1.0
%         crayonmine.mp v0.2

% package 3d
input 3d-1.0

% Definition des crayons
input crayonmine

nbppf:=6;

% Choix des couleurs
%input arcenciel
input arcenciel2

% set scale
drawing_scale:=8cm;

%filled_faces:=false; % default value is |true|

draw_contours:=false;

% show_animation_parameters:=true; % default value is |false|

def animate_two_identical_objects(expr name,imin,imax,index)=
  save v_a;
  numeric v_a;
  numeric ang;ang=360/(imax-imin+1);
  assign_obj("obja",name);assign_obj("objb",name);
  v_a=new_vect;
  rotate_obj_pv("objb",1,vect_J,45);
  vect_mult(v_a,vect_K,rapp*(1+lgmine));
  translate_obj("objb",v_a);
  free_vect(v_a);
  for i:=imin upto imax:
    one_image_two_identical_objects("obja","objb",i+index,i,10,ang);
  endfor;
  show_animation_bbox;
enddef;

def one_image_two_identical_objects(expr name_a,name_b,i,a,rd,ang)=
  beginfig(i);
    set_point(Obs,0,-20,4);
    Obs_phi:=90;Obs_dist:=1;
    point_of_view_obj(name_a,1,Obs_phi);% fix point 1 of object |name_a|
    draw_obj(name_a);draw_obj(name_b);
    rotate_obj_pp(name_b,1,2,ang);
    rotate_obj_abs_pv(name_a,point_null,vect_K,2*ang);
    rotate_obj_abs_pv(name_b,point_null,vect_J,ang);
  endfig;
enddef;

% animations
animate_two_identical_objects("crayonmine",1,60,100);

end
