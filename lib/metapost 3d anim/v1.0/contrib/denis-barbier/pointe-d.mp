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

% Choix des couleurs
input arcenciel
%input arcenciel2

% set scale
drawing_scale:=8cm;

%
filled_faces:=true; % default value
%filled_faces:=false;

draw_contours:=false;

%
show_animation_parameters:=true; % default value is |false|

def one_image(expr name,i,a,rd,ang)=
  beginfig(i);
    set_point(Obs,0,-100,0);
    Obs_phi:=90;Obs_dist:=10;
    point_of_view_obj(name,1,Obs_phi);
    draw_obj(name);
    rotate_obj_pv(name,1,vect_I,ang);
  endfig;
enddef;

% animations
animate_object("crayonmine",1,30,100);

end
