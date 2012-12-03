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

def affiche_un(expr name, n,a) =
    numeric v_a;v_a:=new_vect;
    vect_def_vect(v_a,vect_K);
    vect_mult(v_a,v_a,n);
    rotate_obj_abs_pv(name,v_a,vect_K,a);
    free_vect(v_a); 
enddef;

numeric angleb,anglec;
angleb=anglec=0;

def one_image_three_objects(expr name_a,name_b,name_c,i,a,rd,ang)=
  beginfig(i);
    set_point(Obs,0,-rd,10);
    Obs_phi:=90;Obs_dist:=1;
    point_of_view_obj(name_a,1,Obs_phi);
    angleb := (angleb+2*ang) mod 360;
    anglec := (anglec-ang) mod 360;
    rotate_obj_pv(name_a,1,vect_K,ang);
    rotate_obj_pp(name_b,1,2,ang);
    rotate_obj_pp(name_c,1,2,ang);
    affiche_un(name_b,2,2*ang);
    affiche_un(name_c,6,-ang);
    if (anglec<180):
       draw_obj(name_c);
       if (angleb<180):
         draw_obj(name_b);
         draw_obj(name_a);
       else:
         draw_obj(name_a);
         draw_obj(name_b);
       fi;
    else:
       if (angleb<180):
         draw_obj(name_b);
         draw_obj(name_a);
       else:
         draw_obj(name_a);
         draw_obj(name_b);
       fi;
       draw_obj(name_c);
    fi;
  endfig;
enddef;

vardef animate_three_identical_objects(expr name_a,imin,imax,index)=
  numeric ang;ang=360/(imax-imin+1);
  assign_obj("obja",name_a);assign_obj("objb",name_a);
  assign_obj("objc",name_a);
  numeric v_a;v_a:=new_vect;
  put_obj("obja",vect_null,1,0,-90,0);
  vect_def_vect(v_a,vect_K);vect_sum(v_a,v_a,vect_I);
  vect_mult(v_a,v_a,2);
  put_obj("objb",v_a,1,0,0,0);
  vect_def_vect(v_a,vect_K); vect_mult(v_a,v_a,6);
  vect_sum(v_a,v_a,vect_I);
  vect_sum(v_a,v_a,vect_I);
  put_obj("objc",v_a,1,0,0,0);
  free_vect(v_a);
  for i:=imin upto imax:
    one_image_three_objects("obja","objb","objc",i+index,i,20,ang);
  endfor;
  show_animation_bbox;
enddef;

animate_three_identical_objects("crayonmine",1,50,100);

end
