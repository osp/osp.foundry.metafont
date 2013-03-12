%    arc-en ciel
% Il faut decouper en 3 zones de couleurs :
% 1 -> rouge --> rouge+vert
% 2 -> rouge+vert --> vert+bleu
% 3 -> vert+bleu --> vert
%   Ensuite, on symetrise

string tablecouleur[];

numeric r,g,b,ntmp;

% On commence par ranger les couleurs dans un tableau trop grand ; on le
% dimensionne ensuite.
% nbppf est le nombre de points sur les extremites du crayon, et par
% consequent est aussi le nombre de faces sur le corps du crayon.
ntmp := 6*(1+floor(nbppf/6));

% J'ai besoin de transformer un nombre 0<=j<=255 en sa
% representation hexadecimale.
def dec_to_hex(expr j)=
  res_dec_to_hex(j div 16) & res_dec_to_hex(j mod 16)
enddef;
def res_dec_to_hex(expr j)=
  if j<10: decimal j
  else: char(55+j)
  fi
enddef;

for i:= 1 upto ntmp/6:
  g := round(255*6*i/ntmp);
  tablecouleur[i] := "FF" & dec_to_hex(g) & "00";
  tablecouleur[ntmp+1-i] := tablecouleur[i];
endfor;
for i:= 1 upto ntmp/6:
  r := round(255*(ntmp-6*i)/ntmp);
  b := round(255*6*i/ntmp);
  j := i+ntmp/6;
  tablecouleur[j] := dec_to_hex(r) & "FF" & dec_to_hex(b);
  tablecouleur[ntmp+1-j] := tablecouleur[j];
endfor;
for i:= 1 upto ntmp/6:
  g := round(255*(ntmp-6*i)/ntmp);
  j := i+ntmp/3;
  tablecouleur[j] := "00" & dec_to_hex(g) & "FF";
  tablecouleur[ntmp+1-j] := tablecouleur[j];
endfor;

for i:=1 upto nbppf:
  tablecouleur[i]:=tablecouleur[floor(i*ntmp/nbppf)];
endfor;
