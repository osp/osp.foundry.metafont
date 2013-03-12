%  degrade de couleurs
% 1 -> bleu+rouge --> rouge+vert
% 2 -> rouge+vert --> vert+bleu
% 3 -> vert+bleu --> vert+rouge

string tablecouleur[];

numeric r,g,b,ntmp;

% On commence par ranger les couleurs dans un tableau trop grand ; on le
% dimensionne ensuite.
% nbppf est le nombre de points sur les extremites du crayon, et par
% consequent est aussi le nombre de faces sur le corps du crayon.

ntmp := 3*(1+floor(nbppf/3));

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

for i:= 1 upto ntmp/3:
  g := round(255*3*i/ntmp);
  b := round(255*(ntmp-3*i)/ntmp);
  tablecouleur[i] := "FF" & dec_to_hex(g) & dec_to_hex(b);
endfor;
for i:= 1 upto ntmp/3:
  r := round(255*(ntmp-3*i)/ntmp);
  b := round(255*3*i/ntmp);
  tablecouleur[i+ntmp/3] := dec_to_hex(r) & "FF" & dec_to_hex(b);
endfor;
for i:= 1 upto ntmp/3:
  r := round(255*3*i/ntmp);
  g := round(255*(ntmp-3*i)/ntmp);
  tablecouleur[i+2*ntmp/3] := dec_to_hex(r) & dec_to_hex(g) & "FF";
endfor;

for i:=1 upto nbppf:
  tablecouleur[i]:=tablecouleur[floor(i*ntmp/nbppf)];
endfor;
