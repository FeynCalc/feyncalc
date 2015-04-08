(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CrossProduct *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 December '98 at 16:48 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

CrossProduct::usage=
"CrossProduct[a, b] denotes the three-dimensional cross-product.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CrossProduct`Private`"]

(* antisymmetry *)
CrossProduct[b_, a_] := - CrossProduct[a, b] /; !OrderedQ[{b,a}];

(* linearity *)
CrossProduct[a_Plus, b_] := Map[CrossProduct[#, b]&, a];
CrossProduct[(n_/;NumberQ[n]) a_, b_] := n CrossProduct[a, b];

CrossProduct[a_ThreeVector, CrossProduct[b_, c_]] :=
DotProduct[a, c] b - DotProduct[a, b] c;

CrossProduct[CrossProduct[b_, c_], a_ThreeVector] :=
-(DotProduct[a, c] b - DotProduct[a, b] c);

CrossProduct /: MakeBoxes[CrossProduct[a_,b_], TraditionalForm] :=
TBox[a,"\[Cross]",b];

FCPrint[1,"CrossProduct.m loaded."];
End[]
