(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GS`",{"HighEnergyPhysics`FeynCalc`"}];

GS::"usage"=
"GS[p] is transformed into DiracSlash[p] by FeynCalcInternal.
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GS];

GS[DOT[x_,y__]] := Map[GS,DOT[x,y]];

GS[x_, y__] := DOT @@ Map[GS,{x,y}];

GS/:
  MakeBoxes[ GS[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GS/:
  MakeBoxes[ GS[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

gsg[a_]:=If[FreeQ[y, Plus], Tbox["\[Gamma]", a],
                            Tbox["\[Gamma]", "(",a,")"]
           ];

GS/:
  MakeBoxes[ GS[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GS | \n "]];
Null
