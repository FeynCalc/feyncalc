(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Solve2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Solve2 is like Solve, but only for linear equations *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Solve2`",
             "HighEnergyPhysics`FeynCalc`"];

Solve2::"usage"=
"Solve2 is equivalent to Solve, except that it works only for
linear equations (and returns just a list)
and accepts the options Factoring and FinalSubstitutions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
MakeContext[ Collect2, Expanding , Factor1,Factor2,
             Factoring, FinalSubstitutions,Isolate, 
             IsolateHead 
           ];

Options[Solve2] = {Factoring -> Factor2, FinalSubstitutions -> {}};

Solve2[a_/;Head[a]=!=List, b__] := Solve2[{a}, b];
Solve2[a_, b_/;Head[b]=!=List, c___] := Solve2[a, {b}, c];

Solve2[ai_List, bii_, ops___Rule] := Block[{fixeq, temp, re, 
    factor , finsub, a, b, bi,dumsub, dum},
bi = Flatten[{bii}];
dumsub = Table[bi[[ij]] -> dum[ij],{ij,Length[bi]}];
a = Flatten[{ai}] /. dumsub;
b = Last/@dumsub;
factor = Factoring /. {ops} /. Options[Solve2];
finsub = FinalSubstitutions/. {ops} /. Options[Solve2];
fixeq[x_]:= Isolate[Collect2[
  If[Head[x] === Equal, x[[1]] - x[[2]], x],
                     b, Factoring -> factor ,Expanding -> False
                            ], b, IsolateHead->$soso];
temp = Map[fixeq, a];
(*
soback /: HoldForm[soback[i_]] := soback[i];
*)
re = (Solve[Map[(# == 0)&, temp], b][[1]]);
(*
re = FixedPoint[(# /. $soso -> soback /. soback -> $soso
                )&, re];
*)
re = FixedPoint[ReleaseHold,re]/.Map[Reverse,dumsub];

If[factor === False, 
   re = re /. finsub,
   re = Map[(#[[1]] -> factor[(#[[2]])/.finsub])&, re];
  ];
 re];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Solve2 | \n "]];
Null
