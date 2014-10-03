(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Expand2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed 19th July 2000 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Expand2`",{"HighEnergyPhysics`FeynCalc`"}];

Expand2::"usage"=
"Expand2[exp, x] expands all sums containing x.
Expand2[exp, {x1, x2, ...}]  expands all sums containing x1, x2, ....";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[FreeQ2];

Expand2[x_] := Expand[x/. y_ ^ n_ /; Head[n]=!=Integer :> pow[y,n]
                     ]/.pow->Power;

Expand2[x_, a_ /; Head[a] =!= List] := Expand2[x, {a}];
Expand2[x_, l_List] :=
If[FreeQ[x, Plus], x,
   Block[{pl, t, plus},
         pl[y__] := If[FreeQ2[{Hold[y]}, l], plus[y], Plus[y]];
         t = Expand[x /. Plus -> pl]//.plus->Plus /. pl -> Plus;
         t
        ]
  ];

(*
If$VersionNumber === 2.3, Print["use Expand instead of Expand2 "];
   Expand[x],
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Expand2 | \n "]];
Null
