(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 15 December '97 at 17:56 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`DiracSimplify2`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSimplify2::"usage"=
"DiracSimplify2[exp] simplifies the Dirac structure but leaves \
any DiracGamma[5] untouched. \
DiracGamma[6] and DiracGamma[7] are inserted by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Cases2,DiracGammaExpand,DiracSimplify,
            DotSimplify,Contract,DOT,DiracGamma, FeynCalcInternal];

DiracSimplify2[exp_] := Block[{nn,tt},
If[FreeQ[exp,DOT], exp,
nn = DotSimplify[DiracGammaExpand[FeynCalcInternal[exp]] /.
                  DiracGamma[6] -> (1/2 + DiracGamma[5]/2) /.
                  DiracGamma[7] -> (1/2 - DiracGamma[5]/2)
                ];
If[FreeQ[nn, DiracGamma[5]], DiracSimplify[nn],

tt = Cases2[nn, DOT];

nn/.(
Table[tt[[ij]] ->
      DotSimplify[
      Contract[ tt[[ij]] //.
       {doot[a__, DiracGamma[5], b__] :>
       If[FreeQ[{a}, DiracGamma[5] ],
                     DiracSimplify[DOT[a]],
                     DiracSimplify2[DOT[b]]
         ] .
             DiracGamma[5] .
       If[FreeQ[{b}, DiracGamma[5] ],
                     DiracSimplify2[DOT[a]],
                     DiracSimplify[DOT[b]]
         ]
       }      ]], {ij,Length[tt]}

     ])
  ] ]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimplify2 | \n "]];
Null