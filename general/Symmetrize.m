(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* :Title: Symmetrize *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 December '98 at 16:42 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  total symmetrization *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Symmetrize`",
             "HighEnergyPhysics`FeynCalc`"];

Symmetrize::usage=
"Symmetrize[expr, {a1, a2, ...}] symmetrizes expr with respect 
to the variables a1, a2, ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Symmetrize[x_,v_List] := Block[{su},
                      su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
                      1 / Factorial[Length[v]] *
                      Plus@@Map[su[x, v, #]&, Permutations[v]]];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Symmetrize | \n "]];
Null
