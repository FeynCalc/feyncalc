(* :Title: AntiSymmetrize *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 December '98 at 16:41 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  total symmetrization *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`AntiSymmetrize`",
             "HighEnergyPhysics`FeynCalc`"];

AntiSymmetrize::usage=
"AntiSymmetrize[expr, {a1, a2, ...}] antisymmetrizes expr with respect
to the variables a1, a2, ... ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[AntiSymmetrize, ReadProtected];

AntiSymmetrize[x_,v_List] := Block[{su},
                      su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
                      1 / Factorial[Length[v]] *
                      Plus@@Map[(Signature[#] su[x,v,#])&, Permutations[v]]];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "AntiSymmetrize | \n "]];
Null

