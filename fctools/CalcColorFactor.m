(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CalcColorFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2nd of November 2003 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CalcColorFactor *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`CalcColorFactor`",{"HighEnergyPhysics`FeynCalc`"}];

CalcColorFactor::"usage" =
"CalcColorFactor[expr] calculates the color factor of expr. 
CalcColorFactor is useful for application on FeynArts produced amplitudes.
CalcColorFactor is just a macro function for
CalcColorFactor[x_] := If[FreeQ2[FeynCalcInternal[x], SUNIndex], 
   x, SUNSimplify[SUNSimplify[
     (If[ !FreeQ[#1, DiracGamma], DiracTrick[#1], #1] & )[
      SUNSimplify[x]], Explicit -> False], Explicit -> True]].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[CalcColorFactor, Listable]; 

MakeContext[
DiracGamma,
DiracTrick,
Explicit,
FeynCalcInternal,
FreeQ2,
SUNIndex,
SUNNToCACF,
SUNSimplify
]

Options[CalcColorFactor] = {SUNNToCACF -> True};

SetAttributes[CalcColorFactor, Listable];

CalcColorFactor[x_Plus, opts___?OptionQ] := CalcColorFactor[#, opts]& /@ x;

CalcColorFactor[x_, opts___?OptionQ] := Module[{tmp = FeynCalcInternal[x]},
 If[FreeQ[tmp, SUNIndex], tmp, 
    If[Head[tmp]=!=Times, fac = 1, fac = Select[tmp, FreeQ[#, SUNIndex]&]];
    fac SUNSimplify[SUNSimplify[
    (If[ !FreeQ[#1, DiracGamma], DiracTrick[#1], #1] & )[
     SUNSimplify[tmp/fac]], Explicit -> False], Explicit -> True]]
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CalcColorFactor| \n "]];
Null
