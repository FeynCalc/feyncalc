(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSplit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2 July '97 at 13:43 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: split one into two *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynAmpDenominatorSplit`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmpDenominatorSplit::usage = 
"FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...]
into FeynAmpDenominator[a]*FeynAmpDenominator[b] ... .
FeynAmpDenominatorSplit[expr, q1]
splits every FeynAmpDenominator in expr into
a product of two, one containing q1 and other momenta,
the second without q1.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[ Momentum, FeynAmpDenominator, 
             FeynCalcInternal, PropagatorDenominator,
             Select1, Select2 ];

(*
feynsplit[_][] := 1;
*)
feynsplit[k1_][a__PropagatorDenominator] :=
(
  (FeynAmpDenominator@@Select1[{a}, k1]) *
  (FeynAmpDenominator@@Select2[{a}, k1])
) /.FeynAmpDenominator[]->1 ;

fsp[a__] := Apply[Times, Map[FeynAmpDenominator, {a}]];

FeynAmpDenominatorSplit[x_] := 
   FeynCalcInternal[x] /. FeynAmpDenominator -> fsp;

FeynAmpDenominatorSplit[x_, q1_Momentum] := 
 FeynAmpDenominatorSplit[x, q1[[1]]];

HoldPattern[FeynAmpDenominatorSplit[x_, q1_]] := 
 FeynCalcInternal[x] /. FeynAmpDenominator -> feynsplit[q1] /.
    feynsplit[q1]->FeynAmpDenominator;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpDenominatorSplit | \n "]];
Null
