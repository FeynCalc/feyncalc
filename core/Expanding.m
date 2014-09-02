(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Expanding *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Expanding`",{"HighEnergyPhysics`FeynCalc`"}];

Expanding::"usage" =
"Expanding is an option for DotSimplify, Calc, Contract, DiracSimplify, SUNSimplify, etc.
As option for Contract it specifies whether expansion w.r.t.
LorentzIndex is done BEFORE contraction. \n
If set to False in DiracSimplify or SUNSimplify,
only a limited set of simplifications
(multiplicative linearity etc.) is
performed. For DotSimplity, it determines
whether noncommutative expansion is done.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Expanding | \n "]];
Null
