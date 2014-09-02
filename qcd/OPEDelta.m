(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEDelta*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the variable selecting out the ope-insertions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEDelta`",
             {"HighEnergyPhysics`FeynCalc`"}];

OPEDelta::"usage"= "OPEDelta is the Delta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

LorentzIndex  = MakeContext["LorentzIndex"];
Momentum      = MakeContext["Momentum"];
Pair          = MakeContext["Pair"];
ScalarProduct = MakeContext["ScalarProduct"];

ScalarProduct[OPEDelta, OPEDelta, ___Rule] = 0;
Pair[Momentum[OPEDelta,___], Momentum[OPEDelta,___]] = 0;

(* that is only for Partial* stuff *)
LorentzIndex[Momentum[OPEDelta]^p_.] := Momentum[OPEDelta]^p;

OPEDelta/:
   MakeBoxes[OPEDelta, TraditionalForm] := "\[CapitalDelta]";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEDelta | \n "]];
Null
