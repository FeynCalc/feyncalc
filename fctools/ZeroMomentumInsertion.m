(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ZeroMomentumInsertion*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ZeroMomentumInsertion`",
             "HighEnergyPhysics`FeynCalc`"];

ZeroMomentumInsertion::usage= 
"ZeroMomentumInsertion is an option of FeynRule.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[ZeroMomentumInsertion, ReadProtected];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ZeroMomentumInsertion | \n "]];
Null
