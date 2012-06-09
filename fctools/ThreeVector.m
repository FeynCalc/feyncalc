(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ThreeVector *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 December '98 at 17:11 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ThreeVector *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ThreeVector`",
             "HighEnergyPhysics`FeynCalc`"];

ThreeVector::"usage" = 
"ThreeVector[p] is the three dimensional vector p.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ThreeVector /: MakeBoxes[ThreeVector[p_], TraditionalForm] :=
(* RM: changed Global`OverVector to OverVector 20100119 *)
 MakeBoxes[OverVector[p], TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ThreeVector | \n "]];
Null
