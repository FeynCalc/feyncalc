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

ThreeVector::usage = 
"ThreeVector[p] is the three dimensional vector p.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[ThreeVector, ReadProtected];

ThreeVector /: MakeBoxes[ThreeVector[p_], TraditionalForm] :=
 MakeBoxes[Global`OverVector[p], TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ThreeVector | \n "]];
Null
