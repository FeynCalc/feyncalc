(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkSpinor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: QuarkSpinor denotes spinors *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`QuarkSpinor`",
             "HighEnergyPhysics`FeynCalc`"];

QuarkSpinor::usage = "QuarkSpinor is equivalent to Spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[QuarkSpinor, ReadProtected];

QuarkSpinor := QuarkSpinor =  MakeContext["Spinor"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkSpinor | \n "]];
Null
