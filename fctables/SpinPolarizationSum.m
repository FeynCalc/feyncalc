(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinPolarizationSum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`SpinPolarizationSum`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[SpinPolarizationSum, ReadProtected];

SpinPolarizationSum::usage=
"SpinPolarizationSum is an option for SquareAmplitude and
FermionSpinSum.
The set (pure) function  acts on the usual spin-sum.";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinPolarizationSum | \n "]];
Null
