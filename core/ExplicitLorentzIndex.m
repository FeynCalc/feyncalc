(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Lorentz indices of integers *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ExplicitLorentzIndex::"usage"=
"ExplicitLorentzIndex[ind] is an explicit Lorentz index, i.e., ind is
an integer.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*Commented out 18/6-2002 by F.Orellana. Don't know why it was there - it causes infinite recursion*)
(*MakeContext[ExplicitLorentzIndex];*)

SetAttributes[ExplicitLorentzIndex, Constant ];

ExplicitLorentzIndex /:
   MakeBoxes[ ExplicitLorentzIndex[p_, in___], TraditionalForm
            ] := p;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExplicitLorentzIndex | \n "]];
Null
