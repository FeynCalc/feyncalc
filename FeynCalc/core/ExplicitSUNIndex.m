(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for SUN-Indices *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`",{"HighEnergyPhysics`FeynCalc`"}];

ExplicitSUNIndex::"usage"=
"ExplicitSUNIndex[ind] is a specific SU(N) index, i.e., 
ind is an integer.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

SetAttributes[ExplicitSUNIndex, {Constant, Flat, OneIdentity}];

SUNIndex = MakeContext["SUNIndex"];

ExplicitSUNIndex/:
SUNIndex[i_ExplicitSUNIndex]:= ExplicitSUNIndex[i];

   ExplicitSUNIndex /:
   MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm
            ] := p;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExplicitSUNIndex | \n "]];
Null
