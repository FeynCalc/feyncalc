(* :Summary: Dirac spinors *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSpinor`",
               "HighEnergyPhysics`FeynCalc`"];

DiracSpinor::"usage" =
"DiracSpinor[p, m, ind] is a Dirac spinor for a fermion with momentum p \
and mass m and indices ind. DiracSpinor is the same as Spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative, Spinor];

DiracSpinor=Spinor;

DeclareNonCommutative[DiracSpinor];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSpinor| \n "]];
Null
