(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PauliSigma stands for the vector of Pauli matrices
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PauliSigma`",
             "HighEnergyPhysics`FeynCalc`"];

PauliSigma::"usage" =
"PauliSigma denotes the vector of the 3 Pauli matrices.
PauliSigma[1], PauliSigma[2], PauliSigma[3] give the
explicit Pauli matrices. PauliSigma[] yields
{PauliSigma[1], PauliSigma[2], PauliSigma[3]}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative[PauliSigma];

PauliSigma[1] = { {0, 1}, {1,0} };
PauliSigma[2] = { {0,-I}, {I,0} };
PauliSigma[3] = { {1, 0}, {0,-1}};

PauliSigma[] = {PauliSigma[1], PauliSigma[2], PauliSigma[3]};

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PauliSigma | \n "]];
Null
