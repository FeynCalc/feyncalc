(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DeclareNonCommutative`",{"HighEnergyPhysics`FeynCalc`"}];

DeclareNonCommutative::"usage" =
"DeclareNonCommutative[a, b, ...] declares a,b, ... to be \
noncommutative, i.e., DataType[a,b, ...,  NonCommutative] is set to \
True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative[] := soso /;
Message[DeclareNonCommutative::argrx, DeclareNonCommutative, 0, "1 or more"];

DeclareNonCommutative[b__] :=
 (Map[Set[HighEnergyPhysics`FeynCalc`DataType`DataType[#,
          HighEnergyPhysics`FeynCalc`CoreObjects`NonCommutative],
          True]&, {b}
     ]; Null);

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeclareNonCommutative | \n "]];
Null
