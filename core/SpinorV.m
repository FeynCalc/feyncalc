(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinorV`",
             "HighEnergyPhysics`FeynCalc`"];

SpinorV::"usage" = "SpinorV[p, m] denotes a v-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[SpinorV];

   SpinorV /:
   MakeBoxes[SpinorV[p__], TraditionalForm] := Tbox["v","(",p,")"];
   SpinorV /:
   MakeBoxes[SpinorV[p_,m_,___], TraditionalForm] :=
   Tbox["v","(",p,",",m,")"];
   SpinorV /:
   MakeBoxes[SpinorV[p_,0,___], TraditionalForm] := Tbox["v","(",p,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorV | \n "]];
Null
