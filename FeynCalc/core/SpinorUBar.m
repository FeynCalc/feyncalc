(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinorUBar`",{"HighEnergyPhysics`FeynCalc`"}];

SpinorUBar::"usage" = "SpinorUBar[p, m] denotes a ubar-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[SpinorUBar];

   SpinorUBar /:
   MakeBoxes[SpinorUBar[p_], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];
   SpinorUBar /:
   MakeBoxes[SpinorUBar[p_,m_,___], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,",",m,")"];
   SpinorUBar /:
   MakeBoxes[SpinorUBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorUBar | \n "]];
Null
