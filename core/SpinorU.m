(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinorU`",{"HighEnergyPhysics`FeynCalc`"}];

SpinorU::"usage" = "SpinorU[p, m] denotes a u-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[SpinorU];

   SpinorU /:
    MakeBoxes[SpinorU[p_], TraditionalForm] := Tbox["u","(",p,")"];
   SpinorU /:
    MakeBoxes[SpinorU[p_,m_,___], TraditionalForm] :=
    Tbox["u","(",p,",",m,")"];
   SpinorU /:
    MakeBoxes[SpinorU[p_,0,___], TraditionalForm] := Tbox["u","(",p,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorU | \n "]];
Null
