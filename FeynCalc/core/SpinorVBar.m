(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinorVBar`",{"HighEnergyPhysics`FeynCalc`"}];

SpinorVBar::"usage" = "SpinorVBar[p, m] denotes a vbar-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[SpinorVBar];

   SpinorVBar /:
   MakeBoxes[SpinorVBar[p__], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];
   SpinorVBar /:
   MakeBoxes[SpinorVBar[p_,m_,___], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,",",m,")"];
   SpinorVBar /:
   MakeBoxes[SpinorVBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorVBar | \n "]];
Null
