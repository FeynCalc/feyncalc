(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracGammaT  denotes the a transposed DiracGamma *)
(* :Comments: still experimental !!!  check SUSY-calculations *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracGammaT`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGammaT::"usage" =
"DiracGammaT[x] denotes the transpose of DiracGamma. \
Transpose[DiracGammaT[x]] gives DiracGamma[x]. \
Note that x must have Head LorentzIndex or Momentum.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DeclareNonCommutative, DiracGamma, LorentzIndex, Momentum];

DeclareNonCommutative[DiracGammaT];

DiracGammaT /: Transpose[DiracGammaT[a__]] := DiracGamma[a];

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SubsuperscriptBox["\[Gamma]", Tbox[a], "T"] /; (Head[a] ===
               LorentzIndex) || (Head[a] === Integer);

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SuperscriptBox[Tbox["(","\[Gamma]", "\[CenterDot]",
                                   a, ")"], "T"] /; Head[a] === Momentum;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGammaT | \n "]];
Null
