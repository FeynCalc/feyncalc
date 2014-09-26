(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracMatrix`",{"HighEnergyPhysics`FeynCalc`"}];


DiracMatrix::"usage" =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m. \
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz \
indices m1, m2, etc. DiracMatrix[5] is gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
Dimension = MakeContext["CoreOptions","Dimension"];

MakeContext[ DeclareNonCommutative, DiracGamma, LorentzIndex,
             ExplicitLorentzIndex];

fci  := fci = MakeContext["FeynCalcInternal"];

Options[DiracMatrix] = {Dimension -> 4, fci -> True};

DeclareNonCommutative[DiracMatrix];

DiracMatrix[a_Integer] := DiracGamma[a];

(* 12/1-2002. Comment by F.Orellana:
   Don't know why Rolf provided this alternative input method (below).
   Think it's better to use DiracMatrix[a,b,c,...],
   which will be translated by FCI anyway as soon as
   Contract, DiracSimplify, ... is applied.
   With this alternative input method, integers are wrapped
   in ExplicitLorentzIndex, prohibiting DiracSimplify from working
   (could of course easily be fixed). *)

DiracMatrix[DOT[a_,b__], opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]&, DOT[a,b]];

DiracMatrix[a_, opt___Rule] := (DiracGamma[LorentzIndex[a,
  Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]
                               ) /; Head[a] =!= Integer;

   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_], TraditionalForm
            ] := SuperscriptBox["\[Gamma]",
                                MakeBoxes[x, TraditionalForm]
                               ];
   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_,y___,z_,___Rule],
             TraditionalForm
            ] := RowBox @ Map[
                 SuperscriptBox["\[Gamma]",
                                MakeBoxes[#, TraditionalForm]
                               ]&,
                              {x,y,z}
                             ] /; Head[z]=!=Rule;
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracMatrix | \n "]];
Null
