(* :Summary: left and right handed projectors *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ChiralityProjector`",
               "HighEnergyPhysics`FeynCalc`"];

ChiralityProjector::"usage" =
"ChiralityProjector[+1] denotes DiracGamma[6] (=1/2(1 + DiracMatrix[5])).
ChiralityProjector[-1] denotes DiracGamma[7] (=1/2(1 - DiracMatrix[5])).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative, DiracGamma];

DeclareNonCommutative[ChiralityProjector];

ChiralityProjector[1] = DiracGamma[6];
ChiralityProjector[-1] = DiracGamma[7];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "+"];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[-1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "-"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ChiralityProjector | \n "]];
Null
