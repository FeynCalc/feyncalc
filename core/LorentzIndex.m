
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Title LorentzIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Lorentz index *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LorentzIndex`",
             "HighEnergyPhysics`FeynCalc`"];

LorentzIndex::"usage"= "LorentzIndex is the head of Lorentz indices.
The internal representation of a four-dimensional mu is
LorentzIndex[mu]. For other than four dimensions:
LorentzIndex[mu, Dimension].
LorentzIndex[mu, 4] simplifies to LorentzIndex[mu].
If the first argument is an integer, LorentzIndex[i] turns into
ExplicitLorentzIndex[i].";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

 MakeContext[ExplicitLorentzIndex];

SetAttributes[LorentzIndex, Constant ];
(* expanded because of CreateFCAmp's strange results  ... *)
LorentzIndex[LorentzIndex[in_,d___], d___]  := LorentzIndex[in,d];
LorentzIndex[x_, 4]              := LorentzIndex[x, 4] = LorentzIndex[x];
LorentzIndex[_, 0]               := 0;
LorentzIndex[in_Integer,dim___]  := ExplicitLorentzIndex[in,dim];

LorentzIndex /:
   MakeBoxes[ LorentzIndex[p_, in___], TraditionalForm
            ] := If[$LorentzIndices =!= True,
                    ToBoxes[p,TraditionalForm],
                    If[{in} === {},
                       MakeBoxes[p, TraditionalForm],
                       SubscriptBox[ToBoxes[p, TraditionalForm],
                                    ToBoxes[in, TraditionalForm]
                                   ]
                      ]
                   ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LorentzIndex | \n "]];
Null

