(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: derivation of feynman rules via functional differentiation *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`QuantumField`",
             "HighEnergyPhysics`FeynCalc`"];

QuantumField::"usage"=
"QuantumField[par1, par2, ..., ftype, {lorind}, {sunind}]
denotes a quantum field of type ftype with (possible)
Lorentz-indices lorind and SU(N)-indices sunind.
the optional first argument par1, par2, ...,  are partial
derivatives (PartialD) acting on the field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Momentum, LorentzIndex, OPEDelta, PartialD, SUNIndex];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[QuantumField];

lori[OPEDelta] := Momentum[OPEDelta];
lori[a_SUNIndex] := a;
lori[a_] := LorentzIndex[a];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo},SUNIndex/@{suli}];

QuantumField[f1_QuantumField] := f1;


QuantumField /:
   MakeBoxes[ QuantumField[a_][p_], TraditionalForm
            ] := Tbox[a,"(",p,")"];

QuantumField /:
   MakeBoxes[ QuantumField[a_], TraditionalForm
            ] := Tbox[a];

QuantumField /:
   MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
            ] := SubscriptBox[Tbox[f], Tbox[mu]] /;
                   (lo === LorentzIndex || lo === ExplicitLorentzIndex);

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]
                                  ] /; MatchQ[lo, LorentzIndex | Momentum
                                             ] && sun === SUNIndex;

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ][p_], TraditionalForm
            ] := RowBox[{
           SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]],
                         "(", Tbox[p], ")"
                        }
                       ] /; MatchQ[lo, LorentzIndex | Momentum] &&
                            sun === SUNIndex;
QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_], a_,
 (lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
 lori___HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex),
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SubscriptBox["\[PartialD]", Tbox[pa]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_]^m_, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                      }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 (lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
 lori___HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex),
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuantumField | \n "]];
Null
