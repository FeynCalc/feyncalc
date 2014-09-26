(* ************************************************************** *)
(*                                                                *)
(*                      FCBoxes.m                                 *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              F.Orellana 2000

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, PHI

   Description:         The definitions in this file modifies the
                        way QuantumFields are displayed, so that
                        outputs of calculations with PHI look nicer.

*)

(* ************************************************************** *)

(* Box definitions for FeynCalc *)

ClearAll[HighEnergyPhysics`FeynCalc`CoreObjects`QuantumField];

BeginPackage["HighEnergyPhysics`FeynCalc`QuantumField`",
             {"HighEnergyPhysics`FeynCalc`"}];

Begin["`Private`"];

ExplicitSUNIndex = MakeContext["CoreObjects","ExplicitSUNIndex"];

QuantumField /:
MakeBoxes[ QuantumField[a_][_], TraditionalForm
            ] := Tbox[a](*[[1]]*);

QuantumField /:
MakeBoxes[ QuantumField[f_, lo_[mu_,___]][_], TraditionalForm
            ] := SubscriptBox[Tbox[f](*[[1]]*), Tbox[mu]] /;
                   lo === LorentzIndex ||
                   lo === HighEnergyPhysics`Phi`Objects`UIndex;

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ][_], TraditionalForm
            ] :=
SubsuperscriptBox[Tbox[f](*[[1]]*), Tbox[lori], Tbox[suni]]/;
           MatchQ[lo, LorentzIndex | Momentum | HighEnergyPhysics`Phi`Objects`UIndex] &&
                  (sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`CoreObjects`PartialD[pa_], a_,
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ][_],
              TraditionalForm] :=
RowBox[{SubscriptBox["\[PartialD]" , Tbox[pa]],
SubsuperscriptBox[Tbox[a](*[[1]]*), Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`CoreObjects`PartialD[pa_]^m_, a_,
 (*lori___HighEnergyPhysics`FeynCalc`CoreObjects`Momentum,*)
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ][_],
              TraditionalForm] :=
              RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
SubsuperscriptBox[Tbox[a](*[[1]]*), Tbox[lori], Tbox[suni]]
                      }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`CoreObjects`PartialD, a_,
 (*lori___HighEnergyPhysics`FeynCalc`CoreObjects`Momentum,*)
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ][_],
              TraditionalForm
            ] := RowBox[{TBox[pa],
SubsuperscriptBox[Tbox[a](*[[1]]*), Tbox[lori], Tbox[suni]]
                        }];

(*----------------------------------------------------------------*)

QuantumField /:
   MakeBoxes[ QuantumField[f_,suni:sun_[_]..
                          ][_], TraditionalForm] :=
      SuperscriptBox[Tbox[f](*[[1]]*),Tbox[suni]] /;
      (sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
   MakeBoxes[ QuantumField[f_,suni:sun_[_]..
                          ], TraditionalForm
            ] := SuperscriptBox[Tbox[f](*[[1]]*),Tbox[suni]
                                  ] /;
       (sun === SUNIndex || sun === ExplicitSUNIndex);

(*----------------------------------------------------------------*)

(* Modified original FeynCalc definitions*)

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
 QuantumField@@Join[{f,g},LorentzIndex/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
 QuantumField@@Join[{f,g},LorentzIndex/@{lilo},SUNIndex/@{suli}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}, {ui___}] :=
 QuantumField@@Join[{f,g},LorentzIndex/@{lilo},SUNIndex/@{suli},
                          HighEnergyPhysics`Phi`Objects`UIndex/@{ui}];

QuantumField[f1_QuantumField] := f1;


QuantumField /:
   MakeBoxes[ QuantumField[a_][_], TraditionalForm
            ] := Tbox[a](*[[1]]*);

QuantumField /:
   MakeBoxes[ QuantumField[a_], TraditionalForm
            ] := Tbox[a](*[[1]]*);

QuantumField /:
   MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
            ] := SubscriptBox[Tbox[f](*[[1]]*), Tbox[mu]] /;
                   lo === LorentzIndex ||
                   lo === HighEnergyPhysics`Phi`Objects`UIndex;

QuantumField /:
    MakeBoxes[
      QuantumField[f_,
        lol : (((HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex|
                 HighEnergyPhysics`Phi`Objects`UIndex)[_, ___])..)],
        TraditionalForm] := SubscriptBox[Tbox[f][[1(*, 1*)]], Tbox[lol]];

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
     ], TraditionalForm
 ] := SubsuperscriptBox[Tbox[f](*[[1]]*), Tbox[lori], Tbox[suni]
        ] /; MatchQ[lo, LorentzIndex | Momentum | HighEnergyPhysics`Phi`Objects`UIndex
               ] && (sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
    ][_], TraditionalForm
 ] := SubsuperscriptBox[Tbox[f](*[[1]]*), Tbox[lori], Tbox[suni]] /;
        MatchQ[lo, LorentzIndex | Momentum | HighEnergyPhysics`Phi`Objects`UIndex] &&
                    (sun === SUNIndex || sun === ExplicitSUNIndex);
QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`CoreObjects`PartialD[pa_], a_,
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ],
              TraditionalForm
       ] := RowBox[{SubscriptBox["\[PartialD]" , Tbox[pa]],
        SubsuperscriptBox[Tbox[a](*[[1]]*), Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`CoreObjects`PartialD[pa_]^m_, a_,
 (*lori___HighEnergyPhysics`FeynCalc`CoreObjects`Momentum,*)
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ],
              TraditionalForm
  ] := RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
    SubsuperscriptBox[Tbox[a](*[[1]]*), Tbox[lori], Tbox[suni]]
                      }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`CoreObjects`PartialD, a_,
 (*lori___HighEnergyPhysics`FeynCalc`CoreObjects`Momentum,*)
 lori___HighEnergyPhysics`FeynCalc`CoreObjects`LorentzIndex |
 lori___HighEnergyPhysics`Phi`Objects`UIndex,
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`SUNIndex |
 suni___HighEnergyPhysics`FeynCalc`CoreObjects`ExplicitSUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a](*[[1]]*),
                 Tbox[lori], Tbox[suni]]
                        }];

(* ************************************************************** *)

End[]; EndPackage[];

(* ************************************************************** *)

(* Additional definitions from Objects.m *)

QuantumField[ders___PartialD,a__, lors___LorentzIndex,
      iis___SUNIndex|iis___ExplicitSUNIndex][
      isosp_SUNIndex|isosp_ExplicitSUNIndex]:=
  QuantumField[ders,a,lors,isosp,iis];

QuantumField[
ders___PartialD, a__, lors___LorentzIndex,
        iis___SUNIndex|iis___ExplicitSUNIndex][ui_UIndex] :=
    QuantumField[ders, a, lors, iis, ui];

HighEnergyPhysics`Phi`Objects`Private`setLeftRightComponents;

(* ************************************************************** *)
