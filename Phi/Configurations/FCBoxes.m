(* ************************************************************** *)
(*                                                                *)
(*                      FCBoxes.m                                 *)
(*                                                                *)
(* ************************************************************** *)

(* 
   Author:              F.Orellana 2000

   Mathematica Version: 3.0 

   Requirements:        FeynCalc > 3, Phi 

   Description:         The definitions in this file modifies the
                        way QuantumFields are displayed, so that
                        outputs of calculations with Phi look nicer.  
   
*)

(* ************************************************************** *)

(* Box definitions for FeynCalc *)

ClearAll[HighEnergyPhysics`FeynCalc`QuantumField`QuantumField];

BeginPackage["HighEnergyPhysics`FeynCalc`QuantumField`",
             "HighEnergyPhysics`FeynCalc`"];

Begin["`Private`"];
   

QuantumField /: 
   MakeBoxes[ QuantumField[a_][_], TraditionalForm
            ] := Tbox[a][[1]];

QuantumField /: 
MakeBoxes[ QuantumField[f_, lo_[mu_,___]][_], TraditionalForm
            ] := SubscriptBox[Tbox[f][[1(*,1*)]], Tbox[mu]] /; 
                   lo === LorentzIndex;

QuantumField /: 
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ][_], TraditionalForm
            ] := 
SubsuperscriptBox[Tbox[f][[1]], Tbox[lori], Tbox[suni]]/; 
           MatchQ[lo, LorentzIndex | Momentum] && 
                            sun === SUNIndex;

QuantumField /: 
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_], a_,
 lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ][_],
              TraditionalForm] :=
RowBox[{SubscriptBox["\[PartialD]" , Tbox[pa]],
SubsuperscriptBox[Tbox[a][[1(*,1*)]], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /: 
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_]^m_, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ][_],
              TraditionalForm] :=
              RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
SubsuperscriptBox[Tbox[a][[1(*,1*)]], Tbox[lori], Tbox[suni]]
                      }];

QuantumField /: 
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ][_],
              TraditionalForm
            ] := RowBox[{TBox[pa],
SubsuperscriptBox[Tbox[a][[1(*,1*)]], Tbox[lori], Tbox[suni]]
                        }];

(*----------------------------------------------------------------*)

QuantumField /: 
   MakeBoxes[ QuantumField[f_,suni:sun_[_]..
                          ][_], TraditionalForm] := 
      SuperscriptBox[Tbox[f][[1]],Tbox[suni]] /;sun === SUNIndex;
      
QuantumField /: 
   MakeBoxes[ QuantumField[f_,suni:sun_[_]..
                          ], TraditionalForm
            ] := SuperscriptBox[Tbox[f][[1]],Tbox[suni]
                                  ] /; sun === SUNIndex;

(*----------------------------------------------------------------*)

(* Modified original FeynCalc definitions*)

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo},SUNIndex/@{suli}];

QuantumField[f1_QuantumField] := f1;

  
QuantumField /: 
   MakeBoxes[ QuantumField[a_][_], TraditionalForm
            ] := Tbox[a][[1]];

QuantumField /: 
   MakeBoxes[ QuantumField[a_], TraditionalForm
            ] := Tbox[a][[1]];

QuantumField /: 
   MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
            ] := SubscriptBox[Tbox[f][[1(*,1*)]], Tbox[mu]] /; 
                   lo === LorentzIndex;

QuantumField /:
    MakeBoxes[
      QuantumField[f_,
        lol : ((HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_, ___])..)],
        TraditionalForm] := SubscriptBox[Tbox[f][[1(*, 1*)]], Tbox[lol]];

QuantumField /: 
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
     ], TraditionalForm
 ] := SubsuperscriptBox[Tbox[f][[1]], Tbox[lori], Tbox[suni]
        ] /; MatchQ[lo, LorentzIndex | Momentum
               ] && sun === SUNIndex;

QuantumField /: 
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
    ][_], TraditionalForm
 ] := SubsuperscriptBox[Tbox[f][[1]], Tbox[lori], Tbox[suni]] /;
        MatchQ[lo, LorentzIndex | Momentum] && 
                            sun === SUNIndex;
QuantumField /: 
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_], a_,
 lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
       ] := RowBox[{SubscriptBox["\[PartialD]" , Tbox[pa]],
        SubsuperscriptBox[Tbox[a][[1(*,1*)]], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /: 
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_]^m_, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
  ] := RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
    SubsuperscriptBox[Tbox[a][[1(*,1*)]], Tbox[lori], Tbox[suni]]
                      }];

QuantumField /: 
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a][[1(*,1*)]],
                 Tbox[lori], Tbox[suni]]
                        }];

(* ************************************************************** *)

End[]; EndPackage[];

(* ************************************************************** *)

fcqf[ders___HighEnergyPhysics`FeynCalc`PartialD`PartialD,a__,
      lors___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][
    isosp_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]:=
  fcqf[ders,a,lors,isosp,iis];

fcqf[aa___, Particle[LeftComponent[a_],i___],bb___][x_]:=
  1/2*(fcqf[aa, Particle[Vector[a],i],bb][x]+
        fcqf[aa, Particle[AxialVector[a],i],bb][x]);

fcqf[aa___, Particle[RightComponent[a_],i___],bb___][x_]:=
  1/2*(fcqf[aa, Particle[Vector[a],i],bb][x]-
        fcqf[aa, Particle[AxialVector[a],i],bb][x]);

(* ************************************************************** *)
