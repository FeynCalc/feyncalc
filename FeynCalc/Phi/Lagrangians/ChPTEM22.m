(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTEM22                                   *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana

   Year:                2001

   Mathematica Version: 4.0

   Requirements:        FeynCalc > 3, PHI

   Summary:             Lagrangian for PHI

   Description:         The leading order ChPT lagrangian with
                        electromagnetic couplings.

                        Taken from U. Meissner, G. Mueller,
                        S. Steininger, hep-ph/9704377
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTEM22::"usage"=
"ChPT22 is the name of the file containing the definitions for
Lagrangian[ChPT2EM[2]], which is the leading order pionic
SU(2) ChPT lagrangian with couplings to virtual photons.
To evaluate use ArgumentsSupply.";

(* --------------------------------------------------------------- *)

End[];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

pt/:MakeBoxes[pt[a_],TraditionalForm]:=MakeBoxes[TraditionalForm[a]];
pt/:MakeBoxes[pt[],TraditionalForm]:="";
pt/:MakeBoxes[pt[RenormalizationState[1]],TraditionalForm]:="r";
pt/:MakeBoxes[pt[RenormalizationState[0]],TraditionalForm]:="";

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPTEM2[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SuperscriptBox[MakeBoxes[StyleForm["C",FontSlant->"Italic"]][[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[pt[st]]]},{
          MakeBoxes[TraditionalForm[pt[sc]]]},{
          MakeBoxes[TraditionalForm[pt[qs]]]}]]];

(* --------------------------------------------------------------- *)

Lagrangian[ChPTEM2[2]]:=

1/4*DecayConstant[Pion]^2*

(UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ] +

UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[Adjoint[UChiMatrix],MM] ]) -

1/4*
NM[FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]],
FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]]-

$Gauge/2*
FDr[QuantumField[Particle[Photon],LorentzIndex[\[Mu]]],{\[Mu]}]*
FDr[QuantumField[Particle[Photon],LorentzIndex[\[Nu]]],{\[Nu]}]+

CouplingConstant[ChPTEM2[2]]*
UTrace[NM[UMatrix[UChiralSpurionRight],MM,
UMatrix[UChiralSpurionLeft],Adjoint[MM]]];

(* --------------------------------------------------------------- *)

FieldsSet[ChPTEM2[2]]:=
{IsoVector[QuantumField[Particle[Pion]]], QuantumField[Particle[Photon]]};

$Lagrangians=Union[$Lagrangians,{ChPTEM2[2]}];
