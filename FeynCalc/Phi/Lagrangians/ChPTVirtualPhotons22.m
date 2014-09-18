(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTVirtualPhotons22                       *)
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
    
                        Taken from Marc Knecht and Res Urech
                        (1997), hep-ph/9709348
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* --------------------------------------------------------------- *)

ChPTVirtualPhotons22::"usage"=
   "ChPTVirtualPhotons22.m is the name of the file containing the definitions for \
Lagrangian[ChPTVirtualPhotons[2]], which is the leading order pionic \
SU(2) ChPT lagrangian with couplings to virtual photons. \
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
    CouplingConstant[
  ChPTVirtualPhotons2[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SuperscriptBox[MakeBoxes[StyleForm["C",FontSlant->"Italic"]][[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[pt[st]]]},{
          MakeBoxes[TraditionalForm[pt[sc]]]},{
          MakeBoxes[TraditionalForm[pt[qs]]]}]]];

(* --------------------------------------------------------------- *)

Lagrangian[ChPTVirtualPhotons2[2]]:=

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

CouplingConstant[ChPTVirtualPhotons2[2]]*
UTrace[NM[UChiralSpurionRightMatrix, MM,
UChiralSpurionLeftMatrix, Adjoint[MM]]];
    
(* --------------------------------------------------------------- *)

FieldsSet[ChPTVirtualPhotons2[2]]:=
{IsoVector[QuantumField[Particle[Pion]]],
QuantumField[Particle[Photon]]};

$Lagrangians=Union[$Lagrangians,{ChPTVirtualPhotons2[2]}];
