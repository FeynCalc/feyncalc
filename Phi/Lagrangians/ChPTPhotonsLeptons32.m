(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTPhotonsLeptons32                       *)
(*                                                                 *)
(* *************************************************************** *)

(* 
   Author:              F.Orellana 2001

   Mathematica Version: 4.0 

   Requirements:        FeynCalc > 3, Phi 

   Summary:             Lagrangian for Phi

   Description:         The leading order ChPT lagrangian with 
                        electromagnetic couplings.
    
                        Taken from
                        M. Knecht, H. Neufeld, H. Rupertsberger, P. Talavera
                        (1999), hep-ph/9909284
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTPhotonsLeptons32::"usage"=
   "ChPTPhotonsLeptons32 is the name of the file containing the definitions for \
 Lagrangian[ChPTPhotonsLeptons3[2]], which is the leading order mesonic \
SU(3) ChPT lagrangian with couplings to virtual photons and leptons. \
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
    CouplingConstant[ChPTPhotonsLeptons3[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SuperscriptBox[MakeBoxes[StyleForm["C",FontSlant->"Italic"]][[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[pt[st]]]},{
          MakeBoxes[TraditionalForm[pt[sc]]]},{
          MakeBoxes[TraditionalForm[pt[qs]]]}]]];

(* --------------------------------------------------------------- *)

Lagrangian[
ChPTPhotonsLeptons3[2]]:=

1/4*DecayConstant[PhiMeson]^2*

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

CouplingConstant[ChPTPhotonsLeptons3[2]]*
UTrace[NM[UChiralSpurionRightMatrix,MM,
UChiralSpurionLeftMatrix,Adjoint[MM]]]+

DOT[DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]],
DiracMatrix[LorentzIndex[\[Mu]]],
(I*QuantumField[PartialD[LorentzIndex[\[Mu]]],
Particle[Lepton],SUNIndex[i]]+
CouplingConstant[QED[1]]*
DOT[QuantumField[Particle[Photon],LorentzIndex[\[Mu]]],
QuantumField[Particle[Lepton],SUNIndex[i]]])]-

ParticleMass[Lepton,SUNIndex[i]]*
DOT[DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]],
QuantumField[Particle[Lepton],SUNIndex[i]]]+

DOT[DiracBar[QuantumField[Particle[Neutrino],SUNIndex[i]]],
DiracMatrix[LorentzIndex[\[Mu]]],
DiracMatrix[6],
I*QuantumField[PartialD[LorentzIndex[\[Mu]]],
Particle[Neutrino],SUNIndex[i]]];
    
(* --------------------------------------------------------------- *)

FieldsSet[ChPTPhotonsLeptons3[2]]:=
{IsoVector[QuantumField[Particle[PhiMeson]]],
QuantumField[Particle[Photon]]};

$Lagrangians=Union[$Lagrangians,{ChPTPhotonsLeptons3[2]}];
