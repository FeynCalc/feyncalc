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
To evaluate use ArgumentsSupply";

GaugeFixingParameter::"usage"=
   "GaugeFixingParameter(=1/GaugeXi) is the gauge fixing parameter of QED in Lorentz gauge.  \
the usual choice is Feynman gauge, GaugeFixingParameter=1";

(* --------------------------------------------------------------- *)

Begin["`Private`"];

(* --------------------------------------------------------------- *)

(* Abbreviations *)

fcpd:=HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);
ii=(Global`i);
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;

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

GaugeFixingParameter/:
MakeBoxes[GaugeFixingParameter,TraditionalForm]:=
MakeBoxes[StyleForm["\[Lambda]",FontSlant->"Italic"]][[1]];


(* --------------------------------------------------------------- *)

SetAttributes[ChPTPhotonsLeptons3,NumericFunction];

(* --------------------------------------------------------------- *)


HighEnergyPhysics`fctables`Lagrangian`Lagrangian[
ChPTPhotonsLeptons3[2]]:=

1/4*DecayConstant[PhiMeson]^2*

(UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]] ] +

UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[Adjoint[UChiMatrix],MM] ]) -

1/4*
NM[FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]],
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]]]-

GaugeFixingParameter/2*
FDr[fcqf[Particle[Photon],fcli[mu]],{mu}]*
FDr[fcqf[Particle[Photon],fcli[nu]],{nu}]+

CouplingConstant[ChPTPhotonsLeptons3[2]]*
UTrace[NM[UChiralSpurionRightMatrix,MM,
UChiralSpurionLeftMatrix,Adjoint[MM]]]+

DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcdm[fcli[mu]].
(I*fcqf[fcpd[fcli[mu]],
Particle[Lepton],fcsuni[ii]]+
CouplingConstant[QED[1]]*
fcqf[Particle[Photon],fcli[mu]].
fcqf[Particle[Lepton],fcsuni[ii]])-

ParticleMass[Lepton,fcsuni[ii]]*
DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcqf[Particle[Lepton],fcsuni[ii]]+

DiracBar[fcqf[Particle[Neutrino],fcsuni[ii]]].
fcdm[fcli[mu]].
fcdm[6].
I*fcqf[fcpd[fcli[mu]],
Particle[Neutrino],fcsuni[ii]];
    
(* --------------------------------------------------------------- *)

FieldsSet[ChPTPhotonsLeptons3[2]]:=
{IsoVector[fcqf[Particle[PhiMeson]]],
fcqf[Particle[Photon]]};

$Global`Lagrangians=Union[Global`$Lagrangians,{ChPTPhotonsLeptons3[2]}];

End[];

End[];
