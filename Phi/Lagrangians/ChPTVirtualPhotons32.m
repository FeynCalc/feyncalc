(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTVirtualPhotons32                       *)
(*                                                                 *)
(* *************************************************************** *)

(* 
   Author:              F.Orellana 2001

   Mathematica Version: 4.0 

   Requirements:        FeynCalc > 3, Phi 

   Summary:             Lagrangian for Phi

   Description:         The leading order ChPT lagrangian with 
                        electromagnetic couplings.
    
                        Taken from Res Urech (1994), hep-ph/9405341
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTVirtualPhotons32::"usage"=
   "ChPTVirtualPhotons32 is the name of the file containing the definitions for \
 Lagrangian[ChPTVirtualPhotons3[2]], which is the leading order mesonic \
SU(3) ChPT lagrangian with couplings to virtual photons. \
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

(* ---------------------------------------------------------------- *)

(* Box definitions *)

pt/:MakeBoxes[pt[a_],TraditionalForm]:=MakeBoxes[TraditionalForm[a]];
pt/:MakeBoxes[pt[],TraditionalForm]:="";
pt/:MakeBoxes[pt[RenormalizationState[1]],TraditionalForm]:="r";
pt/:MakeBoxes[pt[RenormalizationState[0]],TraditionalForm]:="";
    
UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTVirtualPhotons3[2],st___RenormalizationState,
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

SetAttributes[ChPTVirtualPhotons3,NumericFunction];

(* --------------------------------------------------------------- *)


HighEnergyPhysics`fctables`Lagrangian`Lagrangian[
ChPTVirtualPhotons3[2]]:=

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

UCouplingConstant[ChPTVirtualPhotons3[2]]*
UTrace[NM[UMatrix[UChiralSpurionRight],MM,
UMatrix[UChiralSpurionLeft],Adjoint[MM]]];
    
(* --------------------------------------------------------------- *)

FieldsSet[ChPTVirtualPhotons3[2]]:=
{IsoVector[fcqf[Particle[PhiMeson]]],
fcqf[Particle[Photon]]};

$Global`Lagrangians=Union[Global`$Lagrangians,{ChPTVirtualPhotons3[2]}];

End[];

End[];
