(* ****************************************************************** *)
(*                                                                    *)
(*                      HBChPT22                                      *)
(*                                                                    *)
(* ****************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The simplest ChPT lagrangian.

                        Taken from G.Ecker and M. Mojzis (1995),
                        hep-ph/9508204
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* ------------------------------------------------------------------ *)

HBChPT22::"usage"=
"HBChPT22.m is the name of the file containing the definitions for
Lagrangian[HBChPT2[2]], which is the SU(2)  ChPT pion-nucleon
lagrangian.  To evaluate use ArgumentsSupply";

GAV::"usage"=
"GAV := CouplingConstant[HBChPT2[2]] is axial vector
coupling constant";

(* ------------------------------------------------------------------ *)

Begin["`Private`"];

(* ------------------------------------------------------------------ *)

mu=(Global`\[Mu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcli:=fcli=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcsuni:=fcsuni=HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;

(* ------------------------------------------------------------------ *)

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant/:
  MakeBoxes[
    HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[
   HBChPT2[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["g",FontSlant->"Italic"]][[1]],
    MakeBoxes["A"],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ------------------------------------------------------------------ *)

GAV :=
   HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[
   HBChPT2[2]];

(* ------------------------------------------------------------------ *)

HighEnergyPhysics`fctables`Lagrangian`Lagrangian[HBChPT2[2]]:=

I*(UVector[
DiracBar[fcqf[Particle[Nucleon]]]].
fcdm[fcli[mu]].
CDr[UVector[fcqf[Particle[Nucleon]]],{mu}])-


ParticleMass[Nucleon]*
UVector[DiracBar[fcqf[Particle[Nucleon]]]].
UVector[fcqf[Particle[Nucleon]]]+


HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[
HBChPT2[2],RenormalizationState[0]]/2*
(UVector[DiracBar[fcqf[Particle[Nucleon]]]].
fcdm[fcli[mu]].USmall[mu].
UVector[fcqf[Particle[Nucleon]]]);

(* ------------------------------------------------------------------ *)

FieldsSet[HBChPT2[2]]:=
{IsoVector[
fcqf[Particle[Pion,RenormalizationState[0]]]],
UVector[fcqf[Particle[Nucleon,RenormalizationState[0]]]]};

Global`$Lagrangians=Union[Global`$Lagrangians,{HBChPT2[2]}];

End[];

End[];
