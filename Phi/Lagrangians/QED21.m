(* *************************************************************** *)
(*                                                                 *)
(*                      QED21                                      *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The standard QED lagrangian
                        for three leptons.

                        Taken from Bjoerken and Drell,
                        "Relativistic Quantum Fields",
                        McGraw-Hill 1965
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

QED21::"usage"=
"QED21.m is the name of the file containing the definitions for
ULagrangian[QED2[1]], which is the standard QED lagrangian,
UCouplingConstant[QED2[1]] is the bare
unit charge (the charge of the positron)";

Begin["`Private`"];

mu=(Global`\[Mu]);nu=(Global`\[Nu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcpd:=HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;
fcsuni:=fcsuni=HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
ii=Global`i;

(* --------------------------------------------------------------- *)

HighEnergyPhysics`FeynCalc`Lagrangian`Lagrangian["QED2"]:=
ULagrangian[QED2[1]];

(* --------------------------------------------------------------- *)

ULagrangian[QED2[1]]:=


-1/4*
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]].
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]]+

DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcdm[fcli[mu]].
(I*fcqf[fcpd[fcli[mu]],
Particle[Lepton],fcsuni[ii]]+
UCouplingConstant[QED[1]]*
fcqf[Particle[Photon],fcli[mu]].
fcqf[Particle[Lepton],fcsuni[ii]])-

ParticleMass[Lepton,fcsuni[ii]]*
DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcqf[Particle[Lepton],fcsuni[ii]];

(* --------------------------------------------------------------- *)

FieldsSet[QED2[1]]:=
{fcqf[Particle[Lepton]],
fcqf[Particle[Photon],fcli[mu]]};

$ULagrangians=Union[$ULagrangians,{QED2[1]}];

End[];

End[];
