(* *************************************************************** *)
(*                                                                 *)
(*                      QED1                                       *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The standard QED lagrangian
                        for the electron.

                        Taken from Bjoerken and Drell,
                        "Relativistic Quantum Fields",
                        McGraw-Hill 1965
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

QED1::"usage"=
"QED1.m is the name of the file containing the definitions for
ULagrangian[QED[1]], which is the standard QED lagrangian,
UCouplingConstant[QED[1]] is the bare
unit charge (the charge of the positron)";

Begin["`Private`"];

mu=(Global`\[Mu]);nu=(Global`\[Nu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcpd:=HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;

(* --------------------------------------------------------------- *)

HighEnergyPhysics`FeynCalc`Lagrangian`Lagrangian["QED"]:=
ULagrangian[QED[1]];

(* --------------------------------------------------------------- *)

ULagrangian[QED[1]]:=


-1/4*
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]].
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]]+

DiracBar[fcqf[Particle[Electron]]].
fcdm[fcli[mu]].
(I*fcqf[fcpd[fcli[mu]],Particle[Electron]]+
UCouplingConstant[QED[1]]*
fcqf[Particle[Photon],fcli[mu]].
fcqf[Particle[Electron]])-

ParticleMass[Electron]*
DiracBar[fcqf[Particle[Electron]]].
fcqf[Particle[Electron]];

(* --------------------------------------------------------------- *)

FieldsSet[QED[1]]:=
{fcqf[Particle[Electron]],
fcqf[Particle[Photon],fcli[mu]]};

$ULagrangians=Union[$ULagrangians,{QED[1]}];

End[];

End[];
