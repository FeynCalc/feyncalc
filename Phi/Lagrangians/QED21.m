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
Lagrangian[QED2[1]], which is the standard QED lagrangian,
CouplingConstant[QED2[1]] is the bare
unit charge (the charge of the positron)";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

Lagrangian["QED2"]:=Lagrangian[QED2[1]];

(* --------------------------------------------------------------- *)

Lagrangian[QED2[1]]:=


-1/4*
FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]].
FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]+

DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]].
DiracMatrix[LorentzIndex[\[Mu]]].
(I*QuantumField[PartialD[LorentzIndex[\[Mu]]],
Particle[Lepton],SUNIndex[i]]+
CouplingConstant[QED[1]]*
QuantumField[Particle[Photon],LorentzIndex[\[Mu]]].
QuantumField[Particle[Lepton],SUNIndex[i]])-

ParticleMass[Lepton,SUNIndex[i]]*
DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]].
QuantumField[Particle[Lepton],SUNIndex[i]];

(* --------------------------------------------------------------- *)

FieldsSet[QED2[1]]:=
{QuantumField[Particle[Lepton]],
QuantumField[Particle[Photon],LorentzIndex[\[Mu]]]};

$Lagrangians=Union[$Lagrangians,{QED2[1]}];
