(* *************************************************************** *)
(*                                                                 *)
(*                      QED1                                       *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                1998

Mathematica Version: 3.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         The standard QED lagrangian
						for the electron.

						Taken from Bjoerken and Drell,
						"Relativistic Quantum Fields",
						McGraw-Hill 1965
*)


Begin["Phi`Objects`"];

(* --------------------------------------------------------------- *)

QED1::usage =
"QED1.m is the name of the file containing the definitions for
Lagrangian[QED[1]], which is the standard QED lagrangian,
CouplingConstant[QED[1]] is the bare
unit charge (the charge of the positron).";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

Lagrangian["QED"] :=
	Lagrangian[QED[1]];

(* --------------------------------------------------------------- *)

Lagrangian[QED[1]] :=
	-1/4*
	DOT[
	FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]],
	FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]
	]+

	DOT[DiracBar[QuantumField[Particle[Electron]]],
	DiracMatrix[LorentzIndex[\[Mu]]],
	(I*QuantumField[FCPartialD[LorentzIndex[\[Mu]]],Particle[Electron]]+
	CouplingConstant[QED[1]]*
	DOT[QuantumField[Particle[Photon],LorentzIndex[\[Mu]]],
	QuantumField[Particle[Electron]]])
	]-

	ParticleMass[Electron]*
	DOT[DiracBar[QuantumField[Particle[Electron]]],
	QuantumField[Particle[Electron]]];

(* --------------------------------------------------------------- *)

FieldsSet[QED[1]] :=
	{QuantumField[Particle[Electron]],
	QuantumField[Particle[Photon],LorentzIndex[\[Mu]]]};

$Lagrangians = Union[$Lagrangians,{QED[1]}];

