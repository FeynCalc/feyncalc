(* *************************************************************** *)
(*                                                                 *)
(*                      ChPT22                                     *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                1997

Mathematica Version: 3.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         The simplest ChPT lagrangian.

						Taken from J.F. Donoghue, E. Golowich
						and B.R. Holstein (1992), "Dynamics of
						the Standard Model", Cambridge
*)


Begin["Phi`Objects`"];

(* --------------------------------------------------------------- *)

ChPT22::usage =
"ChPT22.m is the name of the file containing the definitions for
Lagrangian[ChPT2[2]], which is the SU(2) lowest order ChPT
lagrangian.  To evaluate use ArgumentsSupply.";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

Lagrangian[ChPT2[2]] :=
	1/4*DecayConstant[Pion]^2*

	(UTrace[ NM[CDr[MM, {\[Mu]}],Adjoint[CDr[MM, {\[Mu]}]]] ] +

	UTrace[ NM[UChiMatrix, Adjoint[MM]]+NM[MM, Adjoint[UChiMatrix]] ]);

(* --------------------------------------------------------------- *)

FieldsSet[ChPT2[2]] :=
	{IsoVector[
	QuantumField[Particle[Pion, RenormalizationState[0]]]
	]};

$Lagrangians = Union[$Lagrangians,{ChPT2[2]}];
