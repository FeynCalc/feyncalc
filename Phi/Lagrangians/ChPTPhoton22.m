(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTPhoton22                               *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1997

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The simplest ChPT lagrangian.

                        Taken from J.F. Donoghue, E. Golowich
                        and B.R. Holstein (1992), "Dynamics of
                        the Standard Model", Cambridge
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTPhoton22::"usage"=
"ChPTPhoton22.m is the name of the file containing the definitions for
ULagrangian[ChPT2PhotonPhoton[2]], which is the SU(2) lowest order ChPT
lagrangian with coupling to a photon.  To evaluate use ArgumentsSupply";

Begin["`Private`"];

mu=(Global`\[Mu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

(* --------------------------------------------------------------- *)

ULagrangian[ChPTPhoton2[2]]:=

1/4*DecayConstant[Pion,RenormalizationState[0]]^2*

(UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]] ] +

UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]);

(* --------------------------------------------------------------- *)

FieldsSet[ChPTPhoton2[2]]:=
{IsoVector[
fcqf[Particle[Pion,RenormalizationState[0]]],
fcqf[Particle[Photon,RenormalizationState[0]],LorentzIndex[\[Mu]]]
]};

$ULagrangians=Union[$ULagrangians,{ChPTPhoton2[2]}];

End[];

End[];
