(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTPhoton22                               *)
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


Begin["HighEnergyPhysics`Phi`Objects`"];

(* --------------------------------------------------------------- *)

ChPTPhoton22::"usage"=
"ChPTPhoton22.m is the name of the file containing the definitions for
Lagrangian[ChPT2PhotonPhoton[2]], which is the SU(2) lowest order ChPT
lagrangian with coupling to a photon.  To evaluate use ArgumentsSupply.";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

Lagrangian[ChPTPhoton2[2]]:=

1/4*DecayConstant[Pion,RenormalizationState[0]]^2*

(UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ] +

UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]);

(* --------------------------------------------------------------- *)

FieldsSet[ChPTPhoton2[2]]:=
{IsoVector[
QuantumField[Particle[Pion,RenormalizationState[0]]],
QuantumField[Particle[Photon,RenormalizationState[0]],LorentzIndex[\[Mu]]]
]};

$Lagrangians=Union[$Lagrangians,{ChPTPhoton2[2]}];
