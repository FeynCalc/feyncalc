(* *************************************************************** *)
(*                                                                 *)
(*                      ChPT32                                     *)
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

ChPT32::"usage"=
"ChPT32.m is the name of the file containing the definitions for
ULagrangian[ChPT3[2]], which is the SU(3) lowest order ChPT
lagrangian.  To evaluate use ArgumentsSupply";

Begin["`Private`"];

mu=(Global`\[Mu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

(* --------------------------------------------------------------- *)

ULagrangian[ChPT3[2]]:=

1/4*DecayConstant[PhiMeson,RenormalizationState[0]]^2*

(UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]] ] +

UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]);

(* --------------------------------------------------------------- *)

FieldsSet[ChPT3[2]]:=
{IsoVector[
fcqf[Particle[PhiMeson,RenormalizationState[0]]]
]};

$ULagrangians=Union[$ULagrangians,{ChPT3[2]}];

End[];

End[];
