(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTW32                                    *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1999

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The lowest order CP conserving weak ChPT
                        lagrangian.

                        Taken from Ecker, Kambor and Wyler
                        Resonances in the weak chiral Lagrangian,
                        CERN-TH.6610/92
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTW32::"usage"=
"ChPTW32.m is the name of the file containing the definitions for
ULagrangian[ChPTW3[2]] the simplest weak ChPT lagrangian.
To evaluate use ArgumentsSupply";

C2::"usage"=
"C2 := UCouplingConstant[ChPTW3[2],1] is one of the constants of the
lowest order weak ChPT lagrangian";

C5::"usage"=
"C5 := UCouplingConstant[ChPTW3[2],2] is one of the constants of the
lowest order weak ChPT lagrangian";

Begin["`Private`"];

(* -------------------------------------------------------------- *)

(* Abbreviations *)

C2 := UCouplingConstant[ChPTW3[2],1];
C5 := UCouplingConstant[ChPTW3[2],2];

mu=(Global`\[Mu]);

fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

(* ---------------------------------------------------------------- *)

(* Box definitions *)

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTW3[2],1,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["c",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTW3[2],2,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["c",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[5]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

ULagrangian[ChPTW3[2]]:=

C2*UTrace[ NM[UGeneratorMatrix[6],
           Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ] +

C5*UTrace[ NM[UGeneratorMatrix[6], Adjoint[MM], UChiMatrix]+
           NM[UGeneratorMatrix[6], Adjoint[UChiMatrix], MM] ];

(* --------------------------------------------------------------- *)

FieldsSet[ChPTW3[2]]:=
{IsoVector[
fcqf[Particle[PhiMeson,RenormalizationState[0]]]
]};

$ULagrangians=Union[$ULagrangians,{ChPTW3[2]}];

End[];

End[];
