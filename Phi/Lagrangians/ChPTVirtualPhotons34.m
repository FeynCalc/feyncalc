(* ************************************************************** *)
(*                                                                *)
(*                       ChPTVirtualPhotons34                     *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana 2001

   Mathematica Version: 4.0

   Requirements:        Feyncalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The next to leading order ChPT lagrangian
                        with electromagnetic couplings.

                        Taken from Res Urech (1994), hep-ph/9405341
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTVirtualPhotons34::"usage"=
"\"ChPTVirtualPhotons34.m\" is the name of the file containing the definitions for
Lagrangian[ChPTVirtualPhotons3[4]], which is the mesonic ChPT lagrangian
including virtual photons to fourth order in the energy.
To evaluate use ArgumentsSupply";

L1::"usage"=
"L1 := UCouplingConstant[ChPTVirtualPhotons3[4],1] is one of the constants of the
fourth order ChPT lagrangian";

L2::"usage"=
"L2 := UCouplingConstant[ChPTVirtualPhotons3[4],2] is one of the constants of the
fourth order ChPT lagrangian";

L3::"usage"=
"L3 := UCouplingConstant[ChPTVirtualPhotons3[4],3] is one of the constants of the
fourth order ChPT lagrangian";

L4::"usage"=
"L4 := UCouplingConstant[ChPTVirtualPhotons3[4],4] is one of the constants of the
fourth order ChPT lagrangian";

L5::"usage"=
"L5 := UCouplingConstant[ChPTVirtualPhotons3[4],5] is one of the constants of the
fourth order ChPT lagrangian";

L6::"usage"=
"L6 := UCouplingConstant[ChPTVirtualPhotons3[4],6] is one of the constants of the
fourth order ChPT lagrangian";

L7::"usage"=
"L7 := UCouplingConstant[ChPTVirtualPhotons3[4],7] is one of the constants of the
fourth order ChPT lagrangian";

L8::"usage"=
"L8 := UCouplingConstant[ChPTVirtualPhotons3[4],8] is one of the constants of the
fourth order ChPT lagrangian";

L9::"usage"=
"L9 := UCouplingConstant[ChPTVirtualPhotons3[4],9] is one of the constants of the
fourth order ChPT lagrangian";

L10::"usage"=
"L10 := UCouplingConstant[ChPTVirtualPhotons3[4],10] is one of the constants of the
fourth order ChPT lagrangian";

H1::"usage"=
"H1 := UCouplingConstant[ChPTVirtualPhotons3[4],11] is one of the constants of the
fourth order ChPT lagrangian";

H2::"usage"=
"H2 := UCouplingConstant[ChPTVirtualPhotons3[4],12] is one of the constants of the
fourth order ChPT lagrangian";

K1::"usage"=
"K1 := UCouplingConstant[ChPTVirtualPhotons3[4],13] is one of the constants of the
fourth order ChPT lagrangian";

K2::"usage"=
"K2 := UCouplingConstant[ChPTVirtualPhotons3[4],14] is one of the constants of the
fourth order ChPT lagrangian";

K3::"usage"=
"K3 := UCouplingConstant[ChPTVirtualPhotons3[4],15] is one of the constants of the
fourth order ChPT lagrangian";

K4::"usage"=
"K4 := UCouplingConstant[ChPTVirtualPhotons3[4],16] is one of the constants of the
fourth order ChPT lagrangian";

K5::"usage"=
"K5 := UCouplingConstant[ChPTVirtualPhotons3[4],17] is one of the constants of the
fourth order ChPT lagrangian";

K6::"usage"=
"K6 := UCouplingConstant[ChPTVirtualPhotons3[4],18] is one of the constants of the
fourth order ChPT lagrangian";

K7::"usage"=
"K7 := UCouplingConstant[ChPTVirtualPhotons3[4],19] is one of the constants of the
fourth order ChPT lagrangian";

K8::"usage"=
"K8 := UCouplingConstant[ChPTVirtualPhotons3[4],20] is one of the constants of the
fourth order ChPT lagrangian";

K9::"usage"=
"K9 := UCouplingConstant[ChPTVirtualPhotons3[4],21] is one of the constants of the
fourth order ChPT lagrangian";

K10::"usage"=
"K10 := UCouplingConstant[ChPTVirtualPhotons3[4],22] is one of the constants of the
fourth order ChPT lagrangian";

K11::"usage"=
"K11 := UCouplingConstant[ChPTVirtualPhotons3[4],23] is one of the constants of the
fourth order ChPT lagrangian";

K12::"usage"=
"K12 := UCouplingConstant[ChPTVirtualPhotons3[4],24] is one of the constants of the
fourth order ChPT lagrangian";

K13::"usage"=
"K13 := UCouplingConstant[ChPTVirtualPhotons3[4],25] is one of the constants of the
fourth order ChPT lagrangian";

K14::"usage"=
"K14 := UCouplingConstant[ChPTVirtualPhotons3[4],26] is one of the constants of the
fourth order ChPT lagrangian";

K15::"usage"=
"K15 := UCouplingConstant[ChPTVirtualPhotons3[4],27] is one of the constants of the
fourth order ChPT lagrangian";

K16::"usage"=
"K16 := UCouplingConstant[ChPTVirtualPhotons3[4],28] is one of the constants of the
fourth order ChPT lagrangian";

K17::"usage"=
"K17 := UCouplingConstant[ChPTVirtualPhotons3[4],29] is one of the constants of the
fourth order ChPT lagrangian";

K18::"usage"=
"K18 := UCouplingConstant[ChPTVirtualPhotons3[4],30] is one of the constants of the
fourth order ChPT lagrangian";

K19::"usage"=
"K19 := UCouplingConstant[ChPTVirtualPhotons3[4],31] is one of the constants of the
fourth order ChPT lagrangian";

(* ---------------------------------------------------------------- *)

Begin["`Private`"];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

QQ := UQuarkChargeMatrix;

fcqf := HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

L1 := UCouplingConstant[ChPTVirtualPhotons3[4],1];
L2 := UCouplingConstant[ChPTVirtualPhotons3[4],2];
L3 := UCouplingConstant[ChPTVirtualPhotons3[4],3];
L4 := UCouplingConstant[ChPTVirtualPhotons3[4],4];
L5 := UCouplingConstant[ChPTVirtualPhotons3[4],5];
L6 := UCouplingConstant[ChPTVirtualPhotons3[4],6];
L7 := UCouplingConstant[ChPTVirtualPhotons3[4],7];
L8 := UCouplingConstant[ChPTVirtualPhotons3[4],8];
L9 := UCouplingConstant[ChPTVirtualPhotons3[4],9];
L10 := UCouplingConstant[ChPTVirtualPhotons3[4],10];

H1 := UCouplingConstant[ChPTVirtualPhotons3[4],11];
H2 := UCouplingConstant[ChPTVirtualPhotons3[4],12];

K1 := UCouplingConstant[ChPTVirtualPhotons3[4],13];
K2 := UCouplingConstant[ChPTVirtualPhotons3[4],14];
K3 := UCouplingConstant[ChPTVirtualPhotons3[4],15];
K4 := UCouplingConstant[ChPTVirtualPhotons3[4],16];
K5 := UCouplingConstant[ChPTVirtualPhotons3[4],17];
K6 := UCouplingConstant[ChPTVirtualPhotons3[4],18];
K7 := UCouplingConstant[ChPTVirtualPhotons3[4],19];
K8 := UCouplingConstant[ChPTVirtualPhotons3[4],20];
K9 := UCouplingConstant[ChPTVirtualPhotons3[4],21];
K10:= UCouplingConstant[ChPTVirtualPhotons3[4],22];
K11 := UCouplingConstant[ChPTVirtualPhotons3[4],23];
K12 := UCouplingConstant[ChPTVirtualPhotons3[4],24];
K13 := UCouplingConstant[ChPTVirtualPhotons3[4],25];
K14 := UCouplingConstant[ChPTVirtualPhotons3[4],26];
K15 := UCouplingConstant[ChPTVirtualPhotons3[4],27];
K16 := UCouplingConstant[ChPTVirtualPhotons3[4],28];
K17 := UCouplingConstant[ChPTVirtualPhotons3[4],29];
K18 := UCouplingConstant[ChPTVirtualPhotons3[4],30];
K19 := UCouplingConstant[ChPTVirtualPhotons3[4],31];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTVirtualPhotons3[4],i_?((#<11)&),st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["L",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant /: MakeBoxes[
      UCouplingConstant[ChPTVirtualPhotons3[4], i_?((10<#<13) &), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 10, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["H", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /.
                  "HighEnergyPhysics`Phi`Objects`Private`jj" -> ToString[ii]];

UCouplingConstant /: MakeBoxes[
      UCouplingConstant[ChPTVirtualPhotons3[4], i_?((#>12)&), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 12, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["K", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /.
                  "HighEnergyPhysics`Phi`Objects`Private`jj" -> ToString[ii]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTVirtualPhotons3[4]] =
{3/4, z, -3/4, 2 z, -9/4, 3/2 z, 0, z, -1/4, 1/4+3/2 z, 1/8, 1/4, 0, 0, 3/2 + 3 z + 20 z^2} /.
z -> UCouplingConstant[ChPTVirtualPhotons3[2],RenormalizationState[0]]/
DecayConstant[PhiMeson,RenormalizationState[0]]^4;

(* ---------------------------------------------------------------- *)

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);

(* ---------------------------------------------------------------- *)


HighEnergyPhysics`fctables`Lagrangian`Lagrangian[
ChPTVirtualPhotons3[4]]:=

(* p^4 *)

L1[0]/4*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ] ] +

L2[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}]] ] ] +

L3[0]*
UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}],
                            Adjoint[CDr[MM,{nu}]], CDr[MM,{nu}]] ] +

L4[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}]] ],
    UTrace[ NM[Adjoint[UChiMatrix],MM] + NM[UChiMatrix, Adjoint[MM]] ] ] +

L5[0]*
UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}],
                           NM[ Adjoint[UChiMatrix],MM] + NM[UChiMatrix, Adjoint[MM]] ] ] +

L6[0]*
NM[UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[UChiMatrix, Adjoint[MM]] ],
   UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[UChiMatrix, Adjoint[MM]] ] ] +

L7[0]*
NM[UTrace[ NM[Adjoint[UChiMatrix], MM] - NM[UChiMatrix, Adjoint[MM]] ] ,
   UTrace[ NM[Adjoint[UChiMatrix], MM] - NM[UChiMatrix, Adjoint[MM]] ] ] +

L8[0]*
UTrace[ NM[Adjoint[UChiMatrix], MM, Adjoint[UChiMatrix], MM] +
              NM[UChiMatrix, Adjoint[MM], UChiMatrix, Adjoint[MM]]] -

L9[0]*I*
UTrace[ NM[CDr[MM,{mu}], Adjoint[CDr[MM,{nu}]], GRight[mu,nu]] +
        NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}], GLeft[mu,nu]] ] +

L10[0]*
UTrace[ NM[GRight[mu,nu], Adjoint[GLeft[mu,nu]]] ] +

H1[0]*
UTrace[ NM[GRight[mu,nu], GRight[mu,nu]] +
              NM[GLeft[mu,nu], GLeft[mu,nu]] ] +

H2[0]*
UTrace[ NM[Adjoint[UChiMatrix], UChiMatrix] ] +

(H1[0]-H3[0])/2*
Re[Det[UChiMatrix]] +

(* e^2 p^2 *)

DecayConstant[PhiMeson]^2*(

K1[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ],
    UTrace[ NMPower[QQ, 2] ] ] +

K2[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ],
    UTrace[ NM[QQ, MM, QQ, Adjoint[MM]] ] ] +

K3[0]*
(NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], QQ, MM] ],
     UTrace[ NM[Adjoint[CDr[MM,{mu}]], QQ, MM] ] ] +
 NM[ UTrace[ NM[CDr[MM,{mu}], QQ, Adjoint[MM]] ],
     UTrace[ NM[CDr[MM,{mu}], QQ, Adjoint[MM]] ] ])+

K4[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], QQ, MM] ],
    UTrace[ NM[CDr[MM,{mu}], QQ, Adjoint[MM]] ] ] +

K5[0]*
UTrace[ NM[NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]]+
             NM[ CDr[MM,{mu}], Adjoint[CDr[MM,{mu}]]],
        NMPower[QQ, 2]]  ] +

K6[0]*
UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}], QQ, Adjoint[MM], QQ, MM] +
              NM[CDr[MM,{mu}], Adjoint[CDr[MM,{mu}]], QQ, MM, QQ, Adjoint[MM]]] +

K7[0]*
NM[ UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix] ],
    UTrace[ NMPower[QQ, 2] ] ] +

K8[0]*
NM[ UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix] ],
    UTrace[ NM[QQ, MM, QQ, Adjoint[MM]] ] ] +

K9[0]*
UTrace[ NM[NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix] +
                            NM[UChiMatrix, Adjoint[MM]] + NM[MM, Adjoint[UChiMatrix]],
  NMPower[QQ, 2]] ] +

K10[0]*
UTrace[  NM[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix],
               QQ, Adjoint[MM], QQ, MM] +
               NM[ NM[UChiMatrix, Adjoint[MM]] + NM[MM, Adjoint[UChiMatrix]],
               QQ, MM, QQ, Adjoint[MM]]  ]+

K11[0]*
UTrace[ NM[ NM[Adjoint[UChiMatrix], MM] - NM[Adjoint[MM], UChiMatrix],
        QQ, Adjoint[MM], QQ, MM ] +
        NM[ NM[UChiMatrix, Adjoint[MM]] - NM[MM, Adjoint[UChiMatrix]],
        QQ, MM, QQ,Adjoint[MM] ] ] +

K12[0]*
UTrace[ NM[ Adjoint[CDr[MM,{mu}]],
        NM[CQRight[mu], QQ] -
        NM[QQ, CQRight[mu]], MM ] +
        NM[ CDr[MM,{mu}],
        NM[CQLeft[mu], QQ] -
        NM[QQ, CQLeft[mu]], Adjoint[MM] ] ] +

K13[0]*
UTrace[ NM[ CQRight[mu], MM, CQLeft[mu], Adjoint[MM] ] ] +

K14[0]*
UTrace[ NM[ CQRight[mu], CQRight[mu] ] +
        NM[ CQLeft[mu], CQLeft[mu] ] ]

(*Tentative correction of Urech*)
K19[0]*
UTrace[ NM[ Adjoint[CDr[MM,{mu}]],
        QQ, MM, CQLeft[mu] ] +
        NM[CDr[MM,{mu}],
        QQ, Adjoint[MM], CQRight[mu] ] ] 

) +

(* e^4 *)

DecayConstant[PhiMeson]^4*(

K15[0]*
NMPower[UTrace[ NM[QQ, MM, QQ, Adjoint[MM]]], 2]+

K16[0]*
NM[ UTrace[ NM[QQ, MM, QQ,Adjoint[MM]] ],
    UTrace[ NM[QQ, QQ] ] ] +

K17[0]*
NMPower[UTrace[ NM[QQ, QQ] ], 2] 

) +

(* p^2 e^2 photon propagator counterterm *)

K18[0]*
UTrace[NMPower[QQ,2]]*
NM[FieldStrengthTensor[{mu}, fcqf[Particle[Photon],{nu}]],
FieldStrengthTensor[{mu}, fcqf[Particle[Photon],{nu}]]];

(* ---------------------------------------------------------------- *)

Global`$Lagrangians=Union[Global`$Lagrangians,{ChPTVirtualPhotons3[4]}];

FieldsSet[ChPTVirtualPhotons3[4]]:=
{IsoVector[fcqf[Particle[PhiMeson,RenormalizationState[0]]]],
fcqf[Particle[Photon,RenormalizationState[0]]]};

End[];

End[];
