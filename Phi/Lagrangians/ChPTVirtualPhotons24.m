(* ************************************************************** *)
(*                                                                *)
(*                       ChPTVirtualPhotons24                     *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana

   Year:                2001

   Mathematica Version: 4.0

   Requirements:        Feyncalc > 3, PHI

   Summary:             Lagrangian for PHI

   Description:         The next to leading order ChPT lagrangian
                        with electromagnetic couplings.
    
                        Taken from Marc Knecht and Res Urech
                        (1997), hep-ph/9709348
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTVirtualPhotons24::"usage"=
"\"ChPTVirtualPhotons24.m\" is the name of the file containing the definitions for
Lagrangian[ChPTVirtualPhotons2[4]], which is the pionic ChPT lagrangian
including virtual photons to fourth order in the energy.
To evaluate use ArgumentsSupply";

L1::"usage"=
"L1 := CouplingConstant[ChPTVirtualPhotons2[4],1] is one of the constants of the
fourth order ChPT lagrangian";

L2::"usage"=
"L2 := CouplingConstant[ChPTVirtualPhotons2[4],2] is one of the constants of the
fourth order ChPT lagrangian";

L3::"usage"=
"L3 := CouplingConstant[ChPTVirtualPhotons2[4],3] is one of the constants of the
fourth order ChPT lagrangian";

L4::"usage"=
"L4 := CouplingConstant[ChPTVirtualPhotons2[4],4] is one of the constants of the
fourth order ChPT lagrangian";

L5::"usage"=
"L5 := CouplingConstant[ChPTVirtualPhotons2[4],5] is one of the constants of the
fourth order ChPT lagrangian";

L6::"usage"=
"L6 := CouplingConstant[ChPTVirtualPhotons2[4],6] is one of the constants of the
fourth order ChPT lagrangian";

L7::"usage"=
"L7 := CouplingConstant[ChPTVirtualPhotons2[4],7] is one of the constants of the
fourth order ChPT lagrangian";

H1::"usage"=
"H1 := CouplingConstant[ChPTVirtualPhotons2[4],8] is one of the constants of the
fourth order ChPT lagrangian";

H2::"usage"=
"H2 := CouplingConstant[ChPTVirtualPhotons2[4],9] is one of the constants of the
fourth order ChPT lagrangian";

H3::"usage"=
"H3 := CouplingConstant[ChPTVirtualPhotons2[4],10] is one of the constants of the
fourth order ChPT lagrangian";

K1::"usage"=
"K1 := CouplingConstant[ChPTVirtualPhotons2[4],11] is one of the constants of the
fourth order ChPT lagrangian";

K2::"usage"=
"K2 := CouplingConstant[ChPTVirtualPhotons2[4],12] is one of the constants of the
fourth order ChPT lagrangian";

K3::"usage"=
"K3 := CouplingConstant[ChPTVirtualPhotons2[4],13] is one of the constants of the
fourth order ChPT lagrangian";

K4::"usage"=
"K4 := CouplingConstant[ChPTVirtualPhotons2[4],14] is one of the constants of the
fourth order ChPT lagrangian";

K5::"usage"=
"K5 := CouplingConstant[ChPTVirtualPhotons2[4],15] is one of the constants of the
fourth order ChPT lagrangian";

K6::"usage"=
"K6 := CouplingConstant[ChPTVirtualPhotons2[4],16] is one of the constants of the
fourth order ChPT lagrangian";

K7::"usage"=
"K7 := CouplingConstant[ChPTVirtualPhotons2[4],17] is one of the constants of the
fourth order ChPT lagrangian";

K8::"usage"=
"K8 := CouplingConstant[ChPTVirtualPhotons2[4],18] is one of the constants of the
fourth order ChPT lagrangian";

K9::"usage"=
"K9 := CouplingConstant[ChPTVirtualPhotons2[4],19] is one of the constants of the
fourth order ChPT lagrangian";

K10::"usage"=
"K10 := CouplingConstant[ChPTVirtualPhotons2[4],20] is one of the constants of the
fourth order ChPT lagrangian";

K11::"usage"=
"K11 := CouplingConstant[ChPTVirtualPhotons2[4],21] is one of the constants of the
fourth order ChPT lagrangian";

K12::"usage"=
"K12 := CouplingConstant[ChPTVirtualPhotons2[4],22] is one of the constants of the
fourth order ChPT lagrangian";

K13::"usage"=
"K13 := CouplingConstant[ChPTVirtualPhotons2[4],23] is one of the constants of the
fourth order ChPT lagrangian";

K14::"usage"=
"K14 := CouplingConstant[ChPTVirtualPhotons2[4],24] is one of the constants of the
fourth order ChPT lagrangian";

K15::"usage"=
"K15 := CouplingConstant[ChPTVirtualPhotons2[4],25] is one of the constants of the
fourth order ChPT lagrangian";

(* ---------------------------------------------------------------- *)

End[];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

QQ = UQuarkChargeMatrix;

L1 = CouplingConstant[ChPTVirtualPhotons2[4],1];
L2 = CouplingConstant[ChPTVirtualPhotons2[4],2];
L3 = CouplingConstant[ChPTVirtualPhotons2[4],3];
L4 = CouplingConstant[ChPTVirtualPhotons2[4],4];
L5 = CouplingConstant[ChPTVirtualPhotons2[4],5];
L6 = CouplingConstant[ChPTVirtualPhotons2[4],6];
L7 = CouplingConstant[ChPTVirtualPhotons2[4],7];

H1 = CouplingConstant[ChPTVirtualPhotons2[4],8];
H2 = CouplingConstant[ChPTVirtualPhotons2[4],9];
H3 = CouplingConstant[ChPTVirtualPhotons2[4],10];

K1 = CouplingConstant[ChPTVirtualPhotons2[4],11];
K2 = CouplingConstant[ChPTVirtualPhotons2[4],12];
K3 = CouplingConstant[ChPTVirtualPhotons2[4],13];
K4 = CouplingConstant[ChPTVirtualPhotons2[4],14];
K5 = CouplingConstant[ChPTVirtualPhotons2[4],15];
K6 = CouplingConstant[ChPTVirtualPhotons2[4],16];
K7 = CouplingConstant[ChPTVirtualPhotons2[4],17];
K8 = CouplingConstant[ChPTVirtualPhotons2[4],18];
K9 = CouplingConstant[ChPTVirtualPhotons2[4],19];
K10= CouplingConstant[ChPTVirtualPhotons2[4],20];
K11 = CouplingConstant[ChPTVirtualPhotons2[4],21];
K12 = CouplingConstant[ChPTVirtualPhotons2[4],22];
K13 = CouplingConstant[ChPTVirtualPhotons2[4],23];
K14 = CouplingConstant[ChPTVirtualPhotons2[4],24];
K15 = CouplingConstant[ChPTVirtualPhotons2[4],25];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
      ChPTVirtualPhotons2[4],i_?((#<8)&),st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["l",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant /: MakeBoxes[
      CouplingConstant[
        ChPTVirtualPhotons2[4], i_?((7<#<11) &), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 7, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["h", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /. 
                  "jj" -> ToString[ii]];

CouplingConstant /: MakeBoxes[
      CouplingConstant[
        ChPTVirtualPhotons2[4], i_?((#>10)&), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 10, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["k", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /. 
                  "jj" -> ToString[ii]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTVirtualPhotons2[4]] =
{(* p^4 terms *)
1/3, 2/3, -1/2, 2, -1/6, -1/3, 0, 2, 1/12, 0,
(* e^2*p^2 terms *)
-27/20-1/5*z, 2*z, -3/4, 2*z, -1/4-1/5*z,
1/4+2*z, 0, 1/8-z, 1/4, 0, 0,
(* e^4 terms *)
3/2-12/5*z+84/25*z^2, -3-3/5*z-12/5*z^2, 3/2+3*z+12*z^2, 1/30} /.
z -> CouplingConstant[ChPTVirtualPhotons2[2],RenormalizationState[0]]/
DecayConstant[Pion,RenormalizationState[0]]^4;

(* ---------------------------------------------------------------- *)

Lagrangian[ChPTVirtualPhotons2[4]]:=

(* p^4 *)

L1[0]/4*
NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Mu]}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{\[Nu]}]], CDr[MM,{\[Nu]}]] ] ] +
    
L2[0]/4*
NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Nu]}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Nu]}]] ] ] +
    
L3[0]/16*
NM[UTrace[ NM[Adjoint[UChiMatrix],MM] + NM[Adjoint[MM],UChiMatrix] ],
   UTrace[ NM[Adjoint[UChiMatrix],MM] + NM[Adjoint[MM],UChiMatrix] ] ] +
   
L4[0]/4*
UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[UChiMatrix,{\[Mu]}]] +
        NM[Adjoint[CDr[UChiMatrix,{\[Mu]}]], CDr[MM,{\[Mu]}]] ] +

L5[0]*
UTrace[ NM[GRight[\[Mu],\[Nu]], Adjoint[GLeft[\[Mu],\[Nu]]]] ] +

L6[0]*I/2*
UTrace[ NM[GRight[\[Mu],\[Nu]], CDr[MM,{\[Mu]}], Adjoint[CDr[MM,{\[Nu]}]]] +
        NM[GLeft[\[Mu],\[Nu]], Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Nu]}]] ] -

L7[0]/16*
NM[UTrace[ NM[Adjoint[UChiMatrix], MM] - NM[Adjoint[MM], UChiMatrix] ] ,
   UTrace[ NM[Adjoint[UChiMatrix], MM] - NM[Adjoint[MM], UChiMatrix] ] ] +
    
(H1[0]+H3[0])/4*
UTrace[ NM[Adjoint[UChiMatrix], UChiMatrix] ] +

(H1[0]-H3[0])/2*
Re[Det[UChiMatrix]] -

H2[0]*
UTrace[ NM[GRight[\[Mu],\[Nu]], GRight[\[Mu],\[Nu]]] +
        NM[GLeft[\[Mu],\[Nu]], GLeft[\[Mu],\[Nu]]] ] +

(* e^2 p^2 *)
    
DecayConstant[Pion]^2*(

K1[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Mu]}]] ],
    UTrace[ NMPower[QQ, 2] ] ] +

K2[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], CDr[MM,{\[Mu]}]] ],
    UTrace[ NM[QQ, MM, QQ, Adjoint[MM]] ] ] +

K3[0]*
(NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], QQ, MM] ],
     UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], QQ, MM] ] ] +
 NM[ UTrace[ NM[CDr[MM,{\[Mu]}], QQ, Adjoint[MM]] ],
     UTrace[ NM[CDr[MM,{\[Mu]}], QQ, Adjoint[MM]] ] ])+

K4[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{\[Mu]}]], QQ, MM] ],
    UTrace[ NM[CDr[MM,{\[Mu]}], QQ, Adjoint[MM]] ] ] +

K5[0]*
NM[ UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix] ],
    UTrace[ NMPower[QQ, 2] ] ] +

K6[0]*
NM[ UTrace[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix] ],
    UTrace[ NM[QQ, MM, QQ, Adjoint[MM]] ] ] +

K7[0]*
NM[UTrace[ NM[ NM[UChiMatrix, Adjoint[MM]] + NM[MM, Adjoint[UChiMatrix]],
               QQ ] +
           NM[ NM[Adjoint[UChiMatrix], MM] + NM[Adjoint[MM], UChiMatrix],
               QQ ] ],
   UTrace[ QQ ]
] +

K8[0]*
UTrace[ NM[ NM[UChiMatrix, Adjoint[MM]] - NM[MM, Adjoint[UChiMatrix]],
        QQ, MM, QQ,Adjoint[MM] ] +
        NM[ NM[Adjoint[UChiMatrix], MM] - NM[Adjoint[MM], UChiMatrix],
        QQ, Adjoint[MM], QQ, MM ] ] +
        
K9[0]*
UTrace[ NM[ Adjoint[CDr[MM,{\[Mu]}]],
        NM[CQRight[\[Mu]], QQ] -
        NM[QQ, CQRight[\[Mu]]], MM ] +
        NM[ CDr[MM,{\[Mu]}],
        NM[CQLeft[\[Mu]], QQ] -
        NM[QQ, CQLeft[\[Mu]]], Adjoint[MM] ] ] +

K10[0]*
UTrace[ NM[ CQRight[\[Mu]], MM, CQLeft[\[Mu]], Adjoint[MM] ] ] +

K11[0]*
UTrace[ NM[ CQRight[\[Mu]], CQRight[\[Mu]] ] +
        NM[ CQLeft[\[Mu]], CQLeft[\[Mu]] ] ]

) +

(* e^4 *)

DecayConstant[Pion]^4*(

K12[0]*
NMPower[UTrace[ NM[QQ, QQ] ], 2] +

K13[0]*
NM[ UTrace[ NM[QQ, MM, QQ,Adjoint[MM]] ],
    UTrace[ NM[QQ, QQ] ] ] +

K14[0]*
NMPower[UTrace[ NM[QQ, MM, QQ, Adjoint[MM]]], 2]

) +

(* p^2 e^2 photon propagator counterterm *)

K15[0]*
UTrace[NMPower[QQ,2]]*
NM[FieldStrengthTensor[{\[Mu]}, QuantumField[Particle[Photon],{\[Nu]}]],
FieldStrengthTensor[{\[Mu]}, QuantumField[Particle[Photon],{\[Nu]}]]];

(* ---------------------------------------------------------------- *)

$Lagrangians=Union[$Lagrangians,{ChPTVirtualPhotons2[4]}];

FieldsSet[ChPTVirtualPhotons2[4]]:=
{IsoVector[QuantumField[Particle[Pion,RenormalizationState[0]]]],
QuantumField[Particle[Photon,RenormalizationState[0]]]};

