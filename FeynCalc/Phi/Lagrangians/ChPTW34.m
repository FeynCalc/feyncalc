(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTW34                                    *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana

   Year:                2000

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, PHI

   Summary:             Lagrangian for PHI

   Description:         The next to leading order CP conserving
                        delta s = 1 weak ChPT lagrangian.

                        Taken from Ecker, Kambor and Wyler
                        Resonances in the weak chiral Lagrangian,
                        CERN-TH.6610/92
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTW34::"usage"=
"ChPTW34.m is the name of the file containing the definitions for
Lagrangian[ChPTW3[4]], the lext to leading order CP conserving
weak ChPT lagrangian for delta S = 1.
To evaluate use ArgumentsSupply.";

N1::"usage"=
"N1 := CouplingConstant[ChPTW3[4],1] is one of the constants of the
fourth order weak ChPT lagrangian.";

N2::"usage"=
"N2 := CouplingConstant[ChPTW3[4],2] is one of the constants of the
fourth order weak ChPT lagrangian.";

N3::"usage"=
"N3 := CouplingConstant[ChPTW3[4],3] is one of the constants of the
fourth order weak ChPT lagrangian.";

N4::"usage"=
"N4 := CouplingConstant[ChPTW3[4],4] is one of the constants of the
fourth order weak ChPT lagrangian.";

N5::"usage"=
"N5 := CouplingConstant[ChPTW3[4],5] is one of the constants of the
fourth order weak ChPT lagrangian.";

N6::"usage"=
"L6 := CouplingConstant[ChPTW3[4],6] is one of the constants of the
fourth order weak ChPT lagrangian.";

N7::"usage"=
"N7 := CouplingConstant[ChPTW3[4],7] is one of the constants of the
fourth order weak ChPT lagrangian.";

N8::"usage"=
"N8 := CouplingConstant[ChPTW3[4],8] is one of the constants of the
fourth order weak ChPT lagrangian.";

N9::"usage"=
"N9 := CouplingConstant[ChPTW3[4],9] is one of the constants of the
fourth order weak ChPT lagrangian.";

N10::"usage"=
"N10 := CouplingConstant[ChPTW3[4],10] is one of the constants of the
fourth order weak ChPT lagrangian.";

N11::"usage"=
"N11 := CouplingConstant[ChPTW3[4],11] is one of the constants of the
fourth order weak ChPT lagrangian.";

N12::"usage"=
"N12 := CouplingConstant[ChPTW3[4],12] is one of the constants of the
fourth order weak ChPT lagrangian.";

N13::"usage"=
"N13 := CouplingConstant[ChPTW3[4],13] is one of the constants of the
fourth order weak ChPT lagrangian.";

N14::"usage"=
"N14 := CouplingConstant[ChPTW3[4],14] is one of the constants of the
fourth order weak ChPT lagrangian.";

N15::"usage"=
"N15 := CouplingConstant[ChPTW3[4],15] is one of the constants of the
fourth order weak ChPT lagrangian.";

N16::"usage"=
"N16 := CouplingConstant[ChPTW3[4],16] is one of the constants of the
fourth order weak ChPT lagrangian.";

N17::"usage"=
"N17 := CouplingConstant[ChPTW3[4],17] is one of the constants of the
fourth order weak ChPT lagrangian.";

N18::"usage"=
"N18 := CouplingConstant[ChPTW3[4],18] is one of the constants of the
fourth order weak ChPT lagrangian.";

N19::"usage"=
"N19 := CouplingConstant[ChPTW3[4],19] is one of the constants of the
fourth order weak ChPT lagrangian.";

N20::"usage"=
"N20 := CouplingConstant[ChPTW3[4],20] is one of the constants of the
fourth order weak ChPT lagrangian.";

N21::"usage"=
"N21 := CouplingConstant[ChPTW3[4],21] is one of the constants of the
fourth order weak ChPT lagrangian.";

N22::"usage"=
"N22 := CouplingConstant[ChPTW3[4],22] is one of the constants of the
fourth order weak ChPT lagrangian.";

N23::"usage"=
"N23 := CouplingConstant[ChPTW3[4],23] is one of the constants of the
fourth order weak ChPT lagrangian.";

N24::"usage"=
"N24 := CouplingConstant[ChPTW3[4],24] is one of the constants of the
fourth order weak ChPT lagrangian.";

N25::"usage"=
"N25 := CouplingConstant[ChPTW3[4],25] is one of the constants of the
fourth order weak ChPT lagrangian.";

N26::"usage"=
"N26 := CouplingConstant[ChPTW3[4],26] is one of the constants of the
fourth order weak ChPT lagrangian.";

N27::"usage"=
"N27 := CouplingConstant[ChPTW3[4],27] is one of the constants of the
fourth order weak ChPT lagrangian.";

N28::"usage"=
"N28 := CouplingConstant[ChPTW3[4],28] is one of the constants of the
fourth order weak ChPT lagrangian.";

N29::"usage"=
"N29 := CouplingConstant[ChPTW3[4],29] is one of the constants of the
fourth order weak ChPT lagrangian.";

N30::"usage"=
"N30 := CouplingConstant[ChPTW3[4],30] is one of the constants of the
fourth order weak ChPT lagrangian.";

N31::"usage"=
"N31 := CouplingConstant[ChPTW3[4],31] is one of the constants of the
fourth order weak ChPT lagrangian.";

N32::"usage"=
"N32 := CouplingConstant[ChPTW3[4],32] is one of the constants of the
fourth order weak ChPT lagrangian.";

N33::"usage"=
"N33 := CouplingConstant[ChPTW3[4],33] is one of the constants of the
fourth order weak ChPT lagrangian.";

N34::"usage"=
"N34 := CouplingConstant[ChPTW3[4],34] is one of the constants of the
fourth order weak ChPT lagrangian.";

N35::"usage"=
"N35 := CouplingConstant[ChPTW3[4],35] is one of the constants of the
fourth order weak ChPT lagrangian.";

N36::"usage"=
"N36 := CouplingConstant[ChPTW3[4],36] is one of the constants of the
fourth order weak ChPT lagrangian.";

N37::"usage"=
"N37 := CouplingConstant[ChPTW3[4],37] is one of the constants of the
fourth order weak ChPT lagrangian.";

(* -------------------------------------------------------------- *)

End[];

(* -------------------------------------------------------------- *)

(* Abbreviations *)

N1 = CouplingConstant[ChPTW3[4],1];
N2 = CouplingConstant[ChPTW3[4],2];
N3 = CouplingConstant[ChPTW3[4],3];
N4 = CouplingConstant[ChPTW3[4],4];
N5 = CouplingConstant[ChPTW3[4],5];
N6 = CouplingConstant[ChPTW3[4],6];
N7 = CouplingConstant[ChPTW3[4],7];
N8 = CouplingConstant[ChPTW3[4],8];
N9 = CouplingConstant[ChPTW3[4],9];
N10 = CouplingConstant[ChPTW3[4],10];
N11 = CouplingConstant[ChPTW3[4],11];
N12 = CouplingConstant[ChPTW3[4],12];
N13 = CouplingConstant[ChPTW3[4],13];
N14 = CouplingConstant[ChPTW3[4],14];
N15 = CouplingConstant[ChPTW3[4],15];
N16 = CouplingConstant[ChPTW3[4],16];
N17 = CouplingConstant[ChPTW3[4],17];
N18 = CouplingConstant[ChPTW3[4],18];
N19 = CouplingConstant[ChPTW3[4],19];
N20 = CouplingConstant[ChPTW3[4],20];
N21 = CouplingConstant[ChPTW3[4],21];
N22 = CouplingConstant[ChPTW3[4],22];
N23 = CouplingConstant[ChPTW3[4],23];
N24 = CouplingConstant[ChPTW3[4],24];
N25 = CouplingConstant[ChPTW3[4],25];
N26 = CouplingConstant[ChPTW3[4],26];
N27 = CouplingConstant[ChPTW3[4],27];
N28 = CouplingConstant[ChPTW3[4],28];
N29 = CouplingConstant[ChPTW3[4],29];
N30 = CouplingConstant[ChPTW3[4],30];
N31 = CouplingConstant[ChPTW3[4],31];
N32 = CouplingConstant[ChPTW3[4],32];
N33 = CouplingConstant[ChPTW3[4],33];
N34 = CouplingConstant[ChPTW3[4],34];
N35 = CouplingConstant[ChPTW3[4],35];
N36 = CouplingConstant[ChPTW3[4],36];
N37 = CouplingConstant[ChPTW3[4],37];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTW3[4]]:=
{2,-1/2,0,1,3/2,-1/4,-9/8,-1/2,3/4,2/3,-13/18,-5/12,0,1/4,1/2,-1/4,0,
-1/8,-5/4,3/4,5/6,5/6,0,0,1/2,-3/4,1/8,0,0,0,0,0,0,0,0,-5/12,-1/8};

(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
      ChPTW3[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["N",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

Lagrangian[ChPTW3[4]]:=

CouplingConstant[ChPTW3[2],1]/DecayConstant[PhiMeson]^2*(

(* ............................................................... *)
(* K -> 3 Pi *)

N1*UTrace[ NM[UDelta, USmall[\[Mu]], USmall[\[Mu]], USmall[\[Nu]], USmall[\[Nu]]] ] +

N2*UTrace[ NM[UDelta, USmall[\[Mu]], USmall[\[Nu]], USmall[\[Nu]], USmall[\[Mu]]] ] +

N3*NM[UTrace[ NM[UDelta, USmall[\[Mu]], USmall[\[Nu]]] ],
   UTrace[ NM[USmall[\[Mu]], USmall[\[Nu]]] ]] +

N4*NM[UTrace[ NM[UDelta, USmall[\[Mu]]] ],
   UTrace[ NM[USmall[\[Mu]], USmall[\[Nu]], USmall[\[Nu]]] ]] +

(* ............................................................... *)
(* K -> 2 Pi, 3 Pi *)

N5*UTrace[ NM[UDelta, UChiPlus, USmall[\[Mu]], USmall[\[Mu]]] +
           NM[UDelta, USmall[\[Mu]], USmall[\[Mu]], UChiPlus] ] +

N6*NM[UTrace[ NM[UDelta, USmall[\[Mu]]] ],
   UTrace[ NM[UChiPlus, USmall[\[Mu]]] ]] +

N7*NM[UTrace[ NM[UDelta, UChiPlus] ],
   UTrace[ NM[USmall[\[Mu]], USmall[\[Mu]]] ]] +

N8*NM[UTrace[ NM[UDelta, USmall[\[Mu]], USmall[\[Mu]]] ],
   UTrace[ UChiPlus ]] +

N9*UTrace[ NM[UDelta, UChiMinus, USmall[\[Mu]], USmall[\[Mu]]] -
           NM[UDelta, USmall[\[Mu]], USmall[\[Mu]], UChiMinus] ] +

N10*UTrace[ NM[UDelta, UChiPlus, UChiPlus] ] +

N11*NM[UTrace[ NM[UDelta, UChiPlus] ],
    UTrace[ UChiPlus ]] +

N12*UTrace[ NM[UDelta, UChiMinus, UChiMinus] ] +

N13*NM[UTrace[ NM[UDelta, UChiMinus] ],
    UTrace[ UChiMinus ]] +

(* ............................................................... *)
(* Radiative K decays *)

N14*I*UTrace[ NM[UDelta, UFPlus[\[Mu],\[Nu]], USmall[\[Mu]], USmall[\[Nu]]] +
           NM[UDelta, USmall[\[Mu]], USmall[\[Nu]], UFPlus[\[Mu],\[Nu]]] ] +

N15*I*UTrace[ NM[UDelta, USmall[\[Mu]], UFPlus[\[Mu],\[Nu]], USmall[\[Nu]]] ] +

N16*I*UTrace[ NM[UDelta, UFMinus[\[Mu],\[Nu]], USmall[\[Mu]], USmall[\[Nu]]] +
           NM[UDelta, USmall[\[Mu]], USmall[\[Nu]], UFMinus[\[Mu],\[Nu]]] ] +

N17*I*UTrace[ NM[UDelta, USmall[\[Mu]], UFMinus[\[Mu],\[Nu]], USmall[\[Nu]]] ] +

N18*I*UTrace[ NM[UDelta, UFPlus[\[Mu],\[Nu]], UFPlus[\[Mu],\[Nu]]] -
              NM[UDelta, UFMinus[\[Mu],\[Nu]], UFMinus[\[Mu],\[Nu]]] ] +

(* ............................................................... *)
(* External W bosons*)

N19*I*UTrace[ NM[UNablaHatDelta[\[Mu]], USmall[\[Mu]], USmall[\[Nu]], USmall[\[Nu]]] -
             NM[UNablaHatDelta[\[Mu]], USmall[\[Nu]], USmall[\[Nu]], USmall[\[Mu]]] ] +

N20*UTrace[ NM[UNablaHatDelta[\[Mu]], UOmega[\[Mu],\[Nu]], USmall[\[Nu]]] +
             NM[UNablaHatDelta[\[Mu]], USmall[\[Nu]], UOmega[\[Mu],\[Nu]]] ] +

N21*I*UTrace[ NM[UNablaHatDelta[\[Mu]], UChiPlus, USmall[\[Mu]]] -
             NM[UNablaHatDelta[\[Mu]], USmall[\[Mu]], UChiPlus] ] +

(*N22*UTrace[ NM[UNablaHatDelta[\[Mu]], CovariantNabla[UChiPlus,{\[Mu]}]]  ] +*)
N22*UTrace[ NM[UNablaHatDelta[\[Mu]], NablaChiPlus[\[Mu]]]  ] +

N23*I*UTrace[ NM[UNablaHatDelta[\[Mu]], UChiMinus, USmall[\[Mu]]] +
             NM[UNablaHatDelta[\[Mu]], USmall[\[Mu]], UChiMinus] ] +

N24*I*NM[UTrace[ NM[UNablaHatDelta[\[Mu]], USmall[\[Mu]]] ],
      UTrace[ UChiMinus ]] +

N25*UTrace[ NM[UNablaHatDelta[\[Mu]], UFPlus[\[Mu],\[Nu]], USmall[\[Nu]]] +
             NM[UNablaHatDelta[\[Mu]], USmall[\[Nu]], UFPlus[\[Mu],\[Nu]]] ] +

N26*UTrace[ NM[UNablaHatDelta[\[Mu]], UFMinus[\[Mu],\[Nu]], USmall[\[Nu]]] +
             NM[UNablaHatDelta[\[Mu]], USmall[\[Nu]], UFMinus[\[Mu],\[Nu]]] ] +

N27*UTrace[ NM[2*UFPlus[\[Mu],\[Nu]], UFPlus[\[Mu],\[Nu]]] -
             NM[UFPlus[\[Mu],\[Nu]], UFMinus[\[Mu],\[Nu]]] -
     NM[UFMinus[\[Mu],\[Nu]], UFPlus[\[Mu],\[Nu]]] ]+

(* ............................................................... *)
(* Radiative K decays (anomalous couplings) *)

N28*I*LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*
NM[UTrace[ NM[UDelta, USmall[\[Mu]]] ],
UTrace[ NM[USmall[\[Nu]], USmall[\[Rho]], USmall[\[Sigma]]] ]]+

N29*UTrace[NM[UDelta,
NM[LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*
(UFPlus[\[Rho], \[Sigma]]-UFMinus[\[Rho], \[Sigma]]),
USmall[\[Mu]], USmall[\[Nu]]] -
NM[USmall[\[Mu]], USmall[\[Nu]],
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*
(UFPlus[\[Rho], \[Sigma]]-UFMinus[\[Rho], \[Sigma]])]] ]+

N30*NM[UTrace[ NM[UDelta, USmall[\[Mu]]] ],
UTrace[ NM[LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]],
UFPlus[\[Rho], \[Sigma]], USmall[\[Nu]]] ]]+

N31*NM[UTrace[ NM[UDelta, USmall[\[Mu]]] ],
UTrace[ NM[LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]],
UFMinus[\[Rho],\[Sigma]], USmall[\[Nu]]] ]]+

(* ............................................................... *)
(* External W bosons *)

N32*I*UTrace[
NM[UNablaHatDelta[\[Mu]],UDelta,
NM[
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFPlus[\[Rho], \[Sigma]],USmall[\[Nu]]]-
NM[
USmall[\[Nu]],LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFPlus[\[Rho], \[Sigma]]]] ]+

N32*I*UTrace[
NM[UNablaHatDelta[\[Mu]],UDelta,
NM[
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho], \[Sigma]],USmall[\[Nu]]]-
NM[
USmall[\[Nu]],LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho] ,\[Sigma]]]] ]+

N34*UTrace[
NM[UDelta,
NM[
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFPlus[\[Rho], \[Sigma]]+
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho], \[Sigma]],
USmall[\[Mu]], USmall[\[Nu]]]-
NM[
USmall[\[Mu]], USmall[\[Nu]],
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFPlus[\[Rho], \[Sigma]]+
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho], \[Sigma]]]
] ]+

N35*I*UTrace[
NM[UDelta,
NM[
UFPlus[\[Mu],\[Nu]],
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho], \[Sigma]]]-
NM[
LeviCivita[\[Mu], \[Nu], \[Rho], \[Sigma]]*UFMinus[\[Rho], \[Sigma]],
UFPlus[\[Mu], \[Nu]]]] ]+

(* ............................................................... *)
(* Renormalization *)

N36*UTrace[ NM[UDelta,
NM[UChiPlus, UChiMinus]-NM[UChiMinus, UChiPlus]+
NM[UChiPlus, UChiPlus]-NM[UChiMinus, UChiMinus]] ] +

N37*UTrace[ NM[UDelta,
UFPlus[\[Mu],\[Nu]]+UFMinus[\[Mu],\[Nu]],
UFPlus[\[Mu],\[Nu]]+UFMinus[\[Mu],\[Nu]]] ]

);

(* --------------------------------------------------------------- *)

FieldsSet[ChPTW3[2]]:=
{IsoVector[
QuantumField[Particle[PhiMeson,RenormalizationState[0]]]
]};

$Lagrangians=Union[$Lagrangians,{ChPTW3[4]}];
