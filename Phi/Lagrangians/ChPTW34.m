(* *************************************************************** *)
(*                                                                 *)
(*                      ChPTW34                                    *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 2000

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The next to leading order CP conserving
                        delta s = 1 weak ChPT lagrangian.

                        Taken from Ecker, Kambor and Wyler
                        Resonances in the weak chiral Lagrangian,
                        CERN-TH.6610/92
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

ChPTW34::"usage"=
"ChPTW34.m is the name of the file containing the definitions for
Lagrangian[ChPTW3[4]], the lext to leading order CP conserving
weak ChPT lagrangian for delta S = 1.
To evaluate use ArgumentsSupply";

N1::"usage"=
"N1 := CouplingConstant[ChPTW3[4],1] is one of the constants of the
fourth order weak ChPT lagrangian";

N2::"usage"=
"N2 := CouplingConstant[ChPTW3[4],2] is one of the constants of the
fourth order weak ChPT lagrangian";

N3::"usage"=
"N3 := CouplingConstant[ChPTW3[4],3] is one of the constants of the
fourth order weak ChPT lagrangian";

N4::"usage"=
"N4 := CouplingConstant[ChPTW3[4],4] is one of the constants of the
fourth order weak ChPT lagrangian";

N5::"usage"=
"N5 := CouplingConstant[ChPTW3[4],5] is one of the constants of the
fourth order weak ChPT lagrangian";

N6::"usage"=
"L6 := CouplingConstant[ChPTW3[4],6] is one of the constants of the
fourth order weak ChPT lagrangian";

N7::"usage"=
"N7 := CouplingConstant[ChPTW3[4],7] is one of the constants of the
fourth order weak ChPT lagrangian";

N8::"usage"=
"N8 := CouplingConstant[ChPTW3[4],8] is one of the constants of the
fourth order weak ChPT lagrangian";

N9::"usage"=
"N9 := CouplingConstant[ChPTW3[4],9] is one of the constants of the
fourth order weak ChPT lagrangian";

N10::"usage"=
"N10 := CouplingConstant[ChPTW3[4],10] is one of the constants of the
fourth order weak ChPT lagrangian";

N11::"usage"=
"N11 := CouplingConstant[ChPTW3[4],11] is one of the constants of the
fourth order weak ChPT lagrangian";

N12::"usage"=
"N12 := CouplingConstant[ChPTW3[4],12] is one of the constants of the
fourth order weak ChPT lagrangian";

N13::"usage"=
"N13 := CouplingConstant[ChPTW3[4],13] is one of the constants of the
fourth order weak ChPT lagrangian";

N14::"usage"=
"N14 := CouplingConstant[ChPTW3[4],14] is one of the constants of the
fourth order weak ChPT lagrangian";

N15::"usage"=
"N15 := CouplingConstant[ChPTW3[4],15] is one of the constants of the
fourth order weak ChPT lagrangian";

N16::"usage"=
"N16 := CouplingConstant[ChPTW3[4],16] is one of the constants of the
fourth order weak ChPT lagrangian";

N17::"usage"=
"N17 := CouplingConstant[ChPTW3[4],17] is one of the constants of the
fourth order weak ChPT lagrangian";

N18::"usage"=
"N18 := CouplingConstant[ChPTW3[4],18] is one of the constants of the
fourth order weak ChPT lagrangian";

N19::"usage"=
"N19 := CouplingConstant[ChPTW3[4],19] is one of the constants of the
fourth order weak ChPT lagrangian";

N20::"usage"=
"N20 := CouplingConstant[ChPTW3[4],20] is one of the constants of the
fourth order weak ChPT lagrangian";

N21::"usage"=
"N21 := CouplingConstant[ChPTW3[4],21] is one of the constants of the
fourth order weak ChPT lagrangian";

N22::"usage"=
"N22 := CouplingConstant[ChPTW3[4],22] is one of the constants of the
fourth order weak ChPT lagrangian";

N23::"usage"=
"N23 := CouplingConstant[ChPTW3[4],23] is one of the constants of the
fourth order weak ChPT lagrangian";

N24::"usage"=
"N24 := CouplingConstant[ChPTW3[4],24] is one of the constants of the
fourth order weak ChPT lagrangian";

N25::"usage"=
"N25 := CouplingConstant[ChPTW3[4],25] is one of the constants of the
fourth order weak ChPT lagrangian";

N26::"usage"=
"N26 := CouplingConstant[ChPTW3[4],26] is one of the constants of the
fourth order weak ChPT lagrangian";

N27::"usage"=
"N27 := CouplingConstant[ChPTW3[4],27] is one of the constants of the
fourth order weak ChPT lagrangian";

N28::"usage"=
"N28 := CouplingConstant[ChPTW3[4],28] is one of the constants of the
fourth order weak ChPT lagrangian";

N29::"usage"=
"N29 := CouplingConstant[ChPTW3[4],29] is one of the constants of the
fourth order weak ChPT lagrangian";

N30::"usage"=
"N30 := CouplingConstant[ChPTW3[4],30] is one of the constants of the
fourth order weak ChPT lagrangian";

N31::"usage"=
"N31 := CouplingConstant[ChPTW3[4],31] is one of the constants of the
fourth order weak ChPT lagrangian";

N32::"usage"=
"N32 := CouplingConstant[ChPTW3[4],32] is one of the constants of the
fourth order weak ChPT lagrangian";

N33::"usage"=
"N33 := CouplingConstant[ChPTW3[4],33] is one of the constants of the
fourth order weak ChPT lagrangian";

N34::"usage"=
"N34 := CouplingConstant[ChPTW3[4],34] is one of the constants of the
fourth order weak ChPT lagrangian";

N35::"usage"=
"N35 := CouplingConstant[ChPTW3[4],35] is one of the constants of the
fourth order weak ChPT lagrangian";

N36::"usage"=
"N36 := CouplingConstant[ChPTW3[4],36] is one of the constants of the
fourth order weak ChPT lagrangian";

N37::"usage"=
"N37 := CouplingConstant[ChPTW3[4],37] is one of the constants of the
fourth order weak ChPT lagrangian";

Begin["`Private`"];

(* -------------------------------------------------------------- *)

(* Abbreviations *)

cc = HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant;

N1 := cc[ChPTW3[4],1];
N2 := cc[ChPTW3[4],2];
N3 := cc[ChPTW3[4],3];
N4 := cc[ChPTW3[4],4];
N5 := cc[ChPTW3[4],5];
N6 := cc[ChPTW3[4],6];
N7 := cc[ChPTW3[4],7];
N8 := cc[ChPTW3[4],8];
N9 := cc[ChPTW3[4],9];
N10 := cc[ChPTW3[4],10];
N11 := cc[ChPTW3[4],11];
N12 := cc[ChPTW3[4],12];
N13 := cc[ChPTW3[4],13];
N14 := cc[ChPTW3[4],14];
N15 := cc[ChPTW3[4],15];
N16 := cc[ChPTW3[4],16];
N17 := cc[ChPTW3[4],17];
N18 := cc[ChPTW3[4],18];
N19 := cc[ChPTW3[4],19];
N20 := cc[ChPTW3[4],20];
N21 := cc[ChPTW3[4],21];
N22 := cc[ChPTW3[4],22];
N23 := cc[ChPTW3[4],23];
N24 := cc[ChPTW3[4],24];
N25 := cc[ChPTW3[4],25];
N26 := cc[ChPTW3[4],26];
N27 := cc[ChPTW3[4],27];
N28 := cc[ChPTW3[4],28];
N29 := cc[ChPTW3[4],29];
N30 := cc[ChPTW3[4],30];
N31 := cc[ChPTW3[4],31];
N32 := cc[ChPTW3[4],32];
N33 := cc[ChPTW3[4],33];
N34 := cc[ChPTW3[4],34];
N35 := cc[ChPTW3[4],35];
N36 := cc[ChPTW3[4],36];
N37 := cc[ChPTW3[4],37];

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);
rho=(Global`\[Rho]);
sigma=(Global`\[Sigma]);

fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTW3[4]]:=
{2,-1/2,0,1,3/2,-1/4,-9/8,-1/2,3/4,2/3,-13/18,-5/12,0,1/4,1/2,-1/4,0,
-1/8,-5/4,3/4,5/6,5/6,0,0,1/2,-3/4,1/8,0,0,0,0,0,0,0,0,-5/12,-1/8};

(* ---------------------------------------------------------------- *)

(* Box definitions *)

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant/:
  MakeBoxes[
    HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[
      ChPTW3[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["N",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

HighEnergyPhysics`fctables`Lagrangian`Lagrangian[ChPTW3[4]]:=

cc[ChPTW3[2],1]/
DecayConstant[PhiMeson]^2*(

(* ............................................................... *)
(* K -> 3 Pi *)

N1*UTrace[ NM[UDelta, USmall[mu], USmall[mu], USmall[nu], USmall[nu]] ] +

N2*UTrace[ NM[UDelta, USmall[mu], USmall[nu], USmall[nu], USmall[mu]] ] +

N3*NM[UTrace[ NM[UDelta, USmall[mu], USmall[nu]] ],
   UTrace[ NM[USmall[mu], USmall[nu]] ]] +

N4*NM[UTrace[ NM[UDelta, USmall[mu]] ],
   UTrace[ NM[USmall[mu], USmall[nu], USmall[nu]] ]] +

(* ............................................................... *)
(* K -> 2 Pi, 3 Pi *)

N5*UTrace[ NM[UDelta, UChiPlus, USmall[mu], USmall[mu]] +
           NM[UDelta, USmall[mu], USmall[mu], UChiPlus] ] +

N6*NM[UTrace[ NM[UDelta, USmall[mu]] ],
   UTrace[ NM[UChiPlus, USmall[mu]] ]] +

N7*NM[UTrace[ NM[UDelta, UChiPlus] ],
   UTrace[ NM[USmall[mu], USmall[mu]] ]] +

N8*NM[UTrace[ NM[UDelta, USmall[mu], USmall[mu]] ],
   UTrace[ UChiPlus ]] +

N9*UTrace[ NM[UDelta, UChiMinus, USmall[mu], USmall[mu]] -
           NM[UDelta, USmall[mu], USmall[mu], UChiMinus] ] +

N10*UTrace[ NM[UDelta, UChiPlus, UChiPlus] ] +

N11*NM[UTrace[ NM[UDelta, UChiPlus] ],
    UTrace[ UChiPlus ]] +

N12*UTrace[ NM[UDelta, UChiMinus, UChiMinus] ] +

N13*NM[UTrace[ NM[UDelta, UChiMinus] ],
    UTrace[ UChiMinus ]] +

(* ............................................................... *)
(* Radiative K decays *)

N14*I*UTrace[ NM[UDelta, UFPlus[mu,nu], USmall[mu], USmall[nu]] +
           NM[UDelta, USmall[mu], USmall[nu], UFPlus[mu,nu]] ] +

N15*I*UTrace[ NM[UDelta, USmall[mu], UFPlus[mu,nu], USmall[nu]] ] +

N16*I*UTrace[ NM[UDelta, UFMinus[mu,nu], USmall[mu], USmall[nu]] +
           NM[UDelta, USmall[mu], USmall[nu], UFMinus[mu,nu]] ] +

N17*I*UTrace[ NM[UDelta, USmall[mu], UFMinus[mu,nu], USmall[nu]] ] +

N18*I*UTrace[ NM[UDelta, UFPlus[mu,nu], UFPlus[mu,nu]] -
              NM[UDelta, UFMinus[mu,nu], UFMinus[mu,nu]] ] +

(* ............................................................... *)
(* External W bosons*)

N19*I*UTrace[ NM[UNablaHatDelta[mu], USmall[mu], USmall[nu], USmall[nu]] -
             NM[UNablaHatDelta[mu], USmall[nu], USmall[nu], USmall[mu]] ] +

N20*UTrace[ NM[UNablaHatDelta[mu], UOmega[mu,nu], USmall[nu]] +
             NM[UNablaHatDelta[mu], USmall[nu], UOmega[mu,nu]] ] +

N21*I*UTrace[ NM[UNablaHatDelta[mu], UChiPlus, USmall[mu]] -
             NM[UNablaHatDelta[mu], USmall[mu], UChiPlus] ] +

(*N22*UTrace[ NM[UNablaHatDelta[mu], CovariantNabla[UChiPlus,{mu}]]  ] +*)
N22*UTrace[ NM[UNablaHatDelta[mu], NablaChiPlus[mu]]  ] +

N23*I*UTrace[ NM[UNablaHatDelta[mu], UChiMinus, USmall[mu]] +
             NM[UNablaHatDelta[mu], USmall[mu], UChiMinus] ] +

N24*I*NM[UTrace[ NM[UNablaHatDelta[mu], USmall[mu]] ],
      UTrace[ UChiMinus ]] +

N25*UTrace[ NM[UNablaHatDelta[mu], UFPlus[mu,nu], USmall[nu]] +
             NM[UNablaHatDelta[mu], USmall[nu], UFPlus[mu,nu]] ] +

N26*UTrace[ NM[UNablaHatDelta[mu], UFMinus[mu,nu], USmall[nu]] +
             NM[UNablaHatDelta[mu], USmall[nu], UFMinus[mu,nu]] ] +

N27*UTrace[ NM[2*UFPlus[mu,nu], UFPlus[mu,nu]] -
             NM[UFPlus[mu,nu], UFMinus[mu,nu]] -
     NM[UFMinus[mu,nu], UFPlus[mu,nu]] ]+

(* ............................................................... *)
(* Radiative K decays (anomalous couplings) *)

N28*I*LeviCivita[mu, nu, rho, sigma]*
NM[UTrace[ NM[UDelta, USmall[mu]] ],
UTrace[ NM[USmall[nu], USmall[rho], USmall[sigma]] ]]+

N29*UTrace[NM[UDelta,
NM[LeviCivita[mu, nu, rho, sigma]*
(UFPlus[rho, sigma]-UFMinus[rho, sigma])
USmall[mu], USmall[nu]] -
NM[USmall[mu], USmall[nu],
LeviCivita[mu, nu, rho, sigma]*
(UFPlus[rho, sigma]-UFMinus[rho, sigma])]] ]+

N30*NM[UTrace[ NM[UDelta, USmall[mu]] ],
UTrace[ NM[LeviCivita[mu, nu, rho, sigma],
UFPlus[rho, sigma], USmall[nu]] ]]+

N31*NM[UTrace[ NM[UDelta, USmall[mu]] ],
UTrace[ NM[LeviCivita[mu, nu, rho, sigma],
UFMinus[rho,sigma], USmall[nu]] ]]+

(* ............................................................... *)
(* External W bosons *)

N32*I*UTrace[
NM[UNablaHatDelta[mu],UDelta,
NM[
LeviCivita[mu, nu, rho, sigma]*UFPlus[rho, sigma],USmall[nu]]-
NM[
USmall[nu],LeviCivita[mu, nu, rho, sigma]*UFPlus[rho, sigma]]] ]+

N32*I*UTrace[
NM[UNablaHatDelta[mu],UDelta,
NM[
LeviCivita[mu, nu, rho, sigma]*UFMinus[rho, sigma],USmall[nu]]-
NM[
USmall[nu],LeviCivita[mu, nu, rho, sigma]*UFMinus[rho ,sigma]]] ]+

N34*UTrace[
NM[UDelta,
NM[
LeviCivita[mu, nu, rho, sigma]*UFPlus[rho, sigma]+
LeviCivita[mu, nu, rho, sigma]*UFMinus[rho, sigma],
USmall[mu], USmall[nu]]-
NM[
USmall[mu], USmall[nu],
LeviCivita[mu, nu, rho, sigma]*UFPlus[rho, sigma]+
LeviCivita[mu, nu, rho, sigma]*UFMinus[rho, sigma]]
] ]+

N35*I*UTrace[
NM[UDelta,
NM[
UFPlus[mu,nu],
LeviCivita[mu, nu, rho, sigma]*UFMinus[rho, sigma]]-
NM[
LeviCivita[mu, nu, rho, sigma]*UFMinus[rho, sigma],
UFPlus[mu, nu]]] ]+

(* ............................................................... *)
(* Renormalization *)

N36*UTrace[ NM[UDelta,
NM[UChiPlus, UChiMinus]-NM[UChiMinus, UChiPlus]+
NM[UChiPlus, UChiPlus]-NM[UChiMinus, UChiMinus]] ] +

N37*UTrace[ NM[UDelta,
UFPlus[mu,nu]+UFMinus[mu,nu],
UFPlus[mu,nu]+UFMinus[mu,nu]] ]

);

(* --------------------------------------------------------------- *)

FieldsSet[ChPTW3[2]]:=
{IsoVector[
fcqf[Particle[PhiMeson,RenormalizationState[0]]]
]};

Global`$Lagrangians=Union[Global`$Lagrangians,{ChPTW3[4]}];

End[];

End[];
