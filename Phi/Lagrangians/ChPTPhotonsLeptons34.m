(* ************************************************************** *)
(*                                                                *)
(*                       ChPTPhotonsLeptons34                     *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana 2001

   Mathematica Version: 4.0

   Requirements:        Feyncalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The next to leading order ChPT lagrangian
                        with electromagnetic couplings.

                        Taken from
                        M. Knecht, H. Neufeld, H. Rupertsberger, P. Talavera
                        (1999), hep-ph/9909284
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTPhotonsLeptons34::"usage"=
   "ChPTPhotonsLeptons34 is the name of the file containing the definitions for \
 Lagrangian[ChPTPhotonsLeptons3[4]], which is the next-to-leading order mesonic \
SU(3) ChPT lagrangian with couplings to virtual photons and leptons. \
To evaluate use ArgumentsSupply";

L1::"usage"=
"L1 := CouplingConstant[ChPTPhotonsLeptons3[4],1] is one of the constants of the
fourth order ChPT lagrangian";

L2::"usage"=
"L2 := CouplingConstant[ChPTPhotonsLeptons3[4],2] is one of the constants of the
fourth order ChPT lagrangian";

L3::"usage"=
"L3 := CouplingConstant[ChPTPhotonsLeptons3[4],3] is one of the constants of the
fourth order ChPT lagrangian";

L4::"usage"=
"L4 := CouplingConstant[ChPTPhotonsLeptons3[4],4] is one of the constants of the
fourth order ChPT lagrangian";

L5::"usage"=
"L5 := CouplingConstant[ChPTPhotonsLeptons3[4],5] is one of the constants of the
fourth order ChPT lagrangian";

L6::"usage"=
"L6 := CouplingConstant[ChPTPhotonsLeptons3[4],6] is one of the constants of the
fourth order ChPT lagrangian";

L7::"usage"=
"L7 := CouplingConstant[ChPTPhotonsLeptons3[4],7] is one of the constants of the
fourth order ChPT lagrangian";

L8::"usage"=
"L8 := CouplingConstant[ChPTPhotonsLeptons3[4],8] is one of the constants of the
fourth order ChPT lagrangian";

L9::"usage"=
"L9 := CouplingConstant[ChPTPhotonsLeptons3[4],9] is one of the constants of the
fourth order ChPT lagrangian";

L10::"usage"=
"L10 := CouplingConstant[ChPTPhotonsLeptons3[4],10] is one of the constants of the
fourth order ChPT lagrangian";

H1::"usage"=
"H1 := CouplingConstant[ChPTPhotonsLeptons3[4],11] is one of the constants of the
fourth order ChPT lagrangian";

H2::"usage"=
"H2 := CouplingConstant[ChPTPhotonsLeptons3[4],12] is one of the constants of the
fourth order ChPT lagrangian";

K1::"usage"=
"K1 := CouplingConstant[ChPTPhotonsLeptons3[4],13] is one of the constants of the
fourth order ChPT lagrangian";

K2::"usage"=
"K2 := CouplingConstant[ChPTPhotonsLeptons3[4],14] is one of the constants of the
fourth order ChPT lagrangian";

K3::"usage"=
"K3 := CouplingConstant[ChPTPhotonsLeptons3[4],15] is one of the constants of the
fourth order ChPT lagrangian";

K4::"usage"=
"K4 := CouplingConstant[ChPTPhotonsLeptons3[4],16] is one of the constants of the
fourth order ChPT lagrangian";

K5::"usage"=
"K5 := CouplingConstant[ChPTPhotonsLeptons3[4],17] is one of the constants of the
fourth order ChPT lagrangian";

K6::"usage"=
"K6 := CouplingConstant[ChPTPhotonsLeptons3[4],18] is one of the constants of the
fourth order ChPT lagrangian";

K7::"usage"=
"K7 := CouplingConstant[ChPTPhotonsLeptons3[4],19] is one of the constants of the
fourth order ChPT lagrangian";

K8::"usage"=
"K8 := CouplingConstant[ChPTPhotonsLeptons3[4],20] is one of the constants of the
fourth order ChPT lagrangian";

K9::"usage"=
"K9 := CouplingConstant[ChPTPhotonsLeptons3[4],21] is one of the constants of the
fourth order ChPT lagrangian";

K10::"usage"=
"K10 := CouplingConstant[ChPTPhotonsLeptons3[4],22] is one of the constants of the
fourth order ChPT lagrangian";

K11::"usage"=
"K11 := CouplingConstant[ChPTPhotonsLeptons3[4],23] is one of the constants of the
fourth order ChPT lagrangian";

K12::"usage"=
"K12 := CouplingConstant[ChPTPhotonsLeptons3[4],24] is one of the constants of the
fourth order ChPT lagrangian";

K13::"usage"=
"K13 := CouplingConstant[ChPTPhotonsLeptons3[4],25] is one of the constants of the
fourth order ChPT lagrangian";

K14::"usage"=
"K14 := CouplingConstant[ChPTPhotonsLeptons3[4],26] is one of the constants of the
fourth order ChPT lagrangian";

K15::"usage"=
"K15 := CouplingConstant[ChPTPhotonsLeptons3[4],27] is one of the constants of the
fourth order ChPT lagrangian";

K16::"usage"=
"K16 := CouplingConstant[ChPTPhotonsLeptons3[4],28] is one of the constants of the
fourth order ChPT lagrangian";

K17::"usage"=
"K17 := CouplingConstant[ChPTPhotonsLeptons3[4],29] is one of the constants of the
fourth order ChPT lagrangian";

K18::"usage"=
"K18 := CouplingConstant[ChPTPhotonsLeptons3[4],30] is one of the constants of the
fourth order ChPT lagrangian";

K19::"usage"=
"K19 := CouplingConstant[ChPTPhotonsLeptons3[4],31] is one of the constants of the
fourth order ChPT lagrangian";

(* XX1 etc. is used because FeynArts uses Unique["X"] *)

XX1::"usage"=
"XX1 := CouplingConstant[ChPTPhotonsLeptons3[4],32] is one of the constants of the
fourth order ChPT lagrangian";

XX2::"usage"=
"XX2 := CouplingConstant[ChPTPhotonsLeptons3[4],33] is one of the constants of the
fourth order ChPT lagrangian";

XX3::"usage"=
"XX3 := CouplingConstant[ChPTPhotonsLeptons3[4],34] is one of the constants of the
fourth order ChPT lagrangian";

XX4::"usage"=
"XX4 := CouplingConstant[ChPTPhotonsLeptons3[4],35] is one of the constants of the
fourth order ChPT lagrangian";

XX5::"usage"=
"XX5 := CouplingConstant[ChPTPhotonsLeptons3[4],36] is one of the constants of the
fourth order ChPT lagrangian";

XX6::"usage"=
"XX6 := CouplingConstant[ChPTPhotonsLeptons3[4],37] is one of the constants of the
fourth order ChPT lagrangian";

XX7::"usage"=
"XX7 := CouplingConstant[ChPTPhotonsLeptons3[4],38] is one of the constants of the
fourth order ChPT lagrangian";



(* ---------------------------------------------------------------- *)

Begin["`Private`"];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

QQ := UQuarkChargeMatrix;

fcqf := HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcsuni:=HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
iii=(Global`i);
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;


L1 := CouplingConstant[ChPTPhotonsLeptons3[4],1];
L2 := CouplingConstant[ChPTPhotonsLeptons3[4],2];
L3 := CouplingConstant[ChPTPhotonsLeptons3[4],3];
L4 := CouplingConstant[ChPTPhotonsLeptons3[4],4];
L5 := CouplingConstant[ChPTPhotonsLeptons3[4],5];
L6 := CouplingConstant[ChPTPhotonsLeptons3[4],6];
L7 := CouplingConstant[ChPTPhotonsLeptons3[4],7];
L8 := CouplingConstant[ChPTPhotonsLeptons3[4],8];
L9 := CouplingConstant[ChPTPhotonsLeptons3[4],9];
L10 := CouplingConstant[ChPTPhotonsLeptons3[4],10];

H1 := CouplingConstant[ChPTPhotonsLeptons3[4],11];
H2 := CouplingConstant[ChPTPhotonsLeptons3[4],12];

K1 := CouplingConstant[ChPTPhotonsLeptons3[4],13];
K2 := CouplingConstant[ChPTPhotonsLeptons3[4],14];
K3 := CouplingConstant[ChPTPhotonsLeptons3[4],15];
K4 := CouplingConstant[ChPTPhotonsLeptons3[4],16];
K5 := CouplingConstant[ChPTPhotonsLeptons3[4],17];
K6 := CouplingConstant[ChPTPhotonsLeptons3[4],18];
K7 := CouplingConstant[ChPTPhotonsLeptons3[4],19];
K8 := CouplingConstant[ChPTPhotonsLeptons3[4],20];
K9 := CouplingConstant[ChPTPhotonsLeptons3[4],21];
K10:= CouplingConstant[ChPTPhotonsLeptons3[4],22];
K11 := CouplingConstant[ChPTPhotonsLeptons3[4],23];
K12 := CouplingConstant[ChPTPhotonsLeptons3[4],24];
K13 := CouplingConstant[ChPTPhotonsLeptons3[4],25];
K14 := CouplingConstant[ChPTPhotonsLeptons3[4],26];
K15 := CouplingConstant[ChPTPhotonsLeptons3[4],27];
K16 := CouplingConstant[ChPTPhotonsLeptons3[4],28];
K17 := CouplingConstant[ChPTPhotonsLeptons3[4],29];
K18 := CouplingConstant[ChPTPhotonsLeptons3[4],30];
K19 := CouplingConstant[ChPTPhotonsLeptons3[4],31];

XX1 := CouplingConstant[ChPTPhotonsLeptons3[4],32];
XX2 := CouplingConstant[ChPTPhotonsLeptons3[4],33];
XX3 := CouplingConstant[ChPTPhotonsLeptons3[4],34];
XX4 := CouplingConstant[ChPTPhotonsLeptons3[4],35];
XX5 := CouplingConstant[ChPTPhotonsLeptons3[4],36];
XX6 := CouplingConstant[ChPTPhotonsLeptons3[4],37];
XX7 := CouplingConstant[ChPTPhotonsLeptons3[4],38];


(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPTPhotonsLeptons3[4],i_?((#<11)&),st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["L",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant /: MakeBoxes[
      CouplingConstant[ChPTPhotonsLeptons3[4], i_?((10<#<13) &), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 10, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["H", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /.
                  "HighEnergyPhysics`Phi`Objects`Private`jj" -> ToString[ii]];

CouplingConstant /: MakeBoxes[
      CouplingConstant[ChPTPhotonsLeptons3[4], i_?((12<#<32)&), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 12, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["K", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /.
                  "HighEnergyPhysics`Phi`Objects`Private`jj" -> ToString[ii]];

CouplingConstant /: MakeBoxes[
      CouplingConstant[ChPTPhotonsLeptons3[4], i_?((31<#)&), st___RenormalizationState,
        sc___RenormalizationScheme, qs___QuarkMassExpansionState],
      TraditionalForm] :=
    Block[{ii = i - 31, jj},
      SubsuperscriptBox[MakeBoxes[StyleForm["X", FontSlant -> "Italic"]][[1]],
           MakeBoxes[TraditionalForm[jj]],
          RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
                  TraditionalForm[IndexBox[qs]]]}]]] /.
                  "HighEnergyPhysics`Phi`Objects`Private`jj" -> ToString[ii]];


(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTPhotonsLeptons3[4]] =
{3/32,3/16,0,1/8,3/8,11/144,0,5/48,1/4,-1/4,-1/8,5/24,
3/4, z, -3/4, 2 z, -9/4, 3/2 z, 0, z, -1/4, 1/4+3/2 z, 1/8, 1/4, 0, 0, 3/2 + 3 z + 20 z^2,
0,-3/4,-3,-3/2,3/2,-5,-1,-4/3} /.
z -> CouplingConstant[ChPTPhotonsLeptons3[2],RenormalizationState[0]]/
DecayConstant[PhiMeson,RenormalizationState[0]]^4;

(* ---------------------------------------------------------------- *)

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);

(* ---------------------------------------------------------------- *)


HighEnergyPhysics`fctables`Lagrangian`Lagrangian[
ChPTPhotonsLeptons3[4]]:=

(* p^4 *)

L1[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ] ] +

L2[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}]] ],
    UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{nu}]] ] ] +

L3[0]*
UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}],
                            Adjoint[CDr[MM,{nu}]], CDr[MM,{nu}]] ] +

L4[0]*
NM[ UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}]] ],
    UTrace[ NM[Adjoint[UChiMatrix],MM] + NM[UChiMatrix, Adjoint[MM]] ] ] +

L5[0]*
UTrace[ NM[Adjoint[CDr[MM,{mu}]], CDr[MM,{mu}],
                           NM[ Adjoint[UChiMatrix],MM] + NM[Adjoint[MM], UChiMatrix] ] ] +

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
UTrace[ NM[GRight[mu,nu], MM, GLeft[mu,nu], Adjoint[MM]] ] +

H1[0]*
UTrace[ NM[GRight[mu,nu], GRight[mu,nu]] +
              NM[GLeft[mu,nu], GLeft[mu,nu]] ] +

H2[0]*
UTrace[ NM[Adjoint[UChiMatrix], UChiMatrix] ] +

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
        NM[ CQLeft[mu], CQLeft[mu] ] ]+

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
NM[FieldStrengthTensor[{mu}, fcqf[Particle[Photon],fcli[nu]]],
FieldStrengthTensor[{mu}, fcqf[Particle[Photon],fcli[nu]]]]+

(* Lepton terms *)

XX1[0]*DecayConstant[PhiMeson]^2*CouplingConstant[QED[1]]^2*
NM[DiracBar[fcqf[Particle[Lepton],fcsuni[iii]]].
fcdm[fcli[mu]].
fcdm[7].
fcqf[Particle[Neutrino],fcsuni[iii]],
UTrace[NM[USmall[mu],UAntiCommutator[QQ,UChiralSpurionLeft1Matrix]]]]+

XX2[0]*DecayConstant[PhiMeson]^2*CouplingConstant[QED[1]]^2*
NM[DiracBar[fcqf[Particle[Lepton],fcsuni[iii]]].
fcdm[fcli[mu]].
fcdm[7].
fcqf[Particle[Neutrino],fcsuni[iii]],
UTrace[NM[USmall[mu],UCommutator[QQ,UChiralSpurionLeft1Matrix]]]]+

XX3[0]*DecayConstant[PhiMeson]^2*CouplingConstant[QED[1]]^2*
ParticleMass[Lepton,fcsuni[ii]]*
NM[DiracBar[fcqf[Particle[Lepton],fcsuni[iii]]].
fcdm[7].
fcqf[Particle[Neutrino],fcsuni[iii]],
UTrace[NM[UChiralSpurionLeft1Matrix,QQ]]]+

XX4[0]*DecayConstant[PhiMeson]^2*CouplingConstant[QED[1]]^2*
I*NM[DiracBar[fcqf[Particle[Lepton],fcsuni[iii]]].
fcdm[fcli[mu]].
fcdm[7].
fcqf[Particle[Neutrino],fcsuni[iii]],
UTrace[NM[UChiralSpurionLeft1Matrix,CQLeft[mu]]]]+

XX5[0]*DecayConstant[PhiMeson]^2*CouplingConstant[QED[1]]^2*
I*NM[DiracBar[fcqf[Particle[Lepton],fcsuni[iii]]].
fcdm[fcli[mu]].
fcdm[7].
fcqf[Particle[Neutrino],fcsuni[iii]],
UTrace[NM[UChiralSpurionLeft1Matrix,CQRight[mu]]]]+

XX6[0]*CouplingConstant[QED[1]]^2*
DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcdm[fcli[mu]].
(I*fcqf[fcpd[fcli[mu]],
Particle[Lepton],fcsuni[ii]]+
CouplingConstant[QED[1]]*
fcqf[Particle[Photon],fcli[mu]].
fcqf[Particle[Lepton],fcsuni[ii]])+

XX7[0]*CouplingConstant[QED[1]]^2*
ParticleMass[Lepton,fcsuni[ii]]*
DiracBar[fcqf[Particle[Lepton],fcsuni[ii]]].
fcqf[Particle[Lepton],fcsuni[ii]];

(* ---------------------------------------------------------------- *)

Global`$Lagrangians=Union[Global`$Lagrangians,{ChPTPhotonsLeptons3[4]}];

FieldsSet[ChPTPhotonsLeptons3[4]]:=
{IsoVector[fcqf[Particle[PhiMeson,RenormalizationState[0]]]],
fcqf[Particle[Photon,RenormalizationState[0]]]};

End[];

End[];
