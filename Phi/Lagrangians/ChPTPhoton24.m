(* ************************************************************** *)
(*                                                                *)
(*                       ChPTPhoton24                             *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana 1999

   Additional contexts: HighEnergyPhysics`Phi`Objects`,
                        HighEnergyPhysics`Phi`ChPTRenormalization`

   Mathematica Version: 3.0

   Requirements:        Feyncalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The ChPT lagrangian for pion to fourth
                        order in the energy (mass).

                        Taken from J.F. Donoghue, E. Golowich
                        and B.R. Holstein (1992), "Dynamics of
                        the Standard Model", Cambridge

*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTPhoton24::"usage"=
"ChPTPhoton24.m is the name of the file containing the definitions for
ULagrangian[ChPTPhoton2[4]], which is the pionic ChPT lagrangian to
fourth order in the energy, first written down by
Gasser and Leutwyler.  To evaluate use ULagrangian";

L1::"usage"=
"L1 := UCouplingConstant[ChPTPhoton2[4],1] is one of the constants of the
fourth order ChPT lagrangian";

L2::"usage"=
"L2 := UCouplingConstant[ChPTPhoton2[4],2] is one of the constants of the
fourth order ChPT lagrangian";

L3::"usage"=
"L3 := UCouplingConstant[ChPTPhoton2[4],3] is one of the constants of the
fourth order ChPT lagrangian";

L4::"usage"=
"L4 := UCouplingConstant[ChPTPhoton2[4],4] is one of the constants of the
fourth order ChPT lagrangian";

L5::"usage"=
"L5 := UCouplingConstant[ChPTPhoton2[4],5] is one of the constants of the
fourth order ChPT lagrangian";

L6::"usage"=
"L6 := UCouplingConstant[ChPTPhoton2[4],6] is one of the constants of the
fourth order ChPT lagrangian";

L7::"usage"=
"L7 := UCouplingConstant[ChPTPhoton2[4],7] is one of the constants of the
fourth order ChPT lagrangian";

L8::"usage"=
"L8 := UCouplingConstant[ChPTPhoton2[4],8] is one of the constants of the
fourth order ChPT lagrangian";

L9::"usage"=
"L9 := UCouplingConstant[ChPTPhoton2[4],9] is one of the constants of the
fourth order ChPT lagrangian";

L10::"usage"=
"L10 := UCouplingConstant[ChPTPhoton2[4],10] is one of the constants of the
fourth order ChPT lagrangian";

H1::"usage"=
"H1 := UCouplingConstant[ChPTPhoton2[4],11] is one of the constants of the
fourth order ChPT lagrangian";

H2::"usage"=
"H2 := UCouplingConstant[ChPTPhoton2[4],12] is one of the constants of the
fourth order ChPT lagrangian";

(* ---------------------------------------------------------------- *)

Begin["`Private`"];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

L1 := UCouplingConstant[ChPTPhoton2[4],1];
L2 := UCouplingConstant[ChPTPhoton2[4],2];
L3 := UCouplingConstant[ChPTPhoton2[4],3];
L4 := UCouplingConstant[ChPTPhoton2[4],4];
L5 := UCouplingConstant[ChPTPhoton2[4],5];
L6 := UCouplingConstant[ChPTPhoton2[4],6];
L7 := UCouplingConstant[ChPTPhoton2[4],7];
L8 := UCouplingConstant[ChPTPhoton2[4],8];
L9 := UCouplingConstant[ChPTPhoton2[4],9];
L10:= UCouplingConstant[ChPTPhoton2[4],10];
H1 := UCouplingConstant[ChPTPhoton2[4],11];
H2 := UCouplingConstant[ChPTPhoton2[4],12];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTPhoton2[4],11,st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["H",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTPhoton2[4],12,st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["H",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTPhoton2[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["L",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTPhoton2[4]]:=
{1/12,1/6,0,1/8,1/4,3/32,0,0,1/6,-1/6};

(* ---------------------------------------------------------------- *)

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);

(* ---------------------------------------------------------------- *)

ULagrangian[ChPTPhoton2[4]]:=

L1[0]*
NM[ UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]]],
    UTrace[ NM[CDr[MM,{nu}],Adjoint[CDr[MM,{nu}]]] ] ]+

L2[0]*
NM[ UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{nu}]]] ],
    UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{nu}]]] ] ]+

L3[0]*
UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]],
           CDr[MM,{nu}],Adjoint[CDr[MM,{nu}]]] ]+

L4[0]*
NM[ UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]]],
    UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ]+

L5[0]*
UTrace[ NM[NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]],
        NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]]] ]+

L6[0]*
NM[UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ,
   UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ]+

L7[0]*
NM[UTrace[ NM[Adjoint[UChiMatrix],MM]-NM[Adjoint[MM],UChiMatrix] ] ,
   UTrace[ NM[Adjoint[UChiMatrix],MM]-NM[Adjoint[MM],UChiMatrix] ] ]+

L8[0]*
UTrace[ NM[UChiMatrix,Adjoint[MM],UChiMatrix,Adjoint[MM]]+
        NM[MM,Adjoint[UChiMatrix],MM,Adjoint[UChiMatrix]] ]+

L9[0]*I*
UTrace[
NM[FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix,
CDr[MM,{mu}],Adjoint[CDr[MM,{nu}]]]+

NM[FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix,
Adjoint[CDr[MM,{mu}]],CDr[MM,{nu}]] ]+

L10[0]*
UTrace[
NM[FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix,MM,
FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix,Adjoint[MM]] ]+

H1[0]*
2*UTrace[
NM[FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix,
FieldStrengthTensor[{mu},fcqf[Particle[Photon],{nu}]],
UQuarkChargeMatrix] ]+

H2[0]*
UTrace[ NM[Adjoint[UChiMatrix],UChiMatrix] ];

(* ---------------------------------------------------------------- *)

$ULagrangians=Union[$ULagrangians,{ChPTPhoton2[4]}];

FieldsSet[ChPTPhoton2[4]]:=
{IsoVector[
fcqf[Particle[Pion,RenormalizationState[0]]],
fcqf[Particle[Photon,RenormalizationState[0]],LorentzIndex[\[Mu]]]
]};

End[];

End[];
