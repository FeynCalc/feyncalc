(* ************************************************************** *)
(*                                                                *)
(*                       ChPT34                                   *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana 1997

   Additional contexts: HighEnergyPhysics`Phi`ChPTRenormalization`

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

ChPT34::"usage"=
"ChPT34.m is the name of the file containing the definitions for
Lagrangian[ChPT3[4]], which is the mesonic ChPT lagrangian to fourth
order in the energy, first written down by Gasser and Leutwyler.
To evaluate use ArgumentsSupply";

L1::"usage"=
"L1 := CouplingConstant[ChPT3[4],1] is one of the constants of the
fourth order ChPT lagrangian";

L2::"usage"=
"L2 := CouplingConstant[ChPT3[4],2] is one of the constants of the
fourth order ChPT lagrangian";

L3::"usage"=
"L3 := CouplingConstant[ChPT3[4],3] is one of the constants of the
fourth order ChPT lagrangian";

L4::"usage"=
"L4 := CouplingConstant[ChPT3[4],4] is one of the constants of the
fourth order ChPT lagrangian";

L5::"usage"=
"L5 := CouplingConstant[ChPT3[4],5] is one of the constants of the
fourth order ChPT lagrangian";

L6::"usage"=
"L6 := CouplingConstant[ChPT3[4],6] is one of the constants of the
fourth order ChPT lagrangian";

L7::"usage"=
"L7 := CouplingConstant[ChPT3[4],7] is one of the constants of the
fourth order ChPT lagrangian";

L8::"usage"=
"L8 := CouplingConstant[ChPT3[4],8] is one of the constants of the
fourth order ChPT lagrangian";

L9::"usage"=
"L9 := CouplingConstant[ChPT3[4],9] is one of the constants of the
fourth order ChPT lagrangian";

L10::"usage"=
"L10 := CouplingConstant[ChPT3[4],10] is one of the constants of the
fourth order ChPT lagrangian";

H1::"usage"=
"H1 := CouplingConstant[ChPT3[4],11] is one of the constants of the
fourth order ChPT lagrangian";

H2::"usage"=
"H2 := CouplingConstant[ChPT3[4],12] is one of the constants of the
fourth order ChPT lagrangian";

(* ---------------------------------------------------------------- *)

End[];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

L1 = CouplingConstant[ChPT3[4],1];
L2 = CouplingConstant[ChPT3[4],2];
L3 = CouplingConstant[ChPT3[4],3];
L4 = CouplingConstant[ChPT3[4],4];
L5 = CouplingConstant[ChPT3[4],5];
L6 = CouplingConstant[ChPT3[4],6];
L7 = CouplingConstant[ChPT3[4],7];
L8 = CouplingConstant[ChPT3[4],8];
L9 = CouplingConstant[ChPT3[4],9];
L10= CouplingConstant[ChPT3[4],10];
H1 = CouplingConstant[ChPT3[4],11];
H2 = CouplingConstant[ChPT3[4],12];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPT3[4],11,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["H",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPT3[4],12,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["H",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPT3[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["L",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPT3[4]]:=
{3/32,3/16,0,1/8,3/8,11/144,0,5/48,1/4,-1/4,-1/8,5/24};

(* ---------------------------------------------------------------- *)

Lagrangian[ChPT3[4]]:=

L1[0]*
NM[ UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]]],
    UTrace[ NM[CDr[MM,{\[Nu]}],Adjoint[CDr[MM,{\[Nu]}]]] ] ]+

L2[0]*
NM[ UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Nu]}]]] ],
    UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Nu]}]]] ] ]+

L3[0]*
UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]],
           CDr[MM,{\[Nu]}],Adjoint[CDr[MM,{\[Nu]}]]] ]+

L4[0]*
NM[ UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]]],
    UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ]+

L5[0]*
UTrace[ NM[NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]],
        NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]]] ]+

L6[0]*
NM[UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ,
   UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ] ]+

L7[0]*
NM[UTrace[ NM[UChiMatrix,Adjoint[MM]]-NM[MM,Adjoint[UChiMatrix]] ] ,
   UTrace[ NM[UChiMatrix,Adjoint[MM]]-NM[MM,Adjoint[UChiMatrix]] ] ]+

L8[0]*
UTrace[ NM[UChiMatrix,Adjoint[MM],UChiMatrix,Adjoint[MM]]+
        NM[MM,Adjoint[UChiMatrix],MM,Adjoint[UChiMatrix]] ]+

(* Electromagnetic case *)

(*L9[0]*I*
UTrace[

NM[FieldStrengthTensor[{\[Mu]},QuantumField[Particle[Photon],{\[Nu]}]],
UQuarkChargeMatrix,
CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Nu]}]]] +

NM[FieldStrengthTensor[{\[Mu]},QuantumField[Particle[Photon],{\[Nu]}]],
UQuarkChargeMatrix,
Adjoint[CDr[MM,{\[Mu]}]],CDr[MM,{\[Nu]}]]

]*)

(* General case *)

L9[0]*I*
UTrace[ NM[FST[LeftComponent[0],{\[Mu]},{\[Nu]}],
           CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Nu]}]]]+
        NM[FST[RightComponent[0],{\[Mu]},{\[Nu]}],
           Adjoint[CDr[MM,{\[Mu]}]],CDr[MM,{\[Nu]}]] ]+

L10[0]*
UTrace[ NM[FST[LeftComponent[0],{\[Mu]},{\[Nu]}],MM,
           FST[RightComponent[0],{\[Mu]},{\[Nu]}],Adjoint[MM]] ]+

H1[0]*
UTrace[ NM[FST[LeftComponent[0],{\[Mu]},{\[Nu]}],FST[LeftComponent[0],{\[Mu]},{\[Nu]}]]+
        NM[FST[RightComponent[0],{\[Mu]},{\[Nu]}],FST[RightComponent[0],{\[Mu]},{\[Nu]}]] ]+

H2[0]*
UTrace[ NM[Adjoint[UChiMatrix],UChiMatrix] ];

(* ---------------------------------------------------------------- *)

$Lagrangians=Union[$Lagrangians,{ChPT3[4]}];

FieldsSet[ChPT3[4]]:=
{IsoVector[
QuantumField[Particle[PhiMeson,RenormalizationState[0]]]
]};
