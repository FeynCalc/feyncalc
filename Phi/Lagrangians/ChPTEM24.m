(* ************************************************************** *)
(*                                                                *)
(*                       ChPTEM24                                 *)
(*                                                                *)
(* ************************************************************** *)

(*
   Author:              Frederik Orellana 2001

   Mathematica Version: 4.0

   Requirements:        Feyncalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The next to leading order ChPT lagrangian
                        with electromagnetic couplings.
    
                        Taken from U. Meissner, G. Müller, 
                        S. Steininger, hep-ph/9704377
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTEM24::"usage"=
"ChPTEM24.m is the name of the file containing the definitions for
ULagrangian[ChPTEM2[4]], which is the pionic ChPT lagrangian
including virtual photons to fourth order in the energy.
To evaluate use ULagrangian";

k1::"usage"=
"k1 := UCouplingConstant[ChPTEM2[4],1] is one of the constants of the
fourth order ChPT lagrangian";

k2::"usage"=
"k2 := UCouplingConstant[ChPTEM2[4],2] is one of the constants of the
fourth order ChPT lagrangian";

k3::"usage"=
"k3 := UCouplingConstant[ChPTEM2[4],3] is one of the constants of the
fourth order ChPT lagrangian";

k4::"usage"=
"k4 := UCouplingConstant[ChPTEM2[4],4] is one of the constants of the
fourth order ChPT lagrangian";

k5::"usage"=
"k5 := UCouplingConstant[ChPTEM2[4],5] is one of the constants of the
fourth order ChPT lagrangian";

k6::"usage"=
"k6 := UCouplingConstant[ChPTEM2[4],6] is one of the constants of the
fourth order ChPT lagrangian";

k7::"usage"=
"k7 := UCouplingConstant[ChPTEM2[4],7] is one of the constants of the
fourth order ChPT lagrangian";

k8::"usage"=
"k8 := UCouplingConstant[ChPTEM2[4],8] is one of the constants of the
fourth order ChPT lagrangian";

k9::"usage"=
"k9 := UCouplingConstant[ChPTEM2[4],9] is one of the constants of the
fourth order ChPT lagrangian";

k10::"usage"=
"k10 := UCouplingConstant[ChPTEM2[4],10] is one of the constants of the
fourth order ChPT lagrangian";

k11::"usage"=
"k11 := UCouplingConstant[ChPTEM2[4],11] is one of the constants of the
fourth order ChPT lagrangian";

k12::"usage"=
"k12 := UCouplingConstant[ChPTEM2[4],12] is one of the constants of the
fourth order ChPT lagrangian";

k13::"usage"=
"k13 := UCouplingConstant[ChPTEM2[4],13] is one of the constants of the
fourth order ChPT lagrangian";

(* ---------------------------------------------------------------- *)

Begin["`Private`"];

(* ---------------------------------------------------------------- *)

(* Abbreviations *)

fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;

K1 := UCouplingConstant[ChPTEM2[4],1];
K2 := UCouplingConstant[ChPTEM2[4],2];
K3 := UCouplingConstant[ChPTEM2[4],3];
K4 := UCouplingConstant[ChPTEM2[4],4];
K5 := UCouplingConstant[ChPTEM2[4],5];
K6 := UCouplingConstant[ChPTEM2[4],6];
K7 := UCouplingConstant[ChPTEM2[4],7];
K8 := UCouplingConstant[ChPTEM2[4],8];
K9 := UCouplingConstant[ChPTEM2[4],9];
K10:= UCouplingConstant[ChPTEM2[4],10];
K11 := UCouplingConstant[ChPTEM2[4],11];
K12 := UCouplingConstant[ChPTEM2[4],12];
K13 := UCouplingConstant[ChPTEM2[4],13];
(*K14 := UCouplingConstant[ChPTEM2[4],14];*)

(* ---------------------------------------------------------------- *)

(* Box definitions *)

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[ChPTEM2[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["k",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTEM2[4]]:=
{3/2+3*z+12*z^2, 2*z, -3/4, -2*z, -1/4, 0, 1/4+2*z, 1/8(*-z*)(*I think the -z is a bug?!?*),
-3-3*z/5-12*z^2/5, -27/20-z/5, -1/4-z/5, 0, 3/2-12*z/5+84*z^2/25(*,
Global`x*)} /.
z -> UCouplingConstant[ChPTEM2[2],RenormalizationState[0]]/
DecayConstant[Pion,RenormalizationState[0]]^4;

(* ---------------------------------------------------------------- *)

mu=(Global`\[Mu]);
nu=(Global`\[Nu]);

(* ---------------------------------------------------------------- *)

ULagrangian[ChPTEM2[4]]:=

K1[0]*
NMPower[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]]],2]+

K2[0]*
NM[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]]],
UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]] ]]+

K3[0]*
(NMPower[UTrace[ NM[Adjoint[MM],CDr[MM,{mu}],UMatrix[UChiralSpurion]] ],2] +
 NMPower[UTrace[ NM[CDr[MM,{mu}],Adjoint[MM],UMatrix[UChiralSpurion]] ],2])+

K4[0]*
NM[UTrace[ NM[Adjoint[MM],CDr[MM,{mu}],UMatrix[UChiralSpurion]] ],
UTrace[ NM[CDr[MM,{mu}],Adjoint[MM],UMatrix[UChiralSpurion]] ]]+

K5[0]*
UTrace[NM[(NM[UMatrix[UChiralSpurion], CDr[UMatrix[UChiralSpurion],{mu}]]-
           NM[CDr[UMatrix[UChiralSpurion],{mu}], UMatrix[UChiralSpurion]]),
           Adjoint[MM], CDr[MM,{mu}]] -
        
       NM[(NM[UMatrix[UChiralSpurion], CDr[UMatrix[UChiralSpurion],{mu}]]-
           NM[CDr[UMatrix[UChiralSpurion],{mu}], UMatrix[UChiralSpurion]]),
           CDr[MM,{mu}], Adjoint[MM]] ]+

K6[0]*
UTrace[NM[CDr[UMatrix[UChiralSpurion],{mu}], MM,
          CDr[UMatrix[UChiralSpurion],{mu}], Adjoint[MM]] ]+

K7[0]*
NM[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]] ],
UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[Adjoint[UChiMatrix],MM] ]]+

K8[0]*
UTrace[ NM[NM[Adjoint[MM],UChiMatrix]-NM[Adjoint[UChiMatrix],MM],
            NM[Adjoint[MM],UMatrix[UChiralSpurion],MM,UMatrix[UChiralSpurion]]-
            NM[UMatrix[UChiralSpurion],Adjoint[MM],UMatrix[UChiralSpurion],MM]] ]+

K9[0]*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[UMatrix[UChiralSpurion],MM,UMatrix[UChiralSpurion],Adjoint[MM]] ]]+

K10[0]*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[CDr[MM,{mu}],Adjoint[CDr[MM,{mu}]]] ]]+

K11[0]*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]]+

K12[0]*
UTrace[ NM[CDr[UMatrix[UChiralSpurionRight], {mu}],
           CDr[UMatrix[UChiralSpurionRight], {mu}] ] +
        NM[CDr[UMatrix[UChiralSpurionLeft], {mu}],
           CDr[UMatrix[UChiralSpurionLeft], {mu}] ] ]+

K13[0]*
NMPower[UTrace[ NM[UMatrix[UChiralSpurion],UMatrix[UChiralSpurion]] ],2](*+

K14[0]*
NM[
UTrace[ NM[NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]],UMatrix[UChiralSpurion]] +
        NM[NM[Adjoint[UChiMatrix],MM]+NM[Adjoint[MM],UChiMatrix],UMatrix[UChiralSpurion]] ],
UTrace[UMatrix[UChiralSpurion] ]
]*);
(*The last term is in the Kubis&Meissner paper from 1999*)

(* ---------------------------------------------------------------- *)

$ULagrangians=Union[$ULagrangians,{ChPTEM2[4]}];

FieldsSet[ChPTEM2[4]]:=
{IsoVector[
fcqf[Particle[Pion,RenormalizationState[0]]]
]};

End[];

End[];
