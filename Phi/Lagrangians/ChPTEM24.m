(* ************************************************************** *)
(*                                                                *)
(*                       ChPTEM24                                 *)
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
    
                        Taken from U. Meissner, G. Müller, 
                        S. Steininger, hep-ph/9704377
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* -------------------------------------------------------------- *)

ChPTEM24::"usage"=
"ChPTEM24.m is the name of the file containing the definitions for
Lagrangian[ChPTEM2[4]], which is the pionic ChPT lagrangian
including virtual photons to fourth order in the energy.
To evaluate use ArgumentsSupply.";

K1::"usage"=
"K1 := CouplingConstant[ChPTEM2[4],1] is one of the constants of the
fourth order ChPT lagrangian.";

K2::"usage"=
"K2 := CouplingConstant[ChPTEM2[4],2] is one of the constants of the
fourth order ChPT lagrangian.";

K3::"usage"=
"K3 := CouplingConstant[ChPTEM2[4],3] is one of the constants of the
fourth order ChPT lagrangian.";

K4::"usage"=
"K4 := CouplingConstant[ChPTEM2[4],4] is one of the constants of the
fourth order ChPT lagrangian.";

K5::"usage"=
"K5 := CouplingConstant[ChPTEM2[4],5] is one of the constants of the
fourth order ChPT lagrangian.";

K6::"usage"=
"K6 := CouplingConstant[ChPTEM2[4],6] is one of the constants of the
fourth order ChPT lagrangian.";

K7::"usage"=
"K7 := CouplingConstant[ChPTEM2[4],7] is one of the constants of the
fourth order ChPT lagrangian.";

K8::"usage"=
"K8 := CouplingConstant[ChPTEM2[4],8] is one of the constants of the
fourth order ChPT lagrangian.";

K9::"usage"=
"K9 := CouplingConstant[ChPTEM2[4],9] is one of the constants of the
fourth order ChPT lagrangian.";

K10::"usage"=
"K10 := CouplingConstant[ChPTEM2[4],10] is one of the constants of the
fourth order ChPT lagrangian.";

K11::"usage"=
"K11 := CouplingConstant[ChPTEM2[4],11] is one of the constants of the
fourth order ChPT lagrangian.";

K12::"usage"=
"K12 := CouplingConstant[ChPTEM2[4],12] is one of the constants of the
fourth order ChPT lagrangian.";

K13::"usage"=
"K13 := CouplingConstant[ChPTEM2[4],13] is one of the constants of the
fourth order ChPT lagrangian.";

K14::"usage"=
"K14 := CouplingConstant[ChPTEM2[4],14] is one of the constants of the
fourth order ChPT lagrangian.";

K15::"usage"=
"K15 := CouplingConstant[ChPTEM2[4],15] is one of the constants of the
fourth order ChPT lagrangian.";


(* ---------------------------------------------------------------- *)

End[];


(* ---------------------------------------------------------------- *)

(* Abbreviations *)

K1 = CouplingConstant[ChPTEM2[4],1];
K2 = CouplingConstant[ChPTEM2[4],2];
K3 = CouplingConstant[ChPTEM2[4],3];
K4 = CouplingConstant[ChPTEM2[4],4];
K5 = CouplingConstant[ChPTEM2[4],5];
K6 = CouplingConstant[ChPTEM2[4],6];
K7 = CouplingConstant[ChPTEM2[4],7];
K8 = CouplingConstant[ChPTEM2[4],8];
K9 = CouplingConstant[ChPTEM2[4],9];
K10= CouplingConstant[ChPTEM2[4],10];
K11 = CouplingConstant[ChPTEM2[4],11];
K12 = CouplingConstant[ChPTEM2[4],12];
K13 = CouplingConstant[ChPTEM2[4],13];
K14 = CouplingConstant[ChPTEM2[4],14];
K15 = CouplingConstant[ChPTEM2[4],15];

(* ---------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[ChPTEM2[4],i_,st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["k",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[i]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ---------------------------------------------------------------- *)

RenormalizationCoefficients[ChPTEM2[4]] =
{3/2+3*z+12*z^2, 2*z, -3/4, -2*z, -1/4, 0, 1/4+2*z,
1/8(*-z*)(*I think the -z is a bug?!?*)(*Yes, it's not in the published version*),
-3-3*z/5-12*z^2/5, -27/20-z/5, -1/4-z/5, 0, 3/2-12*z/5+84*z^2/25, 0, 1/30} /.
z -> CouplingConstant[ChPTEM2[2],RenormalizationState[0]]/
DecayConstant[Pion,RenormalizationState[0]]^4;

(* ---------------------------------------------------------------- *)

Lagrangian[ChPTEM2[4]]:=

K1[0]*
DecayConstant[Pion]^4*
NMPower[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]]],2]+

K2[0]*
DecayConstant[Pion]^2*
NM[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]]],
UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ]]+

K3[0]*
DecayConstant[Pion]^2*
(NMPower[UTrace[ NM[Adjoint[MM],CDr[MM,{\[Mu]}],UMatrix[UChiralSpurion]] ],2] +
 NMPower[UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[MM],UMatrix[UChiralSpurion]] ],2])+

K4[0]*
DecayConstant[Pion]^2*
NM[UTrace[ NM[Adjoint[MM],CDr[MM,{\[Mu]}],UMatrix[UChiralSpurion]] ],
UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[MM],UMatrix[UChiralSpurion]] ]]+

K5[0]*
DecayConstant[Pion]^2*
UTrace[NM[(NM[UMatrix[UChiralSpurion], CDr[UMatrix[UChiralSpurion],{\[Mu]}]]-
           NM[CDr[UMatrix[UChiralSpurion],{\[Mu]}], UMatrix[UChiralSpurion]]),
           Adjoint[MM], CDr[MM,{\[Mu]}]] -
        
       NM[(NM[UMatrix[UChiralSpurion], CDr[UMatrix[UChiralSpurion],{\[Mu]}]]-
           NM[CDr[UMatrix[UChiralSpurion],{\[Mu]}], UMatrix[UChiralSpurion]]),
           CDr[MM,{\[Mu]}], Adjoint[MM]] ]+

K6[0]*
DecayConstant[Pion]^2*
UTrace[NM[CDr[UMatrix[UChiralSpurion],{\[Mu]}], MM,
          CDr[UMatrix[UChiralSpurion],{\[Mu]}], Adjoint[MM]] ]+

K7[0]*
DecayConstant[Pion]^2*
NM[UTrace[ NM[UMatrix[UChiralSpurion],MM,
           UMatrix[UChiralSpurion],Adjoint[MM]] ],
UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[Adjoint[UChiMatrix],MM] ]]+

K8[0]*
DecayConstant[Pion]^2*
UTrace[ NM[NM[Adjoint[MM],UChiMatrix]-NM[Adjoint[UChiMatrix],MM],
            NM[Adjoint[MM],UMatrix[UChiralSpurion],MM,UMatrix[UChiralSpurion]]-
            NM[UMatrix[UChiralSpurion],Adjoint[MM],UMatrix[UChiralSpurion],MM]] ]+

K9[0]*
DecayConstant[Pion]^4*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[UMatrix[UChiralSpurion],MM,UMatrix[UChiralSpurion],Adjoint[MM]] ]]+

K10[0]*
DecayConstant[Pion]^2*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ]]+

K11[0]*
DecayConstant[Pion]^2*
NM[UTrace[ NMPower[UMatrix[UChiralSpurion], 2] ],
UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]]+

K12[0]*
DecayConstant[Pion]^2*
UTrace[ NM[CDr[UMatrix[UChiralSpurionRight], {\[Mu]}],
           CDr[UMatrix[UChiralSpurionRight], {\[Mu]}] ] +
        NM[CDr[UMatrix[UChiralSpurionLeft], {\[Mu]}],
           CDr[UMatrix[UChiralSpurionLeft], {\[Mu]}] ] ]+

K13[0]*
DecayConstant[Pion]^4*
NMPower[UTrace[ NM[UMatrix[UChiralSpurion],UMatrix[UChiralSpurion]] ],2]+

K14[0]*
DecayConstant[Pion]^2*
NM[
UTrace[ NM[NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]],UMatrix[UChiralSpurion]] +
        NM[NM[Adjoint[UChiMatrix],MM]+NM[Adjoint[MM],UChiMatrix],UMatrix[UChiralSpurion]] ],
UTrace[UMatrix[UChiralSpurion] ]
]+

K15[0]*
UTrace[NMPower[UMatrix[UChiralSpurion],2]]*
NM[FieldStrengthTensor[{\[Mu]}, QuantumField[Particle[Photon],{\[Nu]}]],
FieldStrengthTensor[{\[Mu]}, QuantumField[Particle[Photon],{\[Nu]}]]];


(* ---------------------------------------------------------------- *)

$Lagrangians=Union[$Lagrangians,{ChPTEM2[4]}];

FieldsSet[ChPTEM2[4]]:=
{IsoVector[QuantumField[Particle[Pion,RenormalizationState[0]]]],
QuantumField[Particle[Photon,RenormalizationState[0]]]};

