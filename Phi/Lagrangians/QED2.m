(* *************************************************************** *)
(*                                                                 *)
(*                      QED2                                       *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         Counterterm QED lagrangian
                        for the electron.

                        Taken from S .Weinberg (1995),
                        "The Quantum Theory of Fields",
                        Cambridge University Press

                        Adapted to the usual space-
                        time metric (1,-1,-1,-1)
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* --------------------------------------------------------------- *)

QED2::"usage"=
"QED2.m is the name of the file containing the definitions for
ULagrangian[QED[2]], which is the QED counterterm lagrangian";

DM::"usage"=
"DM := UCouplingConstant[QED[2],1] is one of the constants of the
counterterm QED lagrangian - the mass counterterm";

Z2::"usage"=
"Z2 := UCouplingConstant[QED[2],2] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical electron field";

Z3::"usage"=
"Z3 := UCouplingConstant[QED[2],3] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical photon field";

(* --------------------------------------------------------------- *)

Begin["`Private`"];

(* --------------------------------------------------------------- *)

(* Box definitions *)

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[QED[2],1,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
    SuperscriptBox[
    MakeBoxes[StyleForm["\[Delta]m",FontSlant->"Italic"]][[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[QED[2],2,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

UCouplingConstant/:
  MakeBoxes[
    UCouplingConstant[QED[2],3,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[3]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

mu=(Global`\[Mu]);nu=(Global`\[Nu]);
fcqf:=HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcpd:=HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli:=HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcdm:=HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix;

DM := UCouplingConstant[QED[2],1];
Z2 := UCouplingConstant[QED[2],2];
Z3 := UCouplingConstant[QED[2],3];

(* --------------------------------------------------------------- *)

ULagrangian[QED[2]]:=


-1/4*(Z3[0]-1)*
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]].
FieldStrengthTensor[fcli[mu],
fcqf[Particle[Photon],fcli[nu]]]+

(Z2[0]-1)*
(DiracBar[fcqf[Particle[Electron]]].
fcdm[fcli[mu]].
(I*fcqf[fcpd[fcli[mu]],Particle[Electron]]+
UCouplingConstant[QED[1]]*
fcqf[Particle[Photon],fcli[mu]].
fcqf[Particle[Electron]])-

ParticleMass[Electron]*
DiracBar[fcqf[Particle[Electron]]].
fcqf[Particle[Electron]])-

Z2[0]*DM[0]*
DiracBar[fcqf[Particle[Electron]]].
fcqf[Particle[Electron]];

(* --------------------------------------------------------------- *)

FieldsSet[QED[2]]:=
{fcqf[Particle[Electron,RenormalizationState[0]]],
fcqf[Particle[Photon,RenormalizationState[0]],fcli[mu]]};

$ULagrangians=Union[$ULagrangians,{QED[2]}];

End[];

End[];
