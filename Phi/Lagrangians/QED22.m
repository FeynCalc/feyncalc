(* *************************************************************** *)
(*                                                                 *)
(*                      QED22                                      *)
(*                                                                 *)
(* *************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         Counterterm QED lagrangian
                        for three leptons.

                        Taken from S .Weinberg (1995),
                        "The Quantum Theory of Fields",
                        Cambridge University Press

                        Adapted to the usual space-
                        time metric (1,-1,-1,-1)
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* --------------------------------------------------------------- *)

QED22::"usage"=
"QED22.m is the name of the file containing the definitions for
Lagrangian[QED2[2]], which is the QED counterterm lagrangian";

DM::"usage"=
"DM := CouplingConstant[QED2[2],1] is one of the constants of the
counterterm QED lagrangian - the mass counterterm";

Z2::"usage"=
"Z2 := CouplingConstant[QED2[2],2] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical Lepton field";

Z3::"usage"=
"Z3 := CouplingConstant[QED2[2],3] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical photon field";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
   QED[2],1,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
    SuperscriptBox[
    MakeBoxes[StyleForm["\[Delta]m",FontSlant->"Italic"]][[1]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
   QED[2],2,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[2]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
   QED[2],3,st___RenormalizationState,
      sc___RenormalizationScheme,qs___ExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
    MakeBoxes[TraditionalForm[3]],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

(* Abbreviations *)

DM = CouplingConstant[QED[2],1];
Z2 = CouplingConstant[QED[2],2];
Z3 = CouplingConstant[QED[2],3];

(* --------------------------------------------------------------- *)

Lagrangian[QED2[2]]:=


-1/4*(Z3[0]-1)*
FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]].
FieldStrengthTensor[LorentzIndex[\[Mu]],
QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]+

(Z2[0]-1)*
(DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]].
DiracMatrix[LorentzIndex[\[Mu]]].
(I*QuantumField[PartialD[LorentzIndex[\[Mu]]],Particle[Lepton],SUNIndex[i]]+
CouplingConstant[QED[1]]*
QuantumField[Particle[Photon],LorentzIndex[\[Mu]]].
QuantumField[Particle[Lepton],SUNIndex[i]])-

ParticleMass[Lepton,SUNIndex[i]]*
DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]].
QuantumField[Particle[Lepton],SUNIndex[i]])-

Z2[0]*DM[0]*
DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]].
QuantumField[Particle[Lepton],SUNIndex[i]];

(* --------------------------------------------------------------- *)

FieldsSet[QED2[2]]:=
{QuantumField[Particle[Lepton,RenormalizationState[0]]],
QuantumField[Particle[Photon,RenormalizationState[0]],LorentzIndex[\[Mu]]]};

$Lagrangians=Union[$Lagrangians,{QED2[2]}];
