(* ****************************************************************** *)
(*                                                                    *)
(*                      HBChPT22                                      *)
(*                                                                    *)
(* ****************************************************************** *)

(*
   Author:              F.Orellana

   Year:                1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, PHI

   Summary:             Lagrangian for PHI

   Description:         The simplest ChPT lagrangian.

                        Taken from G.Ecker and M. Mojzis (1995),
                        hep-ph/9508204
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* ------------------------------------------------------------------ *)

HBChPT22::"usage"=
"HBChPT22.m is the name of the file containing the definitions for
Lagrangian[HBChPT2[2]], which is the SU(2)  ChPT pion-nucleon
lagrangian.  To evaluate use ArgumentsSupply.";

GAV::"usage"=
"GAV := CouplingConstant[HBChPT2[2]] is axial vector
coupling constant.";

(* ------------------------------------------------------------------ *)

End[];

(* ------------------------------------------------------------------ *)

CouplingConstant/:
  MakeBoxes[
    CouplingConstant[
   HBChPT2[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["g",FontSlant->"Italic"]][[1]],
    MakeBoxes["A"],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ------------------------------------------------------------------ *)

GAV = CouplingConstant[HBChPT2[2]];

(* ------------------------------------------------------------------ *)

Lagrangian[HBChPT2[2]]:=

I*(UDot[UVector[
DiracBar[QuantumField[Particle[Nucleon]]]],
DiracMatrix[LorentzIndex[\[Mu]]],
CDr[UVector[QuantumField[Particle[Nucleon]]],{\[Mu]}]])-


ParticleMass[Nucleon]*
UDot[UVector[DiracBar[QuantumField[Particle[Nucleon]]]],
UVector[QuantumField[Particle[Nucleon]]]]+


CouplingConstant[
HBChPT2[2],RenormalizationState[0]]/2*
(UDot[UVector[DiracBar[QuantumField[Particle[Nucleon]]]],
DiracMatrix[LorentzIndex[\[Mu]]],USmall[\[Mu]],
UVector[QuantumField[Particle[Nucleon]]]]);

(* ------------------------------------------------------------------ *)

FieldsSet[HBChPT2[2]]:=
{IsoVector[
QuantumField[Particle[Pion,RenormalizationState[0]]]],
UVector[QuantumField[Particle[Nucleon,RenormalizationState[0]]]]};

$Lagrangians=Union[$Lagrangians,{HBChPT2[2]}];
