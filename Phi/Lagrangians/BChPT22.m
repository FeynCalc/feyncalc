(* ****************************************************************** *)
(*                                                                    *)
(*                      BChPT22                                       *)
(*                                                                    *)
(* ****************************************************************** *)

(*
   Author:              F.Orellana 1998

   Mathematica Version: 3.0

   Requirements:        FeynCalc > 3, Phi

   Summary:             Lagrangian for Phi

   Description:         The simplest ChPT lagrangian.

                        Taken from J. Gasser, M. E. Sainio and
                        A. Svarc (1988), Nucl. Phys, B307, 779-853
*)


Begin["HighEnergyPhysics`Phi`Objects`"];

(* ------------------------------------------------------------------ *)

BChPT22::"usage"=
"BChPT22.m is the name of the file containing the definitions for
Lagrangian[BChPT2[2]], which is the SU(2)  ChPT pion-nucleon
lagrangian.  To evaluate use ArgumentsSupply";

GAV::"usage"=
"GAV := CouplingConstant[BChPT2[2]] is axial vector
coupling constant";

(* ------------------------------------------------------------------ *)

End[];

(* ------------------------------------------------------------------ *)

GAV = CouplingConstant[BChPT2[2]];

(* ------------------------------------------------------------------ *)

(* Box definitions *)

CouplingConstant /:
  MakeBoxes[
CouplingConstant[BChPT2[2],st___RenormalizationState,
      sc___RenormalizationScheme,qs___QuarkMassExpansionState],
    TraditionalForm]:=
  SubsuperscriptBox[MakeBoxes[StyleForm["g",FontSlant->"Italic"]][[1]],
    MakeBoxes["A"],
    RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
          MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
          MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* ------------------------------------------------------------------ *)

Lagrangian[BChPT2[2]]:=

I*(UVector[DiracBar[QuantumField[Particle[Nucleon]]]].
DiracMatrix[LorentzIndex[\[Mu]]].
CNDr[UVector[QuantumField[Particle[Nucleon]]],{\[Mu]}])-


ParticleMass[Nucleon]*
UVector[DiracBar[QuantumField[Particle[Nucleon]]]].
UVector[QuantumField[Particle[Nucleon]]]+


I*CouplingConstant[BChPT2[2],RenormalizationState[0]]*
(UVector[DiracBar[QuantumField[Particle[Nucleon]]]].
NM[DiracMatrix[LorentzIndex[\[Mu]]],DiracMatrix[5],GasserDelta[\[Mu]]].
UVector[QuantumField[Particle[Nucleon]]]);

(* ------------------------------------------------------------------ *)

FieldsSet[BChPT2[2]]:=
{IsoVector[
QuantumField[Particle[Pion,RenormalizationState[0]]]],
QuantumField[Particle[Nucleon,RenormalizationState[0]]]};

$Lagrangians=Union[$Lagrangians,{BChPT2[2]}];
