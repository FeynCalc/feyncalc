(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonSelfEnergy *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonSelfEnergy *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonSelfEnergy`",
             "HighEnergyPhysics`FeynCalc`"];

GluonSelfEnergy::"usage" = 
"GluonSelfEnergy[{mu, a}, {nu,b}] yields the 1-loop Gluon selfenergy."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CA,
CouplingConstant,
Dimension,
Epsilon,
FinalSubstitutions,
Gauge,
Gstrong,
LorentzIndex,
Momentum,
Pair,
ScaleMu,
SUNDelta,
SUNIndex,
Tf
   ];

Options[GluonSelfEnergy] = {Dimension -> D, CouplingConstant -> Gstrong,
                            FinalSubstitutions ->  
                            {Log[ScaleMu^2 _] :> 0,
                             EulerGamma :> Log[4 Pi]
                            },
                             Gauge -> 1,
                            Momentum ->  Global`p
                           };

GluonSelfEnergy[{mu_,a_},{nu_,b_},opt___Rule] := Block[
 {coup, pe, pe2, zdeta, fin , dim},
coup = CouplingConstant /. {opt} /. Options[GluonSelfEnergy];
alpha= Gauge /. {opt} /. Options[GluonSelfEnergy];
pe   = Momentum /.  {opt} /. Options[GluonSelfEnergy];
pe2  = Pair[Momentum[pe], Momentum[pe]];
dim  = Dimension /. {opt} /. Options[GluonSelfEnergy];
fin  = FinalSubstitutions /. {opt} /. Options[GluonSelfEnergy];
zdeta = 2/Epsilon + EulerGamma - Log[4 Pi];
(* gluon + ghost - loop *)
(
 I coup^2 CA/2 SUNDelta[SUNIndex[a], SUNIndex[b]] * 
  (
   Pair[Momentum[pe, dim], LorentzIndex[mu, dim]]  *
   Pair[Momentum[pe, dim], LorentzIndex[nu, dim]] - 
    Pair[LorentzIndex[mu, dim], LorentzIndex[nu, dim]] *
     Pair[Momentum[pe, dim], Momentum[pe, dim]]
  ) ( zdeta (10/3 + (1-alpha)) - 62/9 - 
      10/3 Log[ScaleMu^2/pe2] +
      (1-alpha) ( 2 - Log[ScaleMu^2/pe2]) - 1/2 (1-alpha)^2
    ) +
  I coup^2 Tf SUNDelta[SUNIndex[a], SUNIndex[b]] *
  (
   Pair[Momentum[pe, dim], LorentzIndex[mu, dim]] *
    Pair[Momentum[pe, dim], LorentzIndex[nu, dim]] -
    Pair[LorentzIndex[mu, dim], LorentzIndex[nu, dim]] *
     Pair[Momentum[pe, dim], Momentum[pe, dim]]
  ) ( -4/3 zdeta + 20/9 + 4/3 Log[ScaleMu^2/pe2])
) /. fin
                                                     ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonSelfEnergy | \n "]];
Null
