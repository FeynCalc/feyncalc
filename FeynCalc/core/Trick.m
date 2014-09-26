(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Trick does non-commutative expansion and simple contractions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Trick`",{"HighEnergyPhysics`FeynCalc`"}];

Trick::"usage" =
"Trick[exp] uses Contract, DotSimplify and SUNDeltaContract.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
EpsContract = MakeContext["CoreOptions","EpsContract"];
Expanding = MakeContext["CoreOptions","Expanding"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
PauliSigma = MakeContext["CoreObjects","PauliSigma"];
SUNDelta = MakeContext["CoreObjects","SUNDelta"];
SUNF = MakeContext["CoreObjects","SUNF"];
SUNIndex = MakeContext["CoreObjects","SUNIndex"];
SUNT = MakeContext["CoreObjects","SUNT"];

MakeContext[
DotSimplify,
FeynAmpDenominatorCombine,
FeynCalcInternal,
SUNDeltaContract, SUNSimplify,
CovariantD,
CrossProduct,
DotProduct,
Explicit,
Contract];

Trick[x_] := Block[{tt, paulisigsimp, sigident,doot,cov,palr},
             SetAttributes[cov,HoldFirst];
             cov[y_] := If[CheckContext["CovariantD"],
                           y /. CovariantD[a__] :>
                           CovariantD[a, Explicit -> True],
                           y
                          ];
             SetAttributes[palr,HoldFirst];
             tt = DotSimplify[FeynCalcInternal[x]//cov(*//palr*),
                              Expanding -> False
                             ] /. SUNDelta -> SUNDeltaContract /.
                                  SUNDeltaContract -> SUNDelta;
             If[!FreeQ[tt, LorentzIndex],
                tt = Contract[tt, EpsContract -> False,
                                  Expanding -> False]];
             If[!FreeQ[tt, SUNT],
                tt = (tt /. DOT -> doot) //.
                {doot[a___,b_ /; FreeQ[b,SUNT], c__SUNT, d___] :>
                 doot[a,c,b,d]} /.
                 {doot[a__SUNT, b__] :>
                 (doot[a] doot[b]) /; FreeQ[{b},SUNIndex]} /. doot -> DOT
               ];
             If[!FreeQ[tt, SUNF],
                tt = tt /. ( SUNF[a_,b_,c_] SUNF[d_,e_,f_] :>
                             SUNSimplify[SUNF[a,b,c] SUNF[d,e,f]] ) /.
                     SUNDelta->SUNDeltaContract /. SUNDeltaContract->SUNDelta
               ];

             If[CheckContext["CoreObjects"],
                paulisigsimp[y_] := FixedPoint[sigident, y, 1442];
                sigident[z_] := DotSimplify[(z /. DOT -> doot //.
                {doot[w1___, DotProduct[PauliSigma, a_],
                             DotProduct[PauliSigma, b_], w2___
                     ] :> (doot[w1, DotProduct[a, b], w2] +
                           I doot[w1, DotProduct[PauliSigma,
                                            CrossProduct[a, b]], w2
                                 ]
                          )
                } /. doot -> DOT), Expanding -> False];
                tt = paulisigsimp[tt]
               ];
              If[CheckContext["CoreObjects"],
                 tt = FeynAmpDenominatorCombine[tt];
                ];
                tt];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Trick | \n "]];
Null
