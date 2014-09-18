(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Propagators *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FeynAmpDenominator`",{"HighEnergyPhysics`FeynCalc`"}];

FeynAmpDenominator::"usage" =
"FeynAmpDenominator[ PropagatorDenominator[ ... ],
PropagatorDenominator[ ... ], ... ] represents
the inverse denominators of the propagators, i.e. FeynAmpDenominator[x]
is 1/x .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FAD, FeynCalcInternal, Momentum];

FeynAmpDenominator[ar__List] := FeynAmpDenominator[ar] =
FeynCalcInternal[FAD[ar]];

    MakeBoxes[f_. FeynAmpDenominator[a__], TraditionalForm
             ] := (MakeBoxes[#,TraditionalForm]&)@@{f/ Apply[DOT,
                   Map[( (#[[1]]/.Momentum[aa_,___]:>aa)^2 -
                          #[[2]]^2 )&, {a}
                      ]
                                  ]}

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpDenominator | \n "]];
Null
