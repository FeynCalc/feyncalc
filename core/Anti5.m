(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Anti5 anticommutes gamma5's right or left *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Anti5`",
             "HighEnergyPhysics`FeynCalc`"];

Anti5::"usage" =
"Anti5[exp] anticommutes all gamma5 one time to the right. \
Anti5[exp, n] anticommutes all gamma5 n times to the right. \
Anti5[exp, -n] anticommutes all gamma5 n times to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DiracGamma, FeynCalcInternal, MemSet];

Anti5[a_/;FreeQ[a, DiracGamma[5]],_] := a;
Anti5[x_, Infinity] := FixedPoint[Anti5, x, $RecursionLimit];
Anti5[xx_,n_Integer?Positive] := Nest[Anti5, xx, n];

Anti5[xx_] := (FeynCalcInternal[xx] /. DOT -> doot) /.
            If[$BreitMaison =!= True,
             {doot[a___, DiracGamma[5], DiracGamma[y_[x__], di___], b___] :>
              (-doot[a,DiracGamma[y[x],di],DiracGamma[5],b])
             },
             {doot[a___, DiracGamma[5], DiracGamma[y_[x_]], b___] :>
              (-doot[a,DiracGamma[y[x]],DiracGamma[5],b])
              ,
              doot[a___, DiracGamma[5], DiracGamma[y_[x_,di_Symbol],
                                                           di_Symbol
                                                    ],
                     b___
                    ] :>
              (-doot[a,DiracGamma[y[x], di], DiracGamma[5], b] +
                2 doot[a,DiracGamma[y[x,di-4],di-4],DiracGamma[5],b]
              )
             }
                ] /.doot[a___, DiracGamma[5], DiracGamma[5],b___
                        ] :> doot[a,b] /. doot -> DOT;

Anti5[xx_,-1] :=
           (FeynCalcInternal[xx] /. DOT -> doot) /.
             {doot[a___, DiracGamma[y_[x__], di___],
                         DiracGamma[5],
                  b___] :>
              (-doot[a, DiracGamma[5], DiracGamma[y[x],di], b])
             } /. doot -> DOT

Anti5[xx_,n_Integer?Negative] := Nest[Anti5[#,-1]&, xx, -n] /; n <(-1);
Anti5[x_, -Infinity] := Anti5[x, -$RecursionLimit];
Anti5[x_, -Infinity] := FixedPoint[Anti5[#,-1]&, x,
                                   $RecursionLimit];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Anti5 | \n "]];
Null
