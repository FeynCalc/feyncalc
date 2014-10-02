(* ------------------------------------------------------------------------ *)

(* :Summary: for scalar products *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ScalarProduct`",{"HighEnergyPhysics`FeynCalc`"}];

ScalarProduct::"usage" =
"ScalarProduct[p, q] is the input for scalar product.
ScalarProduct[p] is equivalent to ScalarProduct[p, p].
Expansion of sums of momenta in ScalarProduct is done with
ExpandScalarProduct. Scalar products may be set, e.g.
ScalarProduct[a, b] = m^2; but a and b may not contain sums.
Note that ScalarProduct[a, b] = m^2 actually sets also:
Pair[Momentum[a, ___], Momentum[b, ___]] = m^2 and
SPD[a,b] = m^2 and SP[a,b]=m^2.
It is enouraged to always set ScalarProduct's BEFORE any
calculation. This improves the performance of FeynCalc .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
SP = MakeContext["CoreObjects","SP"];
SPD = MakeContext["CoreObjects","SPD"];
fci := fci = MakeContext["FeynCalcInternal"];
nf  := nf = MakeContext["NumericalFactor"];

MakeContext[ SelectFree, ChangeDimension];


Options[ScalarProduct] = {Dimension->4, fci -> True};

ScalarProduct[a_, b_, c___] := ScalarProduct[b, a, c] /;
                               !OrderedQ[{a, b}];

ScalarProduct[x_, y___Rule] := ScalarProduct[x, x, y];

ScalarProduct[a_,b_, c___Rule] :=
 Pair[Momentum[a, Dimension /. {c} /. Options[ScalarProduct]],
      Momentum[b, Dimension /. {c} /. Options[ScalarProduct]]
     ] /; FreeQ[{a,b}, Momentum] &&
          ((fci /. {c} /. Options[ScalarProduct]) === True);

ScalarProduct/:Set[ScalarProduct[a_,b_,c___],z_]:= Block[
{ste, rst, downv, scal},
If[FreeQ[a, Pattern], ste = fci[ScalarProduct[a, b, c]];
   If[ste === 0 , rst = 0,
      ste = ChangeDimension[ste, ___Symbol];
      If[(Head[a] === Pattern) && (a === b),
         (SetDelayed @@ {ste, ScalarProduct[a[[1]], a[[1]]]})
         ,
         Set@@{ste/nf[ste], z / nf[ste]};
(* addition Sept. 2003, RM*)
         SPD[a,b] = z;
         SP[a,b] = z;
        ];
 If[(nf[a] === 1) && (nf[b] === 1), rst = z,
    If[(a =!= 0) && (b =!= 0),
        rst = z/nf[a]/nf[b]
   ]  ]
  ];
 ];
(* might be a setting before *)
   If[z =!= ste,
      downv = DownValues[ScalarProduct];
      downv = SelectFree[downv,
                     RuleDelayed@@{HoldPattern@@{scal[a,b,c]}, ste} /.
                     scal -> ScalarProduct];
      DownValues[ScalarProduct] = downv;
If[FreeQ[a,Pattern],
   rst = z,
   rst = z
   ] ,
      rst = ste
     ];
nd = RuleDelayed @@ {HoldPattern @@ {ScalarProduct[a, b, c]}, rst};
If[!MemberQ[DownValues[ScalarProduct], nd],
   AppendTo[DownValues[ScalarProduct], nd] ];
rst];


     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, a_, ___Rule],
                  TraditionalForm
                 ] := SuperscriptBox@@{MakeBoxes@@{a, TraditionalForm
                                                  }, 2
                                      };
     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_Plus, b_], TraditionalForm] :=
     RowBox[{"(",MakeBoxes[a, TraditionalForm],")", "\[CenterDot]",
             MakeBoxes[b, TraditionalForm]}
           ]/;Head[b]=!=Plus;

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, b_Plus], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]","(",
             MakeBoxes[b, TraditionalForm], ")"}
           ]/;Head[b]=!=Plus;

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_Plus, b_Plus], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]","(",
             MakeBoxes[b, TraditionalForm], ")"}
           ];

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, b_], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]",
             MakeBoxes[b, TraditionalForm]}
           ]

initialDownValues = DownValues[ScalarProduct];
initialUpValues = UpValues[ScalarProduct];

(* tentative *)

(*
Unprotect[ReplaceAll];
ReplaceAll[y_, ScalarProduct[a_, b_] -> z_] :=
   (y /. Pair[Momentum[a, ___Symbol], Momentum[b, ___Symbol]] -> z
   ) /; FreeQ[y, ScalarProduct[a,b]];
Protect[ReplaceAll];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProduct | \n "]];
Null
