(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tr2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTr2ace) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Tr2`",
             {"HighEnergyPhysics`FeynCalc`"}];

Tr2::"usage"=
"If exp contains DiracTrace's, Tr2[exp] simplifies exp and does the
Dirac traces unless more that 4 gamma matrices and DiracGamma[5] occur.
Tr2[exp] also separates the color-strucure,
and takes the color trace if Tf occurs in exp.
If exp does not contain DiracTrace's, Tr2[exp] takes the Dirac trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
DiracTraceEvaluate = MakeContext["CoreOptions","DiracTraceEvaluate"];
EpsContract = MakeContext["CoreOptions","EpsContract"];
Factoring = MakeContext["CoreOptions","Factoring"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
SUNNToCACF = MakeContext["CoreOptions","SUNNToCACF"];
Tf = MakeContext["CoreObjects","Tf"];
SUNIndex = MakeContext["CoreObjects","SUNIndex"];
FCI := FCI = MakeContext["FeynCalcInternal"];

MakeContext[
Collect2,
Contract,
DiracGammaExpand,
DiracOrder,
DiracSimplify,
DiracTrace,
ExpandScalarProduct,
Explicit,
FeynCalcForm,
FreeQ2,
InsideDiracTrace,
NonCommFreeQ,
SUNSimplify,
SUNTrace,
Select1,
Select2,
TR,
Trick
];



(* NonCommQ replaced with NonCommFreeQ due to change (fix) of
   definitions of these functions. F.Orellana, 13/9-2002 *)

Options[ Tr2 ] = {Factoring -> False};


dirtr[x_, ___Rule] := If[FreeQ[x, SUNIndex], DiracTrace[x],
             If[!FreeQ[x, Tf],
                SUNSimplify[DiracTrace[x], SUNTrace->True,
                                           Explicit -> False],
                SUNSimplify[DiracTrace[x], SUNTrace->False,
                                           Explicit -> False]
               ]];

treasy[0] = 0;
treasy[y_Plus] := Map[treasy, y];
treasy[a_] := TR[a] /; NonCommFreeQ[a];

treasy[fa_. DiracGamma[5]] := 0 /; FreeQ[fa, DiracGamma];
treasy[fa_. DOT[x_,y__]] :=
 If[FreeQ[fa, DOT] &&
    (FreeQ2[{x,y}, {DiracGamma[5], DiracGamma[6],  DiracGamma[7]} ] ||
     Length[{x,y}] < 6
    ), TR[fa DOT[x,y]],
    DiracTrace[fa DOT[x,y]]
   ];

trup /: (f_/;FreeQ2[f, {DiracTrace,DOT}]) trup[x_,ops___Rule] :=
  DiracTrace[f x, ops];
trap[y_,ops___Rule] := If[Head[y] =!= Times,
               DiracTrace[y,ops],
               Select1[y, {DiracGamma, LorentzIndex}] *
               DiracTrace[y/Select1[y, {DiracGamma, LorentzIndex}],ops]
              ];

trdifficult[y_, ops___Rule] :=
 If[MatchQ[y, _. DOT[(a__) /; FreeQ2[{a}, {DiracGamma[5],DiracGamma[6],
                                           DiracGamma[7]}], DiracGamma[5]]
          ],
    treasy[Expand[ExpandScalarProduct[
           Contract[DiracOrder[
            Collect2[
             DiracSimplify[y, InsideDiracTrace -> True],
                     DOT, Factoring -> False]],
                               EpsEvaluate->False]],
                 DiracGamma],ops
          ],
    treasy[Expand[ExpandScalarProduct[
           Contract[DiracOrder[y],EpsEvaluate->False]],
              DiracGamma], ops
          ]
   ] /. treasy -> DiracTrace /. DiracTrace -> trup /.
        trup -> DiracTrace /. DiracTrace -> trap;

Tr2[x_] := Block[{tt},
tt = FCI[x];
If[FreeQ[tt, DiracTrace], tt = DiracTrace[tt]];
tt = Trick[tt];
If[!FreeQ[tt, SUNIndex],
   tt = SUNSimplify[tt /. DiracTrace -> dirtr, SUNNToCACF -> True]
  ];
tt = tt /. DiracTrace -> treasy /. treasy -> DiracTrace;
tt = tt /. DiracTrace -> trup /.trup -> DiracTrace /. DiracTrace -> trap;
tt = tt /. DiracTrace -> trdifficult;
tt];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tr2 | \n "]];
Null
