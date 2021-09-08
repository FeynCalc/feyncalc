(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tr2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTr2ace) *)

(* ------------------------------------------------------------------------ *)

Tr2::usage=
"If exp contains DiracTraces, Tr2[exp] simplifies exp and does the Dirac traces
unless more that 4 gamma matrices and DiracGamma[5] occur. Tr2[exp] also
separates the color-structure, and takes the color trace if Tf occurs in exp.
If exp does not contain DiracTraces, Tr2[exp] takes the Dirac trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Tr2`Private`"]

Options[Tr2] = {
	Factoring -> False
};


dirtr[x_, OptionsPattern[]] :=
	If[FreeQ[x, SUNIndex], DiracTrace[x],
		If[!FreeQ[x, Tf],
			SUNSimplify[DiracTrace[x], SUNTrace->True, Explicit -> False],
			SUNSimplify[DiracTrace[x], SUNTrace->False, Explicit -> False]
		]
	];

treasy[0] = 0;
treasy[y_Plus] :=
	Map[treasy, y];
treasy[a_] :=
	TR[a] /; NonCommFreeQ[a];
treasy[fa_. DiracGamma[5]] :=
	0 /; FreeQ[fa, DiracGamma];
treasy[fa_. DOT[x_,y__]] :=
	If[FreeQ[fa, DOT] && (FreeQ2[{x,y}, {DiracGamma[5], DiracGamma[6],
		DiracGamma[7]} ] || Length[{x,y}] < 6),
		TR[fa DOT[x,y]],
		DiracTrace[fa DOT[x,y]]
	];

trup /: (f_/;FreeQ2[f, {DiracTrace,DOT}]) trup[x_,ops___Rule] :=
	DiracTrace[f x, ops];

trap[y_,ops___Rule] :=
	If[Head[y] =!= Times,
		DiracTrace[y,ops],
		SelectFree[y, {DiracGamma, LorentzIndex}] *
		DiracTrace[y/SelectFree[y, {DiracGamma, LorentzIndex}],ops]
	];

trdifficult[y_, ops___Rule] :=
	If[MatchQ[y, _. DOT[(a__) /; FreeQ2[{a}, {DiracGamma[5],DiracGamma[6], DiracGamma[7]}], DiracGamma[5]]],
		treasy[Expand[ExpandScalarProduct[Contract[DiracOrder[Collect2[DiracSimplify[y, InsideDiracTrace -> True],
		DOT, Factoring -> False]],EpsContract->False]],DiracGamma],ops],
		treasy[Expand[ExpandScalarProduct[Contract[DiracOrder[y],EpsContract->False]], DiracGamma], ops]
	] /. treasy -> DiracTrace /. DiracTrace -> trup /. trup -> DiracTrace /. DiracTrace -> trap;

Tr2[x_] :=
	Block[{tt=FCI[x]},

		If[FreeQ[tt, DiracTrace],
			tt = DiracTrace[tt]
		];
		tt = Trick[tt];
		If[!FreeQ[tt, SUNIndex],
			tt = SUNSimplify[tt /. DiracTrace -> dirtr, SUNNToCACF -> True]
		];
		tt = tt /. DiracTrace -> treasy /. treasy -> DiracTrace;
		tt = tt /. DiracTrace -> trup /.trup -> DiracTrace /. DiracTrace -> trap;
		tt = tt /. DiracTrace -> trdifficult;
		tt
	];

FCPrint[1,"Tr2.m"];
End[]
