(* :Title: Apart1 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Apart1::usage =
"Apart1[expr, x] is equivalent to Apart[expr, x],
but it fixes a Mathematica bug.";

Apart2::usage =
"Apart2[expr] partial fractions FeynAmpDenominators.";

Apart3::usage =
"Apart3[expr, x] is equivalent to
Map2[Factor2, Collect2[Apart1[expr,x],x]].";

ExcludeMasses::usage =
"ExcludeMasses is an option of Apart2. It allows to specify masses for
which partional fractioning should not be performed,e.g. ExcludeMasses->{m1,m2,3}"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Apart`Private`"]

Apart1[expr_, x_] :=
	Block[ {i},
		If[ FreeQ[expr, Complex],
			Apart[expr,x],
			Apart[expr /. Complex[0,a_] :> i a, x] /. i->I
		]
	];

Options[Apart2]= {
	Factoring->True,
	ExcludeMasses->{}
};

Apart2[y_, OptionsPattern[]] :=
	Block[{factoring,factFun,exclM},
	factoring = OptionValue[Factoring];
	exclM  = OptionValue[ExcludeMasses];

	If[factoring,
		factFun=Factor2,
		factFun=Identity,
		factFun=factoring
	];
	feynampdenpartfrac[a___, PropagatorDenominator[qpe1_, m1_], b___,
	PropagatorDenominator[qpe1_, m2_], c___] :=
		factFun[(1/(m1^2 - m2^2) *
		(FeynAmpDenominator[a, PropagatorDenominator[qpe1, m1], b, c] -
		FeynAmpDenominator[a, b, PropagatorDenominator[qpe1, m2], c]))] /;
		(m1 =!= m2) && FreeQ2[{m1,m2},exclM];


	(FeynCalcInternal[y] //. FeynAmpDenominator -> feynampdenpartfrac) /.
	feynampdenpartfrac -> FeynAmpDenominator
];
Apart3[expr_, x_] :=
	Map2[Factor2, Collect2[Apart1[expr,x],x]];

FCPrint[1,"Apart.m loaded."];
End[]
