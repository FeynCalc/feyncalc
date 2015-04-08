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

Apart2[y_] :=
	(FeynCalcInternal[y] //. FeynAmpDenominator -> feynampdenpartfrac) /.
	feynampdenpartfrac -> FeynAmpDenominator;

feynampdenpartfrac[a___, PropagatorDenominator[qpe1_, m1_], b___,
PropagatorDenominator[qpe1_, m2_], c___] :=
	Factor2[(1/(m1^2 - m2^2) *
	(FeynAmpDenominator[a, PropagatorDenominator[qpe1, m1], b, c] -
	FeynAmpDenominator[a, b, PropagatorDenominator[qpe1, m2], c]))] /; m1 =!= m2;

Apart3[expr_, x_] :=
	Map2[Factor2, Collect2[Apart1[expr,x],x]];

FCPrint[1,"Apart.m loaded."];
End[]
