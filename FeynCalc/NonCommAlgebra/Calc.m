(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Calc*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: changed slightly September 2003                                *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Calc does a lot *)

(* ------------------------------------------------------------------------ *)

Calc::usage =
"Calc[exp] performs several simplifications.
Calc[exp] is the same as
DotSimplify[DiracSimplify[EpsEvaluate[Contract[DiracSimplify[Contract[Explicit[SUNSimplify[PowerSimplify[Trick[exp]],
Explicit -> False]]]]]]]].";

(* ------------------------------------------------------------------------ *)

Begin["`Calc`Package`"]
End[]

Begin["`Private`"]

Calc[expr_] :=
	Block[{calc},
		calc[exp_] :=calc[exp] =
			Expand2[ExpandScalarProduct[ DotSimplify[DiracSimplify[EpsEvaluate[Contract[DiracSimplify[
			Contract[Explicit[ SUNSimplify[Trick[exp]//PowerSimplify, Explicit -> False] ]]]]]]]]//PowerSimplify
		];
		FixedPoint[calc,expr, 5]
	];
FCPrint[1,"Calc.m loaded"];
End[]
