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

ApartFF::usage =
"ApartFF[amp,{q1,q2,...}] partial fractions loop integrals by decomposing them \
into simpler integrals that contain only linearly independent propagators. It \
uses FCApart as a backend and works and is suitable also for multiloop integrals.";

Apart3::usage =
"Apart3[expr, x] is equivalent to Map2[Factor2, Collect2[Apart1[expr,x],x]].";

ExcludeMasses::usage =
"ExcludeMasses is an option of Apart2. It allows to specify masses for \
which partional fractioning should not be performed,e.g. ExcludeMasses->{m1,m2,3}"

ApartFF::failmsg = "Error! ScalApartFF has encountered a fatal problem and must abort \
the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Apart`Private`"]

affVerbose::usage="";

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


(*	FeynAmpDenominator is an internal option to make ApartFF behave like SPC,
	i.e. block partial fractioning on loop integrals that don't contain scalar products *)
Options[ApartFF] = {
	Collecting -> True,
	ExpandScalarProduct -> True,
	FCI -> False,
	FCVerbose -> False,
	FDS -> True,
	FeynAmpDenominator -> True,
	FeynAmpDenominatorCombine -> True
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

ApartFF[int_, lmoms_ , OptionsPattern[]]:=
	Block[{	exp,tmp,loopHead,null1,null2,res,rest,
			loopInts,intsUnique,solsList,repRule},

		If [OptionValue[FCVerbose]===False,
			affVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				affVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			exp = int,
			exp = FCI[int]
		];

		If[	OptionValue[FeynAmpDenominatorCombine],
			exp = FeynAmpDenominatorCombine[exp]
		];

		FCPrint[3, "ApartFF: Entering with ", exp, FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: Loop momenta are ", lmoms, FCDoControl->affVerbose];

		(*	Split loop integrals from the rest	*)
		tmp = FCLoopSplit[exp,lmoms];

		If[	OptionValue[FeynAmpDenominator],
			rest  = tmp[[1]];
			loopInts = FCLoopIsolate[Plus@@tmp[[2;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->True, PaVe->False],

			rest  = tmp[[1]]+tmp[[2]];
			loopInts = FCLoopIsolate[Plus@@tmp[[3;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->True, PaVe->False]
		];

		(*	Split loop integrals from the rest	*)
		intsUnique = (Cases[loopInts+null1+null2,loopHead[___],Infinity]/.null1|null2->0)//Union;

		FCPrint[3,"ApartFF: List of the unique integrals: ", intsUnique, FCDoControl->affVerbose];

		(*	Apply FCApart to each of the unique loop integrals	*)
		solsList = Map[FCApart[#,lmoms,FCI->True,FDS->OptionValue[FDS]]&,(intsUnique/.loopHead->Identity)];

		If[Length[solsList]=!=Length[intsUnique],
			Message[ApartFF::failmsg,"ApartFF can't create the solution list."];
			Abort[]
		];

		repRule = MapIndexed[(Rule[#1, First[solsList[[#2]]]]) &, intsUnique];

		FCPrint[3, "ApartFF: Replacement rule ", repRule, FCDoControl->affVerbose];

		(*	Substitute simplified integrals back into the original expression	*)
		res = FeynAmpDenominatorCombine[rest + (loopInts/.repRule)];

		If [OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res]
		];

		If[	OptionValue[Collecting],
			res = Collect2[res,FeynAmpDenominator]
		];

		FCPrint[3, "ApartFF: Leaving with ", res, FCDoControl->affVerbose];
		res
	]

FCPrint[1,"Apart.m loaded."];
End[]
