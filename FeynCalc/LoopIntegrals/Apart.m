(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Apart															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Collection of different partial fractioning routines		*)

(* ------------------------------------------------------------------------ *)

Apart1::usage =
"Apart1[expr, x] is equivalent to Apart[expr, x], \
but it fixes a Mathematica bug.";

Apart2::usage =
"Apart2[expr] partial fractions very simple 1-loop integrals " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/Apart2"],
StandardForm]

ApartFF::usage =
"ApartFF[amp,{q1,q2,...}] partial fractions loop integrals " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/ApartFF"],
StandardForm]

Apart3::usage =
"Apart3[expr, x] is equivalent to Map2[Factor2, Collect2[Apart1[expr,x],x]].";

ExcludeMasses::usage =
"ExcludeMasses is an option of Apart2. It allows to specify masses for \
which partional fractioning should not be performed,e.g. ExcludeMasses->{m1,m2,3}"

ApartFF::failmsg = "Error! ScalApartFF has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

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
	DropScaleless -> True,
	ExpandScalarProduct -> True,
	Factoring -> Factor,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	FDS -> True,
	FeynAmpDenominator -> True,
	FeynAmpDenominatorCombine -> True,
	FCProgressBar -> False,
	MaxIterations -> Infinity,
	Numerator -> True,
	SetDimensions-> {3, 4, D-1, D},
	TemporalMomentum -> False
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

ApartFF[int_, lmoms_List , OptionsPattern[]]:=
	Block[{	exp,tmp,loopHead,null1,null2,res,rest,
			loopInts,intsUnique,solsList,repRule, time,
			optCollecting, tcRepList},

		optCollecting = OptionValue[Collecting];

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

		FCPrint[1, "ApartFF: Entering.", FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: Entering with ", exp, FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: Loop momenta are ", lmoms, FCDoControl->affVerbose];


		(*
			By default we will not do partial fractioning w.r.t the temporal loop momenta, since those
			are not regularized in DR.
		*)
		tcRepList = {};
		If[	!OptionValue[TemporalMomentum] && !FreeQ[exp,TemporalMomentum],
			tcRepList = Map[Rule[TemporalMomentum[#], TemporalMomentum[Unique["fctm"]]] &, lmoms];
			exp = exp /. Dispatch[tcRepList]
		];


		(*	Split loop integrals from the rest	*)
		tmp = FCLoopSplit[exp,lmoms, Collecting->False];

		time=AbsoluteTime[];
		FCPrint[1, "ApartFF: Applying FCLoopRemoveNegativePropagatorPowers.", FCDoControl->affVerbose];
		tmp = FCLoopRemoveNegativePropagatorPowers[tmp,FCI->True,FCLoopPropagatorPowersCombine -> False];
		FCPrint[1, "ApartFF: Done applying FCLoopRemoveNegativePropagatorPowers, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: After FCLoopRemoveNegativePropagatorPowers: ", tmp, FCDoControl->affVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "ApartFF: Extracting unique loop integrals.", FCDoControl->affVerbose];

		If[	OptionValue[FeynAmpDenominator],
			rest  = tmp[[1]];
			loopInts = FCLoopIsolate[Plus@@tmp[[2;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->True, PaVe->False, Numerator->OptionValue[Numerator]],

			rest  = tmp[[1]]+tmp[[2]];
			loopInts = FCLoopIsolate[Plus@@tmp[[3;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->True, PaVe->False, Numerator->OptionValue[Numerator]]
		];



		(*	Split loop integrals from the rest	*)
		intsUnique = (Cases[loopInts+null1+null2,loopHead[___],Infinity]/.null1|null2->0)//Union;
		FCPrint[1, "ApartFF: Done extracting unique loop integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose];

		FCPrint[1,"ApartFF: Number of the unique integrals: ", Length[intsUnique], FCDoControl->affVerbose];
		FCPrint[3,"ApartFF: List of the unique integrals: ", intsUnique, FCDoControl->affVerbose];

		time=AbsoluteTime[];

		FCPrint[1, "ApartFF: Applying FCApart.", FCDoControl->affVerbose];
		(*	Apply FCApart to each of the unique loop integrals	*)
		solsList = MapIndexed[ (If[ OptionValue[FCProgressBar],
									FCProgressBar["ApartFF: Processing integral ",First[#2],Length[intsUnique]]
								];
			FCApart[#1,lmoms,FCI->True,FDS->OptionValue[FDS],DropScaleless->OptionValue[DropScaleless],
			MaxIterations->OptionValue[MaxIterations],SetDimensions->OptionValue[SetDimensions]])&,(intsUnique/.loopHead->Identity)];

		FCPrint[1, "ApartFF: Done applying FCApart, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: After FCApart: ", solsList, FCDoControl->affVerbose];

		If[Length[solsList]=!=Length[intsUnique],
			Message[ApartFF::failmsg,"ApartFF can't create the solution list."];
			Abort[]
		];

		repRule = Thread[Rule[intsUnique, solsList]];
		FCPrint[3, "ApartFF: Replacement rule ", repRule, FCDoControl->affVerbose];

		(*	Substitute simplified integrals back into the original expression	*)

		time=AbsoluteTime[];
		FCPrint[1, "ApartFF: Inserting simplified integrals back into the original expression.", FCDoControl->affVerbose];
		res = FeynAmpDenominatorCombine[rest + (loopInts/.repRule)];
		FCPrint[1, "ApartFF: Done inserting simplified integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose];


		If[	tcRepList=!={},
				res = res /. Dispatch[Reverse/@tcRepList]
		];

		If [OptionValue[ExpandScalarProduct],
			time=AbsoluteTime[];
			FCPrint[1, "ApartFF: Applying ExpandScalarProduct.", FCDoControl->affVerbose];
			res = ExpandScalarProduct[res, FCI->True];
			FCPrint[1, "ApartFF: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose]
		];

		If[	optCollecting=!=False,
			time=AbsoluteTime[];
			FCPrint[1, "ApartFF: Applying Collect2.", FCDoControl->affVerbose];

			If[ TrueQ[optCollecting===True],
				res = Collect2[res,FeynAmpDenominator, Factoring->OptionValue[Factoring]],
				res = Collect2[res,optCollecting, Factoring->OptionValue[Factoring]]
			];

			FCPrint[1, "ApartFF: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->affVerbose]
		];

		FCPrint[1, "ApartFF: Leaving.",  FCDoControl->affVerbose];
		FCPrint[3, "ApartFF: Leaving with ", res, FCDoControl->affVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	]

FCPrint[1,"Apart.m loaded."];
End[]
