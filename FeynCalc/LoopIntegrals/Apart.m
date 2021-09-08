(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Apart															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Collection of different partial fractioning routines		*)

(* ------------------------------------------------------------------------ *)

Apart1::usage =
"Apart1[expr, x] is equivalent to Apart[expr, x], but it fixes a Mathematica
bug relevant when expr contains complex numbers.";

Apart2::usage =
"Apart2[expr] partial fractions propagators of the form
$1/[(q^2-m1^2)(q^2-m2^2)]$.";

Apart3::usage =
"Apart3[expr, x] is equivalent to Map2[Factor2, Collect2[Apart1[expr,x],x]].";

ApartFF::usage =
"ApartFF[amp, {q1, q2, ...}] partial fractions loop integrals by decomposing
them into simpler integrals that contain only linearly independent
propagators. It uses FCApart as a backend and is equally suitable for 1-loop
and  multi-loop integrals.

FCApart  implements an algorithm based on
[arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F. Feng that seems to
employ a variety Leinartas's algorithm (cf.
[arXiv:1206.4740](https://arxiv.org/abs/1206.4740)). Unlike Feng's
[$Apart](https://github.com/F-Feng/APart) that is applicable to general
multivariate polynomials, FCApart is tailored to work only with FeynCalc's
FeynAmpDenominator, Pair and CartesianPair symbols, i.e. it is less general in
this respect.

ApartFF[amp * extraPiece1, extraPiece2, {q1, q2, ...}] is a special working
mode of ApartFF, where the final result of partial fractioning amp*extraPiece1
is multiplied by extraPiece2. It is understood, that extraPiece1*extraPiece2
should be unity, e. g. when extraPiece1 is an FAD, while extraPiece is an SPD
inverse to it. This mode should be useful for nonstandard integrals where the
desired partial fraction decomposition can be performed only after multiplying
amp with extraPiece1.";

ExcludeMasses::usage =
"ExcludeMasses is an option of Apart2. It allows to specify masses w.r.t which
partial fraction decomposition should not be performed, e.g.
ExcludeMasses->{m1,m2,3}.";

ApartFF::failmsg = "Error! ScalApartFF has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Apart`Private`"]

affVerbose::usage="";
optExcludeMasses::usage="";
optSqrt::usage="";
factFun::usage="";

(*	FeynAmpDenominator is an internal option to make ApartFF behave like SPC,
	i.e. block partial fractioning on loop integrals that don't contain scalar products *)
Options[ApartFF] = {
	Collecting 					-> True,
	DropScaleless 				-> True,
	ExpandScalarProduct 		-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCProgressBar 				-> False,
	FCVerbose 					-> False,
	FDS 						-> True,
	Factoring 					-> {Factor, 5000},
	FeynAmpDenominator 			-> True,
	FeynAmpDenominatorCombine	-> True,
	MaxIterations 				-> Infinity,
	Numerator 					-> True,
	SetDimensions				-> {3, 4, D-1, D},
	TemporalMomentum 			-> False,
	TimeConstrained 			-> 3
};

Options[Apart2]= {
	Factoring		->	True,
	FCE				->	False,
	FCI				->	False,
	ExcludeMasses	->	{},
	Sqrt			->	True
};

Apart1[expr_, x_] :=
	Block[ {i},
		If[ FreeQ[expr, Complex],
			Apart[expr,x],
			Apart[expr /. Complex[0,a_] :> i a, x] /. i->I
		]
	];

Apart2[expr_, OptionsPattern[]] :=
	Block[{ex, res, optFactoring, factFun, exclM},

		optFactoring		= OptionValue[Factoring];
		optExcludeMasses	= OptionValue[ExcludeMasses];
		optSqrt				= OptionValue[Sqrt];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	optFactoring,
			factFun = Factor2,
			factFun = Identity,
			factFun = optFactoring
		];

		res = ex//. FeynAmpDenominator -> feynampdenpartfrac /.
			feynampdenpartfrac -> FeynAmpDenominator;

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res
];


feynampdenpartfrac[a___, PropagatorDenominator[qpe1_, m1_], b___, PropagatorDenominator[qpe1_, m2_], c___] :=
	factFun[1/(m1^2 - m2^2) * (
			FeynAmpDenominator[a, PropagatorDenominator[qpe1, m1], b, c] -
			FeynAmpDenominator[a, b, PropagatorDenominator[qpe1, m2], c]
			)] /; (m1 =!= m2) && FreeQ2[{m1,m2},optExcludeMasses];

feynampdenpartfrac[a___, cpd : CartesianPropagatorDenominator[mom_CartesianMomentum, 0, mm_ : 0, {1, _}], b___,
						gpd : GenericPropagatorDenominator[(c1_ :0) + (c2_. Sqrt[CartesianPair[mom_CartesianMomentum, mom_CartesianMomentum]]), {1, _}], c___] :=
	factFun[(c1 - c2 Sqrt[CartesianPair[mom, mom]])/(c1^2 + c2^2 mm) FeynAmpDenominator[a, b, cpd, c] +
			c2^2/(c1^2 + c2^2 mm) FeynAmpDenominator[a, gpd, b, c]
	]/; optSqrt;

feynampdenpartfrac[a___, gpd : GenericPropagatorDenominator[(c1_ :0) + (c2_. Sqrt[CartesianPair[mom_CartesianMomentum, mom_CartesianMomentum]]), {1, _}], b___,
						cpd : CartesianPropagatorDenominator[mom_CartesianMomentum, 0, mm_ : 0, {1, _}], c___] :=
	factFun[(c1 - c2 Sqrt[CartesianPair[mom, mom]])/(c1^2 + c2^2 mm) FeynAmpDenominator[a, cpd, b, c] +
			c2^2/(c1^2 + c2^2 mm) FeynAmpDenominator[a, b, gpd, c]
	]/; optSqrt;

Apart3[expr_, x_] :=
	Map2[Factor2, Collect2[Apart1[expr,x],x]];

ApartFF[int_, lmoms_List , opts:OptionsPattern[]]:=
	ApartFF[int, 1, lmoms , opts];


ApartFF[int_, extraPiece_, lmoms_List , OptionsPattern[]]:=
	Block[{	exp,tmp,loopHead,null1,null2,res,rest,
			loopInts,intsUnique,solsList,repRule, time,
			optCollecting, tcRepList, optFDS, optDropScaleless},

		optCollecting 		= OptionValue[Collecting];
		optFDS 				= OptionValue[FDS];
		optDropScaleless	= OptionValue[DropScaleless];

		If [OptionValue[FCVerbose]===False,
			affVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
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
			The usage of extraPiece indicates that ApartFF/FCApart operates only on a part of the full
			loop integral. Therefore, we are not allowed to discard seemingly scaleless integrals
			or perform shifts in loop momenta. All this should be done in a second run of ApartFF/FCApart
			without an extraPiece.
		*)
		If[	extraPiece =!= 1,
			FCPrint[1,"ApartFF: extraPiece=!=1, disabling FDS and DropScaleless.", FCDoControl->affVerbose];
			optFDS = False;
			optDropScaleless = False;
		];



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
			loopInts = FCLoopIsolate[Plus@@tmp[[2;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->optDropScaleless, PaVe->False, Numerator->OptionValue[Numerator]],
			(*FDS->False means that pure denominator integrals are omitted*)
			rest  = tmp[[1]]+tmp[[2]];
			loopInts = FCLoopIsolate[Plus@@tmp[[3;;4]], lmoms, FCI->True, Head->loopHead, DropScaleless->optDropScaleless, PaVe->False, Numerator->OptionValue[Numerator]]
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
			FCApart[#1, extraPiece, lmoms, FCI->True, FDS->optFDS, DropScaleless->optDropScaleless,
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
		res = FeynAmpDenominatorCombine[rest*extraPiece + (loopInts/.repRule)];
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
				res = Collect2[res,FeynAmpDenominator, Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained]],
				res = Collect2[res,optCollecting, Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained]]
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
