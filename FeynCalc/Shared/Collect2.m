(* ::Package:: *)



(* :Title: Collect2															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Extension of the Mathematica Collect

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

Collect2::usage=
"Collect2[expr, x] collects together terms which are not free of any occurrence
of x.

Collect2[expr, {x1, x2, ...}] (or also Collect2[expr, x1, x2, ...]) collects
together terms which are not free of any occurrence of x1, x2, ....

The coefficients are put over a common denominator. If expr is expanded before
collecting depends on the option Factoring, which may be set to Factor,
Factor2, or any other function, which is applied to the coefficients. If expr
is already expanded with respect to x (x1, x2, ...), the option Expanding can
be set to False.";

FactoringDenominator::usage =
"FactoringDenominator is an option for Collect2. It is taken into account only
when the option Numerator is set to True. If FactoringDenominator is set to
any function f, this function will be applied to the denominator of the
fraction. The default value is False, i.e. the denominator will be left
unchanged.";

Collect2::failmsg =
"Error! Collect2 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`Collect2`Private`"];

Options[Collect2] = {
	Denominator 				-> False,
	Dot 						-> False,
	Expanding 					-> True,
	FCFactorOut 				-> 1,
	FCVerbose 					-> False,
	Factoring 					-> Factor,
	FCParallelize				-> False,
	FactoringDenominator 		-> False,
	Head						-> Identity,
	InitialFunction 			-> Identity,
	IntermediateSubstitutions	-> {},
	IsolateFast 				-> False,
	IsolateNames 				-> False,
	Numerator					-> False,
	ParallelKernels				-> False,
	TimeConstrained 			-> Infinity
};

Collect2[a_ == b_, y__] :=
	Collect2[a,y] == Collect2[b,y];

Collect2[(h:Rule|RuleDelayed)[a_,b_], y__] :=
	With[{zz=Collect2[b,y]}, h[a,zz]];


Collect2[x_List, y__,  opts:OptionsPattern[]] :=
	Block[{res,time,optVerbose,optIsolateNames},

		If[OptionValue[FCVerbose]===False,
				optVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					optVerbose=OptionValue[FCVerbose]
				];
			];

		time=AbsoluteTime[];
		optIsolateNames = OptionValue[IsolateNames];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1, "Collect2: Applying Collect2 to a list in parallel." , FCDoControl->optVerbose];
				If[	optIsolateNames=!=False,
					If[	TrueQ[!(Head[optIsolateNames] === List && Length[optIsolateNames]===$KernelCount)],
						Message[Collect2::failmsg,"In the parallel mode, the option IsolateNames should be set to a list with the length being equal to the number of parallel kernels."];
						Abort[]
					];
					Table[With[{oin = optIsolateNames[[i]]},
					ParallelEvaluate[SetOptions[Collect2, IsolateNames -> oin];, i, DistributedContexts -> None]],{i,1,$KernelCount}];
				];

				With[{xxx = {y}, ooo = {opts}},
					ParallelEvaluate[FCParallelContext`Collect`pArgs = Flatten[xxx]; FCParallelContext`Collect`pOpts = FilterRules[ooo, Except[FCParallelize|FCVerbose|IsolateNames]];, DistributedContexts -> None]
				];
				res = ParallelMap[(Collect2[#,FCParallelContext`Collect`pArgs,FCParallelContext`Collect`pOpts, FCParallelize->False])&,x, DistributedContexts->None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[x]/$KernelCount]/10]];

				If[	optIsolateNames=!=False,
					Table[ParallelEvaluate[SetOptions[Collect2, IsolateNames -> False];, DistributedContexts -> None],{i,1,$KernelCount}];
				],
				FCPrint[1, "Collect2: Applying Collect2 to a list.", FCDoControl->optVerbose];
				res = (Collect2[#, y, FilterRules[{opts}, Except[FCParallelize|FCVerbose]]]& /@ x)
		];

		FCPrint[1, "Collect2: Collecing done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		res
	];

Collect2[x_, y_, opts:OptionsPattern[]] :=
	Collect2[x, {y}, opts] /; (Head[y]=!=List && !OptionQ[y] && Head[x]=!=List && !OptionQ[x]);

Collect2[x_, z__, y_, opts:OptionsPattern[]] :=
	Collect2[x, {z,y}, opts] /; (Head[y]=!=List && !OptionQ[y] && Head[x]=!=List && !OptionQ[x]);

Collect2[expr_/; !MemberQ[{List,Equal},Head[expr]], vv_List/; (!OptionQ[vv] || vv==={}), opts:OptionsPattern[]] :=
	Block[{monomList,ru,ex,holdForm2,factoring,optIsolateNames,tog,fr0,frx,lin,tv={},mp,monomialHead,cd,co,dde,
		new = 0, unity,res,compCON,ccflag = False, factor,expanding, times,time,time0, sparseArray, tmp,
		null1,null2,coeffArray,tvm,coeffHead,optIsolateFast,tempIso,factorOut, monomRepRule={}, coeffs,
		nonAtomicMonomials,optHead,firstHead,secondHead=Null,optInitialFunction,numerator,denominator,
		optNumerator, optFactoringDenominator, optTimeConstrained, ident, optVerbose,optFCParallelize, frh},

		If[	OptionValue[ParallelKernels] && $KernelID===0,
			Message[Collect2::failmsg,"Tasks for parallel kernels are being executed on the main kernel."];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];



		{factoring, optIsolateNames, expanding, dde, optIsolateFast} =
			{OptionValue[Factoring], OptionValue[IsolateNames],
			OptionValue[Expanding], OptionValue[Denominator],
			OptionValue[IsolateFast]  };


		optHead 				= OptionValue[Head];
		optInitialFunction 		= OptionValue[InitialFunction];
		optNumerator 			= OptionValue[Numerator];
		optFactoringDenominator = OptionValue[FactoringDenominator];
		optTimeConstrained 		= OptionValue[TimeConstrained];
		factorOut 				= OptionValue[FCFactorOut];
		optFCParallelize		= OptionValue[FCParallelize];

		If[	Head[optHead]===List,
			firstHead 	= optHead[[1]];
			secondHead	= optHead[[2]];
			If[	firstHead===Identity,
				firstHead=ident
			],
			firstHead 	= optHead
		];

		Which[
			OptionValue[Dot] === True,
			times = Dot,
			OptionValue[Dot] === False,
			times = Times,
			True,
			times = OptionValue[Dot]
		];

		Switch[factoring,
			False,
				factor = Identity,
			True|Factor2,
				factor = Function[fuArg,TimeConstrained[Factor2[fuArg],optTimeConstrained,fuArg]],
			{_,_Integer},
				factor = Function[fuArg,
					If[	TrueQ[LeafCount[fuArg]<factoring[[2]]],
						TimeConstrained[(factoring[[1]])[fuArg],optTimeConstrained,fuArg],
						fuArg
					]
				],
			_,
				factor = factoring
		];

		FCPrint[1, "Collect2: Entering Collect2.", FCDoControl->optVerbose];
		FCPrint[2, "Collect2: Entering with: ", expr, FCDoControl->optVerbose];

		ex = expr;

		If[	!FreeQ2[ex,{SeriesData,ConditionalExpression}],
			Message[Collect2::failmsg,"Collect2 cannot work on expressions that contain SeriesData or ConditionalExpression!"];
			Abort[]
		];

		If[	optNumerator && Head[ex]===Times,
			numerator=Numerator[ex];
			If[ optFactoringDenominator===False,
				denominator=Denominator[ex],
				denominator=optFactoringDenominator[Denominator[ex]]
			];
			ex = numerator,
			denominator = 1
		];

		ex = ex /. OptionValue[IntermediateSubstitutions];

		ex = ex/factorOut;

		FCPrint[2,"Collect2: After factoring out ", factorOut, " : ", ex,  FCDoControl->optVerbose];

		FCPrint[1, "Collect2: Applying initial function.", FCDoControl->optVerbose];
		If[	Head[optInitialFunction]===List,
			ex = (Composition@@optInitialFunction)[ex],
			ex = optInitialFunction[ex]
		];
		FCPrint[3, "Collect2: After initial function ", ex,  FCDoControl->optVerbose];

		monomList = Union[Select[ vv, ((Head[#] =!= Plus) && (Head[#] =!= Times) && (!NumberQ[#]))& ]];
		monomList = Select[ monomList, !FreeQ[ex, #]&];

		(*If the monomials are not atomic, we should better mask them beforehand *)
		nonAtomicMonomials = Select[monomList, ! AtomQ[#] &];
		If[nonAtomicMonomials=!={} && FCPatternFreeQ[nonAtomicMonomials],
			monomRepRule = Thread[Rule[nonAtomicMonomials,Table[Unique["monom"], {r,1,Length[nonAtomicMonomials]} ]]];
			monomList = monomList/.monomRepRule;
			ex = ex/.monomRepRule;
			monomRepRule = Reverse/@monomRepRule
		];

		FCPrint[1, "Collect2: Monomials w.r.t which we will collect: ", monomList, FCDoControl->optVerbose];

		If[Length[monomList] === 0,
			FCPrint[1, "Collect2: The input expression contains no relevant monomials, leaving.", FCDoControl->optVerbose];
			unity = 1;
			res = factorOut factor[ex]/denominator;
			If[	optIsolateNames=!=False,
				res  = Isolate[res,IsolateNames -> optIsolateNames, IsolateFast-> optIsolateFast]
			];
			Return[res]
		];


		(* Hm, that's a problem, maybe *)
		If[!FreeQ[ex, ComplexConjugate],
			ccflag = True;
			ex = ex /. ComplexConjugate -> compCON;
			monomList = monomList /. ComplexConjugate -> compCON;
		];

		ex = ex/. holdForm[k_[ii_]] -> holdForm2[k][ii];

		time=AbsoluteTime[];

		frh[x_] := FRH[x/.holdForm->Identity, IsolateNames->{optIsolateNames,tempIso}];

		If[ factoring === False,
			FCPrint[1, "Collect2: No factoring function defined.", FCDoControl->optVerbose];
			(* 	This can speed things up, if the expression contains very large sums free of
				monomials *)
			ex = ex /. Plus -> holdPlus /. holdPlus[x__] /; FreeQ2[{x}, monomList] :>
				Isolate[(Plus[x]/.holdPlus -> Plus), IsolateFast -> True, IsolateNames -> tempIso] /. holdPlus -> Plus;
			tog[x_] := frh[x],


			FCPrint[1, "Collect2: Factoring function is ", factor, FCDoControl->optVerbose];
			fr0[x__] :=
				Plus[x] /; !FreeQ2[{x}, monomList];
			tog[x_]  :=
				factor[frh[x]];
			frx[x__] :=
				holdForm[Plus[x]];
			ex = ex /. Plus -> fr0 /. fr0 -> frx
		];


		If[ expanding =!= False,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Expanding", FCDoControl->optVerbose];
			ex  = Expand2[ex,monomList];
			FCPrint[1, "Collect2: Expanding done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Separating the part free of the monomials (linear part)", FCDoControl->optVerbose];
		(* lin denotes the part free of monomList *)
		{lin,ex} = FCSplit[ex,monomList,Expanding->False];
		FCPrint[1, "Collect2: Separation done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		FCPrint[3, "Collect2: Part that contains the monomials: ", ex, FCDoControl->optVerbose];
		FCPrint[3, "Collect2: Linear part: ", lin, FCDoControl->optVerbose];

		If[factoring =!= False && lin=!=0,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Factoring the linear part", FCDoControl->optVerbose];
			lin = tog[lin];
			FCPrint[1, "Collect2: Factoring done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Wrapping the momomials with special heads.", FCDoControl->optVerbose];

		ex = (Map[(SelectFree[#, monomList] monomialHead[SelectNotFree[#, monomList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0}) ;
		tv = Cases2[ex,monomialHead];
		FCPrint[1, "Collect2: Wrapping done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		FCPrint[3, "Collect2: ex: ", ex , FCDoControl->optVerbose];
		FCPrint[3, "Collect2: tv: ", tv , FCDoControl->optVerbose];

		If[dde === True,
			FCPrint[1, "Collect2: Also denominators containing variables will be collected", FCDoControl->optVerbose];
			cd[x_] := ((Numerator[#]/(factor[Denominator[#]] /.
			Plus-> (Collect2[Plus[##], monomList, opts]&)))& @ x ) /;
			(!FreeQ[Denominator[x], Plus]) && (!FreeQ2[Denominator[x], monomList])
		];

		FCPrint[1, "Collect2: There are ", Length[tv] , " momomials to collect", FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Computing CoefficientArrays.", FCDoControl->optVerbose];
		coeffArray = CoefficientArrays[ex,tv];
		FCPrint[1, "Collect2: CoefficientArrays ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		FCPrint[3, "Collect2: CoefficientArrays: ", coeffArray, FCDoControl->optVerbose];

		If[	coeffArray=!={0},

			If[	Length[coeffArray]>2 || Head[coeffArray[[2]]]=!=SparseArray,
				Message[Collect2::failmsg,"Something went wrong when applying CoefficientArrays."];
				Abort[]
			];

			If[	coeffArray[[1]]=!=0,
				Message[Collect2::failmsg,"There is another linear part!"];
				Abort[]
			];

			sparseArray = coeffArray[[2]];

			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Collecting the monomials.", FCDoControl->optVerbose];

			tvm = (frh[#] /. monomialHead -> cd /. cd -> firstHead)&/@tv;

			FCPrint[3, "Collect2: tvm: ", tvm, FCDoControl->optVerbose];
			FCPrint[1, "Collect2: Done collecting the monomials, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

			time=AbsoluteTime[];
			If[	$ParallelizeFeynCalc && optFCParallelize,
					FCPrint[1, "Collect2: Factoring coefficients of the monomials in parallel.", FCDoControl->optVerbose];
					With[{xxx = unity, yyy=factor},
						ParallelEvaluate[FCParallelContext`Collect2`unity = xxx; FCParallelContext`Collect2`factor = yyy;, DistributedContexts -> None]
					];
					DistributeDefinitions[factor,factoring,optTimeConstrained];

					coeffs = frh/@Normal[sparseArray];
					coeffs = ParallelMap[factor[FCParallelContext`Collect2`unity*#]&,coeffs,
							DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[coeffs]/$KernelCount]/10]];
					coeffs = coeffs /. FCParallelContext`Collect2`unity->unity;

					,

					FCPrint[1, "Collect2: Factoring coefficients of the monomials.", FCDoControl->optVerbose];
					coeffs = tog[unity*#]&/@sparseArray
			];
			FCPrint[1, "Collect2: Done factoring coefficients of the monomials, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

			If[	optIsolateNames=!=False,
				time=AbsoluteTime[];
				FCPrint[1, "Collect2: Isolating coefficients of the monomials.", FCDoControl->optVerbose];
				coeffs = Isolate[#/. {unity->1, holdForm2[ka_][j_] :> holdForm[ka[j]]}, IsolateNames -> optIsolateNames, IsolateFast-> optIsolateFast]&/@coeffs;
				FCPrint[1, "Collect2: Done isolating coefficients of the monomials, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			];

			new =  Dot[coeffs,tvm],

			new = 0
		];

		FCPrint[3, "Collect2: new: ", new, FCDoControl->optVerbose];

		If[	!FreeQ2[lin,monomList],
			Message[Collect2::failmsg,"Linear part contains monomials!"];
			Print[lin];
			Abort[]
		];

		time=AbsoluteTime[];

		FCPrint[1, "Collect2: Releasing tempIso.", FCDoControl->optVerbose];
		If[ optIsolateNames =!= False,
			lin = Isolate[frh[lin], IsolateNames->optIsolateNames, IsolateFast->optIsolateFast],
			lin = frh[lin/.holdForm->Identity]
		];
		FCPrint[1, "Collect2: Done releasing tempIso, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	secondHead=!=Null,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Applying secondHead.", FCDoControl->optVerbose];
			lin = secondHead[lin,1] /. secondHead[0,_] -> 0;
			new = secondHead/@(new + null1 + null2) /. secondHead[null1|null2]->0 /. secondHead[a_firstHead b_]:> secondHead[b,a] /. ident->Identity;
			FCPrint[1, "Collect2: Done applying secondHead, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Putting res togehter.", FCDoControl->optVerbose];
		res = ((new + lin) /. holdForm2[ka_][j_] -> holdForm[ka[j]] /.	frx->Plus);
		FCPrint[1, "Collect2: Done putting res togehter, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		FCPrint[1, "Collect2: Done releasing tempIso, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[ccflag,
			res = res /. compCON -> ComplexConjugate
		];

		unity=1;

		res = (factorOut (res/denominator))/.monomRepRule;

		FCPrint[1, "Collect2: Leaving.", FCDoControl->optVerbose];
		FCPrint[3, "Collect2: Leaving with ", res, FCDoControl->optVerbose];

		res
	];

FCPrint[1, "Collect2 loaded"];
End[]
