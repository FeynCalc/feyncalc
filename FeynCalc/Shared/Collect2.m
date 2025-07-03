(* ::Package:: *)



(* :Title: Collect2															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Extension of the Mathematica Collect						*)

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
	TimeConstrained 			-> Infinity
};

Collect2[a_ == b_, y__] :=
	Collect2[a,y] == Collect2[b,y];

Collect2[(h:Rule|RuleDelayed)[a_,b_], y__] :=
	With[{zz=Collect2[b,y]}, h[a,zz]];


Collect2[x_List, y__,  opts:OptionsPattern[]] :=
	Block[{res,time,cl2Verbose,optIsolateNames},

		If[OptionValue[FCVerbose]===False,
				cl2Verbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					cl2Verbose=OptionValue[FCVerbose]
				];
			];

		time=AbsoluteTime[];
		optIsolateNames = OptionValue[IsolateNames];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1, "Collect2: Applying Collect2 to a list in parallel." , FCDoControl->cl2Verbose];
				If[	optIsolateNames=!=False,
					If[	TrueQ[!(Head[optIsolateNames] === List && Length[optIsolateNames]===$KernelCount)],
						Message[Collect2::failmsg,"In the parallel mode, the option IsolateNames should be set to a list with the length being equal to the number of parallel kernels."];
						Abort[]
					];

					Table[With[{oin = optIsolateNames, ii = i},
					ParallelEvaluate[SetOptions[Collect2, IsolateNames -> oin[[ii]]];, DistributedContexts -> None]],{i,1,$KernelCount}];


				];

				With[{xxx = {y}, ooo = {opts}},
					ParallelEvaluate[FCParallelContext`Collect`pArgs = xxx; FCParallelContext`Collect`pOpts = FilterRules[ooo, Except[FCParallelize|FCVerbose|IsolateNames]];, DistributedContexts -> None]
				];

				res = ParallelMap[(Collect2[#,FCParallelContext`Collect`pArgs,FCParallelContext`Collect`pOpts, FCParallelize->False])&,x, DistributedContexts->None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[x]/$KernelCount]/10]];

				If[	optIsolateNames=!=False,
					Table[ParallelEvaluate[SetOptions[Collect2, IsolateNames -> False];, DistributedContexts -> None],{i,1,$KernelCount}];
				],
				FCPrint[1, "FCFeynmanPrepare: Applying Collect2 to a list.", FCDoControl->cl2Verbose];
				res = (Collect2[#, y, FilterRules[{opts}, Except[FCParallelize|FCVerbose]]]& /@ x)
		];

		FCPrint[1, "Collect2: Collecing done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];
		res
	];

Collect2[x_, y_, opts:OptionsPattern[]] :=
	Collect2[x, {y}, opts] /; (Head[y]=!=List && !OptionQ[y] && Head[x]=!=List && !OptionQ[x]);

Collect2[x_, z__, y_, opts:OptionsPattern[]] :=
	Collect2[x, {z,y}, opts] /; (Head[y]=!=List && !OptionQ[y] && Head[x]=!=List && !OptionQ[x]);

Collect2[expr_/; !MemberQ[{List,Equal},Head[expr]], vv_List/; (!OptionQ[vv] || vv==={}), opts:OptionsPattern[]] :=
	Block[{monomList,ru,nx,lk,factoring,optIsolateNames,tog,fr0,frx,lin,tv={},mp,monomialHead,cd,co,dde,
		new = 0, unity,re,compCON,ccflag = False, factor,expanding, times,time,time0,
		null1,null2,coeffArray,tvm,coeffHead,optIsolateFast,tempIso,factorOut, monomRepRule={},
		nonAtomicMonomials,optHead,firstHead,secondHead=Null,optInitialFunction,numerator,denominator,
		optNumerator, optFactoringDenominator, optTimeConstrained, ident, cl2Verbose,optFCParallelize, frh},

		If [OptionValue[FCVerbose]===False,
			cl2Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				cl2Verbose=OptionValue[FCVerbose]
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

		If[	optFCParallelize && optIsolateNames,
			Message[Collect2::failmsg,"In the parallel mode, when applied to a single expression, the option IsolateNames must be set to False."];
			Abort[]

		];

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

		FCPrint[1, "Collect2: Entering Collect2.", FCDoControl->cl2Verbose];
		FCPrint[2, "Collect2: Entering with: ", expr, FCDoControl->cl2Verbose];

		nx = expr;

		If[	!FreeQ2[nx,{SeriesData,ConditionalExpression}],
			Message[Collect2::failmsg,"Collect2 cannot work on expressions that contain SeriesData or ConditionalExpression!"];
			Abort[]
		];

		If[	optNumerator && Head[nx]===Times,
			numerator=Numerator[nx];
			If[ optFactoringDenominator===False,
				denominator=Denominator[nx],
				denominator=optFactoringDenominator[Denominator[nx]]
			];
			nx = numerator,
			denominator = 1
		];

		nx = nx /. OptionValue[IntermediateSubstitutions];

		nx = nx/factorOut;

		FCPrint[2,"Collect2: After factoring out ", factorOut, " : ", nx,  FCDoControl->cl2Verbose];

		FCPrint[1, "Collect2: Applying initial function.", FCDoControl->cl2Verbose];
		If[	Head[optInitialFunction]===List,
			nx = (Composition@@optInitialFunction)[nx],
			nx = optInitialFunction[nx]
		];
		FCPrint[3, "Collect2: After initial function ", nx,  FCDoControl->cl2Verbose];

		monomList = Union[Select[ vv, ((Head[#] =!= Plus) && (Head[#] =!= Times) && (!NumberQ[#]))& ]];
		monomList = Select[ monomList, !FreeQ[nx, #]&];

		(*If the monomials are not atomic, we should better mask them beforehand *)
		nonAtomicMonomials = Select[monomList, ! AtomQ[#] &];
		If[nonAtomicMonomials=!={} && FCPatternFreeQ[nonAtomicMonomials],
			monomRepRule = Thread[Rule[nonAtomicMonomials,Table[Unique["monom"], {r,1,Length[nonAtomicMonomials]} ]]];
			monomList = monomList/.monomRepRule;
			nx = nx/.monomRepRule;
			monomRepRule = Reverse/@monomRepRule
		];

		FCPrint[1, "Collect2: Monomials w.r.t which we will collect: ", monomList, FCDoControl->cl2Verbose];

		If[Length[monomList] === 0,
			FCPrint[1, "Collect2: The input expression contains no relevant monomials, leaving.", FCDoControl->cl2Verbose];
			unity = 1;
			re = factorOut factor[nx]/denominator;
			If[	optIsolateNames=!=False,
				re  = Isolate[re,IsolateNames -> optIsolateNames, IsolateFast-> optIsolateFast]
			];
			Return[re]
		];


		(* Hm, that's a problem, maybe *)
		If[!FreeQ[nx, ComplexConjugate],
			ccflag = True;
			nx = nx /. ComplexConjugate -> compCON;
			monomList = monomList /. ComplexConjugate -> compCON;
		];

		nx = nx/. holdForm[k_[ii_]] -> lk[k][ii];

		time=AbsoluteTime[];

		frh[x_] := FRH[x/.holdForm->Identity, IsolateNames->{optIsolateNames,tempIso}];

		If[ factoring === False,
			FCPrint[1, "Collect2: No factoring function defined.", FCDoControl->cl2Verbose];
			(* 	This can speed things up, if the expression contains very large sums free of
				monomials *)
			nx = nx /. Plus -> holdPlus /. holdPlus[x__] /; FreeQ2[{x}, monomList] :>
				Isolate[(Plus[x]/.holdPlus -> Plus), IsolateFast -> True, IsolateNames -> tempIso] /. holdPlus -> Plus;
			tog[x_] := frh[x],


			FCPrint[1, "Collect2: Factoring function is ", factor, FCDoControl->cl2Verbose];
			fr0[x__] :=
				Plus[x] /; !FreeQ2[{x}, monomList];
			tog[x_]  :=
				factor[frh[x]];
			frx[x__] :=
				holdForm[Plus[x]];
			nx = nx /. Plus -> fr0 /. fr0 -> frx
		];


		If[ expanding =!= False,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Expanding", FCDoControl->cl2Verbose];
			nx  = Expand2[nx,monomList];
			FCPrint[1, "Collect2: Expanding done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Separating the part free of the monomials (linear part)", FCDoControl->cl2Verbose];
		(* lin denotes the part free of monomList *)
		{lin,nx} = FCSplit[nx,monomList,Expanding->False];
		FCPrint[1, "Collect2: Separation done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];
		FCPrint[3, "Collect2: Part that contains the monomials: ", nx, FCDoControl->cl2Verbose];
		FCPrint[3, "Collect2: Linear part: ", lin, FCDoControl->cl2Verbose];

		If[factoring =!= False && lin=!=0,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Factoring the linear part", FCDoControl->cl2Verbose];
			lin = tog[lin];
			FCPrint[1, "Collect2: Factoring done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];

		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Wrapping the momomials with special heads.", FCDoControl->cl2Verbose];

		nx = (Map[(SelectFree[#, monomList] monomialHead[SelectNotFree[#, monomList]]) &,
				nx + null1 + null2] /. {null1 | null2 -> 0}) ;
		tv = Cases2[nx,monomialHead];
		FCPrint[1, "Collect2: Wrapping done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cl2Verbose];

		FCPrint[3, "Collect2: nx: ", nx , FCDoControl->cl2Verbose];
		FCPrint[3, "Collect2: tv: ", tv , FCDoControl->cl2Verbose];

		If[dde === True,
			FCPrint[1, "Collect2: Also denominators containing variables will be collected", FCDoControl->cl2Verbose];
			cd[x_] := ((Numerator[#]/(factor[Denominator[#]] /.
			Plus-> (Collect2[Plus[##], monomList, opts]&)))& @ x ) /;
			(!FreeQ[Denominator[x], Plus]) && (!FreeQ2[Denominator[x], monomList])
		];

		FCPrint[1, "Collect2: There are ", Length[tv] , " momomials to collect", FCDoControl->cl2Verbose];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Computing CoefficientArrays.", FCDoControl->cl2Verbose];
		coeffArray = CoefficientArrays[nx,tv];
		FCPrint[1, "Collect2: CoefficientArrays ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];
		FCPrint[3, "Collect2: CoefficientArrays: ", coeffArray, FCDoControl->cl2Verbose];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Collecting the monomials.", FCDoControl->cl2Verbose];

		tvm = (frh[#] /. monomialHead -> cd /. cd -> firstHead)&/@tv;

		FCPrint[3, "Collect2: tvm: ", tvm, FCDoControl->cl2Verbose];

		If[	coeffArray[[1]]=!=0,
			Message[Collect2::failmsg,"There is another linear part!"];
			Abort[]
		];

		FCPrint[1, "Collect2: Done collecting the monomials, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];
		(*FCPrint[3, "Collect2: prelminiary new: ", new, FCDoControl->cl2Verbose];*)
		time=AbsoluteTime[];


		If[	optIsolateNames===False,
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1, "Collect2: Factoring the coefficients of the monomials in parallel.", FCDoControl->cl2Verbose];
				With[{xxx = unity, yyy=factor},
					ParallelEvaluate[FCParallelContext`Collect2`unity = xxx; FCParallelContext`Collect2`factor = yyy;, DistributedContexts -> None]
				];
				DistributeDefinitions[factor,factoring,optTimeConstrained];

				coeffHead[li_SparseArray]:=
					Block[{tmp,res},
						tmp = frh/@Normal[li];
						res = ParallelMap[factor[FCParallelContext`Collect2`unity*#]&,tmp,
						DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[tmp]/$KernelCount]/10]];
						res
				],

				FCPrint[1, "Collect2: Factoring the coefficients of the monomials (no isolation).", FCDoControl->cl2Verbose];
				coeffHead[li_SparseArray]:=
					tog[unity*#]&/@li
			],

			FCPrint[1, "Collect2: Factoring the coefficients of the monomials (with isolation).", FCDoControl->cl2Verbose];
			coeffHead[li_SparseArray]:=
				Isolate[tog[unity*#]/. {unity:>1, lk[ka_][j_] :> holdForm[ka[j]]},
					IsolateNames -> optIsolateNames, IsolateFast-> optIsolateFast]&/@li;
		];

		new =  Sum[dotHold[coeffHead[coeffArray[[i]]] , Sequence @@ Table[tvm, {i - 1}]], {i, 2, Length[coeffArray]}];

		FCPrint[1, "Collect2: Done factoring the coefficients of the monomials, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];


		new = new /. dotHold-> Dot;

		FCPrint[3, "Collect2: new: ", new, FCDoControl->cl2Verbose];

		If[	!FreeQ2[lin,monomList],
			Message[Collect2::failmsg,"Linear part contains monomials!"];
			Print[lin];
			Abort[]
		];

		time=AbsoluteTime[];

		FCPrint[1, "Collect2: Releasing tempIso.", FCDoControl->cl2Verbose];
		If[ optIsolateNames =!= False,
			lin = Isolate[frh[lin], IsolateNames->optIsolateNames, IsolateFast->optIsolateFast],
			lin = frh[lin/.holdForm->Identity]
		];
		FCPrint[1, "Collect2: Done releasing tempIso, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];

		If[	secondHead=!=Null,
			time=AbsoluteTime[];
			FCPrint[1, "Collect2: Applying secondHead.", FCDoControl->cl2Verbose];
			lin = secondHead[lin,1] /. secondHead[0,_] -> 0;
			new = secondHead/@(new + null1 + null2) /. secondHead[null1|null2]->0 /. secondHead[a_firstHead b_]:> secondHead[b,a] /. ident->Identity;
			FCPrint[1, "Collect2: Done applying secondHead, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];
		];

		time=AbsoluteTime[];
		FCPrint[1, "Collect2: Putting re togehter.", FCDoControl->cl2Verbose];
		re = ((new + lin) /. lk[ka_][j_] -> holdForm[ka[j]] /.	frx->Plus);
		FCPrint[1, "Collect2: Done putting re togehter, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];


		(*	(*Just a small consistency check *)
		If[	optIsolateNames =!= False,
			If[	!FreeQ2[FRH[Cases[re,_HoldForm,Infinity]//Sort//DeleteDuplicates,IsolateNames->{optIsolateNames}], monomList],
				Message[Collect2::failmsg,"Isolated prefactors contain monomials!"];
				Abort[]
			]
		];*)


		FCPrint[1, "Collect2: Done releasing tempIso, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cl2Verbose];

		If[ccflag,
			re = re /. compCON -> ComplexConjugate
		];

		unity=1;

		re = (factorOut (re/denominator))/.monomRepRule;

		FCPrint[1, "Collect2: Leaving.", FCDoControl->cl2Verbose];
		FCPrint[3, "Collect2: Leaving with", re, FCDoControl->cl2Verbose];

		re
	];

FCPrint[1, "Collect2 loaded"];
End[]
