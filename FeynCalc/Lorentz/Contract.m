(* ::Package:: *)



(* :Title: Contract															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Contraction routines for Lorentz algebra

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

Contract::usage =
"Contract[expr] contracts pairs of Lorentz or Cartesian indices of metric
tensors, vectors and (depending on the value of the option EpsContract) of
Levi-Civita tensors in expr.

For contractions of Dirac matrices with each other use DiracSimplify.

Contract[exp1, exp2] contracts (exp1*exp2), where exp1 and exp2 may be larger
products of sums of metric tensors and 4-vectors. This can be also useful when
evaluating polarization sums, where exp2 should be the product (or expanded
sum) of the polarization sums for the vector bosons.";

Contract::fail =
"Error! Contract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Rename::usage =
"Rename is an option for Contract. If set to True, dummy indices in Eps are
renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
FCFastContract;
End[]


Begin["`Contract`Private`"]

prefactorIso::usage="";
optExpandScalarProduct::usage="";
optFactoring::usage="";
optTimeConstrained::usage="";

FCFastContract[x_,OptionsPattern[]]:=
	x /. Pair -> PairContract /. PairContract -> Pair  /. CartesianPair -> CartesianPairContract /. CartesianPairContract -> CartesianPair;

Options[Contract] = {
	EpsContract     	-> True,
	EpsExpand    		-> True,
	Expanding       	-> True,
	ExpandScalarProduct -> True,
	(*Factoring       	-> False,*)
	Factoring 			-> {Factor2, 5000},
	FCParallelize		-> False,
	FCE					-> False,
	FCI					-> False,
	FCVerbose			-> False,
	MomentumCombine 	-> True,
	TimeConstrained 	-> 3
};

Contract[expr_List, opts:OptionsPattern[]] :=
	Block[{optVerbose, res, time},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];
		time = AbsoluteTime[];

		FCPrint[1, "Contract: Entering.", FCDoControl->optVerbose];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
			FCPrint[1,"Contract: Applying Contract in parallel.", FCDoControl->optVerbose];

			res = ParallelMap[Contract[#, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr,
			DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
			FCPrint[1, "Contract: Done applying Contract in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			FCPrint[1,"Contract: Applying Contract.", FCDoControl->optVerbose];
			res = Map[Contract[#,FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr];
			FCPrint[1, "Contract: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		FCPrint[1, "Contract: Leaving.", FCDoControl->optVerbose];

		res
	];



(* Contract[... == ...] *)
Contract[Equal[a_, b_], opts:OptionsPattern[]] :=
	Contract[a, opts] == Contract[b, opts];


Contract[expr_/; Head[expr]=!=List, opts:OptionsPattern[]] :=
	Block[{ex, tmp, rest1=0,rest2=0,rest3=0,noDummy=0,nodot,
		null1,null2,freeIndList,freeHead,tmpFin,res,expandOpt,
		epsContractOpt, times,tmpList,time,tmpCheck, epsExpandOpt,
		optVerbose,factor},

		expandOpt 				= OptionValue[Expanding];
		epsContractOpt 			= OptionValue[EpsContract];
		epsExpandOpt 			= OptionValue[EpsExpand];
		optExpandScalarProduct	= OptionValue[ExpandScalarProduct];

		optFactoring 			= OptionValue[Factoring];
		optTimeConstrained		= OptionValue[TimeConstrained];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		(* At first we try to achieve maximal simplification without doing expansions *)

		FCPrint[1, "Contract: Entering main contract", FCDoControl->optVerbose];
		FCPrint[3, "Contract: Entering with", ex, FCDoControl->optVerbose];

		If[	FreeQ2[ex, {CartesianIndex, LorentzIndex, Eps}],
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices from those that don't.", FCDoControl->optVerbose];
		(* First splitting: Terms that do not need any contractions are not processed further	*)
		{rest1,tmp} = FCSplit[ex, {CartesianIndex, LorentzIndex, Eps},Expanding->False];
		(* TODO isolate terms without LorentzIndex and Eps*)
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		FCPrint[3, "Contract: Terms that contain LorentzIndex or Eps: ", tmp, FCDoControl->optVerbose];
		FCPrint[3, "Contract: Other terms: ", rest1, FCDoControl->optVerbose];

		If[!FreeQ[tmp,PairContract],
			FCPrint[1,"Contract: Replacing PairContract with Pair.", FCDoControl->optVerbose];
			rest1 = rest1 /. PairContract -> Pair;
			tmp = tmp /. PairContract -> Pair;
		];

		If[!FreeQ[tmp,CartesianPairContract],
			FCPrint[1,"Contract: Replacing CartesianPairContract with CartesianPair.", FCDoControl->optVerbose];
			rest1 = rest1 /. CartesianPairContract -> CartesianPair;
			tmp = tmp /. CartesianPairContract -> CartesianPair;
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices with a FreeIndex DataType", FCDoControl->optVerbose];
		freeIndList = Cases[res, _LorentzIndex, Infinity] // DeleteDuplicates // Sort //
			Cases[#, LorentzIndex[i_, ___] /; DataType[i, FreeIndex] :> {i, freeHead[i]}, Infinity] & // DeleteDuplicates // Sort;
		If[	freeIndList=!={},
			FCPrint[3, "Contract: List of indices that should not be summed over", freeIndList, FCDoControl->optVerbose];
			tmp/. Map[(Rule @@ #) &, freeIndList];
			{tmp,rest2} = FCSplit[tmp, {freeHead},Expanding->False];
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		FCPrint[3, "Contract: Terms that contain Lorentz indices with a FreeIndex DataType: ", rest2, FCDoControl->optVerbose];
		FCPrint[3, "Contract: Other terms: ", tmp, FCDoControl->optVerbose];

		(* Now we determine terms that contain dummy indices *)
		(* TODO Fish out the eps terms beforehand!!! *)
		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain dummy Lorentz indices", FCDoControl->optVerbose];
		tmpCheck = tmp;
		If[ !FreeQ[tmp,Power],
			tmp = tmp /. Power[a_, b_Integer?Positive] /; !FreeQ2[a, {CartesianIndex, LorentzIndex}] :> Apply[times, Table[a, {i, b}]];
		];

		If[ !DummyIndexFreeQ[tmp,{CartesianIndex,LorentzIndex}],

			tmpList = Apply[List,tmp+null1+null2];
			tmp = Select[tmpList, hasDummyIndices];
			noDummy = Complement[tmpList,tmp];

			(*noDummy contains epsilon tensors like LC[a,b,c,d]LC[e,f,g,h]*)
			tmp = Total[tmp]/.times->Times/.null1|null2->0;
			noDummy = Total[noDummy]/.times->Times/.null1|null2->0,

			If[ !FreeQ[tmp,times],
				tmp = tmp /.times->Times;
			];

			noDummy = tmp;
			tmp = 0
		];

		(* check that we didn't loose any terms *)
		If[tmp+noDummy =!=tmpCheck,
			Message[Contract::fail,"Extraction of dummy indices failed!"];
			Abort[]
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		FCPrint[3, "Contract: Terms that contain dummy Lorentz indices: ", tmp, FCDoControl->optVerbose];
		FCPrint[3, "Contract: Other terms: ", noDummy, FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Applying DotSimplify where it is needed.", FCDoControl->optVerbose];
		If[	!FreeQ[tmp, DOT],
			{nodot,tmp} = FCSplit[tmp, {DOT},Expanding->False];
			FCPrint[4, "Contract: Terms that contain DOT: ", tmp, FCDoControl->optVerbose];
			FCPrint[4, "Contract: Other terms: ", nodot, FCDoControl->optVerbose];
			tmp = nodot+DotSimplify[tmp, Expanding -> False]
		];

		(*	noDummy might still contain Epsilon tensors that we can simplify.
			This is the only reason why we still care about it. *)
		If[	!FreeQ[noDummy, DOT] && !FreeQ[noDummy,Eps],
			{nodot,noDummy} = FCSplit[noDummy, {DOT},Expanding->False];
			FCPrint[4, "Contract: Rest Terms that contain DOT: ", noDummy, FCDoControl->optVerbose];
			FCPrint[4, "Contract: Rest Other terms: ", nodot, FCDoControl->optVerbose];
			noDummy = nodot+DotSimplify[noDummy, Expanding -> False]
		];
		FCPrint[1,"Contract: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];


		FCPrint[3, "Contract: Terms to be processed further: ", tmp, FCDoControl->optVerbose];

		If[ OptionValue[MomentumCombine] && LeafCount[tmp] < 1000,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying MometumCombine.", FCDoControl->optVerbose];
			tmp =  MomentumCombine[tmp, FCI->True];
			FCPrint[1,"Contract: MometumCombine done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		];

		FCPrint[3, "Contract: After MometumCombine: ", tmp, FCDoControl->optVerbose];

		time=AbsoluteTime[];

		If[ !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying PairContract.", FCDoControl->optVerbose];

				If[	TrueQ[optExpandScalarProduct],
					tmp = tmp /. Pair -> PairContract3 /. PairContract3 -> PairContract/. PairContract ->  Pair,
					tmp = tmp /. Pair -> PairContract/. PairContract ->  Pair
				];

				FCPrint[1,"Contract: mainContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3,"Contract: mainContract: After PairContract: ", tmp , FCDoControl->optVerbose]
		];

		If[ !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],

			If[ MemberQ[{Plus, Times}, Head[tmp]] && !FreeQ2[tmp,{Pair,CartesianPair}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying prepareProductContractions.", FCDoControl->optVerbose];

				tmp = prepareProductContractions[tmp, optVerbose];
				FCPrint[1,"Contract: mainContract: prepareProductContractions done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3,"Contract: mainContract: After prepareProductContractions: ", tmp , FCDoControl->optVerbose];
				If[	!FreeQ2[tmp,{prepareProductContractions,reduceSumsToProducts,contractWithProductOfPairs,contractWithSinglePair}],
					Message[Contract::fail,"Something went wrong during prepareProductContractions."];
					Abort[]
				]
			]
		];

		If[	!FreeQ[tmp,CartesianIndex],
			FCPrint[1,"Contract: Applying cartesianContract.", FCDoControl->optVerbose];
			Which[ 	Head[tmp]===Plus,
					tmpFin = cartesianContract[#,opts]&/@tmp,

					Head[tmp]===Times,
					tmpFin = cartesianContract[tmp,opts],

					True,
					tmpFin = cartesianContract[tmp,opts]
			];
			FCPrint[1,"Contract: cartesianContract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "Contract: After cartesianContract: ", tmpFin, FCDoControl->optVerbose],

			tmpFin = tmp
		];

		(* Here we are done *)


		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding w.r.t Lorentz and Cartesian indices.", FCDoControl->optVerbose];
			If[!FreeQ[tmpFin, LorentzIndex],
				tmpFin = Expand[tmpFin, CartesianIndex]
			];
			If[!FreeQ[tmpFin, CartesianIndex],
				tmpFin = Expand[tmpFin, CartesianIndex]
			];

			If[!FreeQ[noDummy, LorentzIndex],
				noDummy = Expand[noDummy, LorentzIndex]
			];
			If[!FreeQ[noDummy, CartesianIndex],
				noDummy = Expand[noDummy, CartesianIndex]
			];

			FCPrint[1,"Contract: Expansion done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		];


		If[	!FreeQ[tmpFin, Eps],
			FCPrint[1,"Contract: Applying EpsEvaluate.", FCDoControl->optVerbose];
			tmpFin = EpsEvaluate[tmpFin,FCI->True, EpsExpand->epsExpandOpt];
			FCPrint[3,"Contract: After EpsEvaluate: ", tmpFin, FCDoControl->optVerbose];
		];

		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Contracting epsilon tensors.", FCDoControl->optVerbose];
			If[	!EpsContractFreeQ[tmpFin],
				tmpFin = EpsContract[tmpFin,FCI->True]
			];

			If[	!EpsContractFreeQ[noDummy],
				noDummy = EpsContract[noDummy,FCI->True]
			];
			FCPrint[1,"Contract: Epsilon contractions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		];

		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding in Lorentz and Cartesian indices.", FCDoControl->optVerbose];
			If[	!FreeQ[tmpFin, LorentzIndex],
				tmpFin = Expand[tmpFin, LorentzIndex]
			];
			If[!FreeQ[tmpFin, CartesianIndex],
				tmpFin = Expand[tmpFin, CartesianIndex]
			];

			If[	!FreeQ[noDummy, LorentzIndex],
				noDummy = Expand[noDummy,LorentzIndex]
			];
			If[!FreeQ[noDummy, CartesianIndex],
				noDummy = Expand[noDummy, CartesianIndex]
			];

			FCPrint[1,"Contract: Expansions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

			If[ epsContractOpt,
				time=AbsoluteTime[];
				FCPrint[1,"Contract: Expanding in Epsilon.", FCDoControl->optVerbose];
				If[ !FreeQ[tmpFin, Eps],
					tmpFin = Expand[tmpFin, Eps]
				];
				If[ !FreeQ[noDummy, Eps],
					noDummy = Expand[noDummy, Eps]
				];
				FCPrint[1,"Contract: Expansions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose]
			];


		];

		If[	!FreeQ[tmpFin,Pair] && !DummyIndexFreeQ[tmpFin,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying PairContract.", FCDoControl->optVerbose];
			If[	TrueQ[optExpandScalarProduct],
				tmpFin = tmpFin /. Pair->PairContract3 /. PairContract3 -> PairContract /.PairContract->Pair,
				tmpFin = tmpFin /. Pair-> PairContract /. PairContract->Pair;
			];
			FCPrint[1,"Contract: PairContract done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose]
		];

		If[	!FreeQ[tmpFin,CartesianPair] && !DummyIndexFreeQ[tmpFin,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying CartesianPairContract.", FCDoControl->optVerbose];
			tmpFin = tmpFin /. CartesianPair-> CartesianPairContract /. CartesianPairContract -> CartesianPair;
			FCPrint[1,"Contract: CartesianPairContract done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		];


		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying EpsEvaluate.", FCDoControl->optVerbose];
			If[	!FreeQ[tmpFin, Eps],
				tmpFin = EpsEvaluate[tmpFin,FCI->True, EpsExpand->epsExpandOpt];
				FCPrint[3,"Contract: After EpsEvaluate: ", tmpFin, FCDoControl->optVerbose];
			];
			If[	!FreeQ[noDummy, Eps],
				noDummy = EpsEvaluate[noDummy,FCI->True, EpsExpand->epsExpandOpt];
				FCPrint[3,"Contract: After EpsEvaluate: ", noDummy, FCDoControl->optVerbose];
			];
			FCPrint[1,"Contract: EpsEvaluate done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		];

		(*Here we can unite the two*)
		tmpFin = tmpFin + noDummy;

		FCPrint[3, "Contract: After additional manipulations: ", tmpFin, FCDoControl->optVerbose];

		res = rest1+rest2+rest3+tmpFin;
		FCPrint[3, "Contract: Preliminary result: ", tmpFin, FCDoControl->optVerbose];


		Switch[optFactoring,
			False,
				factor = Identity,
			True|Factor2,
				factor = Function[fuArg,TimeConstrained[Factor2[fuArg],optTimeConstrained,fuArg]],
			{_,_Integer},
				factor = Function[fuArg,
					If[	TrueQ[LeafCount[fuArg]<optFactoring[[2]]],
						TimeConstrained[(optFactoring[[1]])[fuArg],optTimeConstrained,fuArg],
						fuArg
					]
				],
			_,
				factor = optFactoring
		];


		time=AbsoluteTime[];
		FCPrint[1,"Contract: Factoring the result.", FCDoControl->optVerbose];
		res = factor[res];
		FCPrint[1,"Contract: Factoring done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];


		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "Contract: Leaving. ", FCDoControl->optVerbose];
		FCPrint[3, "Contract: Leaving with : ", res, FCDoControl->optVerbose];
		res
	];

(* #################################################################### *)


hasDummyIndices[expr_] :=
	(Cases[expr, (LorentzIndex|CartesianIndex)[i_, ___] :> i, Infinity,Heads->True]//Tally//Transpose//Last//Max//Greater[#, 1]&) /;
	!FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];

hasDummyIndices[expr_] := False/;
	FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];


cartesianContract[x : Except[_Plus], opts:OptionsPattern[]] :=
		Block[ { contractres = x, contractexpandopt, time, optVerbose},

			If [OptionValue[Contract,{opts},FCVerbose]===False,
				optVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[Contract,{opts},FCVerbose], _Integer],
					optVerbose=OptionValue[Contract,{opts},FCVerbose]
				];
			];

			contractexpandopt = OptionValue[Contract,{opts},Expanding];

			FCPrint[3, "Contract: cartesianContract: Entering", FCDoControl->optVerbose];
			FCPrint[4, "Contract: cartesianContract: Entering with ",x, FCDoControl->optVerbose];

			If[	contractexpandopt,
				contractres = Expand2[contractres,CartesianIndex]
			];

			time=AbsoluteTime[];
			FCPrint[3, "Contract: cartesianContract: Applying PairContract.", FCDoControl->optVerbose];
			contractres = contractres /. Pair -> PairContract /. CartesianPair -> CartesianPairContract /. PairContract->Pair /. CartesianPairContract->CartesianPair;
			FCPrint[3,"Contract: cartesianContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[4,"Contract: cartesianContract: After PairContract: ", contractres , FCDoControl->optVerbose];


			FCPrint[3,"Contract: cartesianContract: Leaving.", FCDoControl->optVerbose];
			FCPrint[4,"Contract: cartesianContract: Leaving with: ", contractres , FCDoControl->optVerbose];

			contractres
		];

(* #################################################################### *)

(*
	prepareProductContractions works only with expressions made of Pairs.
	The main idea is that given a large product Pairs, ideally
	(A1+...An)(B1*B2*...Bn), where (B1*B2*...Bn) are all Pairs
	sharing Lorentz indices with the first bracket, we can quickly
	replace the Lorentz index of say B1 inside the bracket with whatever
	is in the second slot of B1. Proceeding this way with the other Bs
	we can do the contractions much faster as compared to expanding everything.
	Obviously, this trick can work only for specific expressions. If Bs are
	not Pairs but e.g. Eps tensors or Dirac matirces, the trick won't work.
*)

(*prepareProductContractions -> prepareProductContractions *)

prepareProductContractions[ex_ /; !MemberQ[{Times,Plus},Head[ex]], _] :=
	ex;

prepareProductContractions[ex_Plus, optVerbose_] :=
	Map[prepareProductContractions[#,optVerbose]&, ex];


(*The input expression is a product*)
prepareProductContractions[expr_Times, optVerbose_] :=
	Block[{ex=expr, irrelevantPart, relevantPart, listOfProducts, time, res},

		FCPrint[1,"Contract: prepareProductContractions: Entering.", FCDoControl->optVerbose];
		FCPrint[3,"Contract: prepareProductContractions: Entering with: ", expr, FCDoControl->optVerbose];

		If[ !FreeQ2[ex, {DiracGamma, DiracChain, PauliSigma, PauliChain, Eps, Rule}],
			FCPrint[1,"Contract: prepareProductContractions: Expression contains DiracGamma, PauliSigma or Eps. Passing to Contract.", FCDoControl->optVerbose];
			Return[ex]
		];

		If[	DummyIndexFreeQ[ex, {LorentzIndex, CartesianIndex}],
			FCPrint[1,"Contract: prepareProductContractions: Expression is free of Lorentz indices. Leaving.", FCDoControl->optVerbose];
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: prepareProductContractions: Splitting into parts free and not free of Lorentz indices.", FCDoControl->optVerbose];
		{irrelevantPart,relevantPart} = FCProductSplit[ex,{LorentzIndex,CartesianIndex}];
		FCPrint[1,"Contract: prepareProductContractions: Splitting done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		FCPrint[3,"Contract: prepareProductContractions: Part free of Lorentz indices: ", irrelevantPart, FCDoControl->optVerbose];
		FCPrint[3,"Contract: prepareProductContractions: Part with Lorentz indices: ", relevantPart, FCDoControl->optVerbose];

		Switch[Head[relevantPart],
			Plus,
				time=AbsoluteTime[];
				FCPrint[1,"Contract: prepareProductContractions: The indexed part is a sum, applying prepareProductContractions again.", FCDoControl->optVerbose];
				relevantPart = prepareProductContractions[relevantPart, 0(*optVerbose*)];
				FCPrint[1,"Contract: prepareProductContractions: Done applying prepareProductContractions. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose],
			Times,

				FCPrint[1,"Contract: prepareProductContractions: The indexed part is a product, applying reduceSumsToProducts.", FCDoControl->optVerbose];

				time=AbsoluteTime[];
				FCPrint[1,"Contract: prepareProductContractions: Collecting w.r.t indices.", FCDoControl->optVerbose];
				listOfProducts = List@@relevantPart;


				listOfProducts = Collect2[listOfProducts,{LorentzIndex,CartesianIndex}, IsolateNames->prefactorIso,Factoring->optFactoring,
				TimeConstrained->optTimeConstrained];
				FCPrint[1,"Contract: prepareProductContractions: Done collecting, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[1,"Contract: prepareProductContractions: Applying reduceSumsToProducts.", FCDoControl->optVerbose];
				time=AbsoluteTime[];

				relevantPart = Fold[reduceSumsToProducts[Collect2[#1,{LorentzIndex,CartesianIndex}, IsolateNames->prefactorIso,Factoring->optFactoring,
				TimeConstrained->optTimeConstrained],#2,optVerbose]&,listOfProducts];
				FCPrint[1,"Contract: prepareProductContractions: Done applying reduceSumsToProducts. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

				time=AbsoluteTime[];
				FCPrint[1,"Contract: prepareProductContractions: Removing isolations.", FCDoControl->optVerbose];
				relevantPart = FRH[relevantPart, IsolateNames->prefactorIso];
				FCPrint[1,"Contract: prepareProductContractions: Done remvoing isolations, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];


				If[	optExpandScalarProduct,
					time=AbsoluteTime[];
					FCPrint[1,"Contract: prepareProductContractions: Applying ExpandScalarProduct.", FCDoControl->optVerbose];
					res = ExpandScalarProduct[relevantPart,FCI->True];
					FCPrint[1,"Contract: prepareProductContractions: ExpandScalarProduct done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				];
				,
			_,
				FCPrint[1,"Contract: prepareProductContractions: The indexed part is a single term or something else.", FCDoControl->optVerbose];
				True
		];

		If[	!FreeQ2[relevantPart,{prepareProductContractions,reduceSumsToProducts}],
			Message[Contract::fail,"Something went wrong when applying reduceSumsToProducts or prepareProductContractions."];
			Abort[]
		];



		res = relevantPart irrelevantPart;
		FCPrint[1,"Contract: prepareProductContractions: Leaving.",  FCDoControl->optVerbose];
		FCPrint[4,"Contract: prepareProductContractions: Leaving with: ", res, FCDoControl->optVerbose];

		res
	];


(*
	To match the desired (A1+...An)(B1*B2*...Bn) pattern, we demand that
	the expression in the 1st slot must be more complicated than the one
	in the 2nd one.
*)
reduceSumsToProducts[a_,b_, optVerbose_]:=
	reduceSumsToProducts[b, a, optVerbose]/;LeafCount[a]<LeafCount[b];

(*
	Here b still may be a sum.
	The function will recursively call itself until b is becomes a product of Pairs.
*)
reduceSumsToProducts[a_,b_, optVerbose_]:=
	Block[{res, bnew},

		FCPrint[3, "Contract: reduceSumsToProducts: Entering.",  FCDoControl->optVerbose];
		FCPrint[4, "Contract: reduceSumsToProducts: Entering with: {a,b}: ", {a,b}, FCDoControl->optVerbose];

		If[FreeQ2[b,{LorentzIndex,CartesianIndex}],
			Return[a b]
		];

		If[ TrueQ[MatchQ[SelectNotFree[b, Pair,CartesianPair], HoldPattern[Times][__CartesianPair, __Pair] |
			HoldPattern[Times][__Pair, __CartesianPair] | HoldPattern[Times][__Pair] | HoldPattern[Times][__CartesianPairPair]]] || (Head[b]===Pair) || (Head[b]===CartesianPair),

			(*The simpler piece is just a product of Pairs times some irrelevant term *)
			FCPrint[3, "Contract: reduceSumsToProducts: b factorizes, calling contractWithProductOfPairs.", FCDoControl->optVerbose];
			FCPrint[4, "Contract: reduceSumsToProducts: {a,b}:", {a,b}, FCDoControl->optVerbose];
			res = contractWithProductOfPairs[a, b],

			(*
				b is a product, but not w.r.t Pairs, e.g. "x * (FV[p1, mu] + FV[p2, mu])(FV[p1, nu] + FV[p3, nu])"
				We need to collect this w.r.t LorentIndices, since this is what reduceSumsToProducts expects.
			*)
			FCPrint[3, "Contract: reduceSumsToProducts: b doesn't factorize, calling Collect2.", FCDoControl->optVerbose];
			bnew = Collect2[b, {LorentzIndex,CartesianIndex}];

			If[ Head[bnew] === Plus,
				(*If b does not factorize, use contractWithProductOfPairs*)
				FCPrint[3, "Contract: reduceSumsToProducts: b still does not factorize, calling reduceSumsToProducts", FCDoControl->optVerbose];
				res = Map[reduceSumsToProducts[a,#,optVerbose]&,bnew],

				(*
					If b factorizes, use contractWithProductOfPairs
					Think of sth like [aa ((D - 4) FV[p1, mu] + D FV[p1, mu])
				*)
				FCPrint[3, "Contract: reduceSumsToProducts: b now factorizes, calling contractWithProductOfPairs.", FCDoControl->optVerbose];
				res = contractWithProductOfPairs[a, bnew]

			]
		];

		FCPrint[4, "Contract: reduceSumsToProducts: result: ", res, FCDoControl->optVerbose];

		If[	optExpandScalarProduct,
			res = ExpandScalarProduct[res,FCI->True];
		];

		If[ !FreeQ2[res, {LorentzIndex,CartesianIndex}],
			res = Expand2[res, {LorentzIndex,CartesianIndex}];
		];

		FCPrint[3, "Contract: reduceSumsToProducts: Leaving.", FCDoControl->optVerbose];

		res

	]/; LeafCount[a]>=LeafCount[b];


(* b is by definition a single Pair or a product thereof! *)
contractWithProductOfPairs[a_, b_Pair] :=
	contractWithSinglePair[a, b];

contractWithProductOfPairs[a_, b_CartesianPair] :=
	contractWithSinglePair[a, b];

contractWithProductOfPairs[a_, b_/;!MemberQ[{Pair,CartesianPair,Times},Head[b]]] :=
	a b;

contractWithProductOfPairs[a_, b_Times] :=
	(Fold[contractWithSinglePair,a, List@@b]/. Pair -> PairContract /. PairContract -> Pair)/; FreeQ2[{a,b},{CartesianPair,CartesianMomentum,CartesianIndex}];


contractWithProductOfPairs[a_, b_Times] :=
	(Fold[contractWithSinglePair,a, List@@b]/. Pair -> PairContract /. CartesianPair -> CartesianPairContract /.
	PairContract -> Pair /. CartesianPairContract -> CartesianPair)/; !FreeQ2[{a,b},{CartesianPair,CartesianMomentum,CartesianIndex}];

contractWithSinglePair[a_, pref_/;!MemberQ[{Pair, CartesianPair,Times},Head[pref]]] :=
	a pref;

contractWithSinglePair[a_, Pair[LorentzIndex[ind_, dim___], secondSlot_]] :=
	Expand2[a Pair[LorentzIndex[ind, dim], secondSlot], Pair]/; FreeQ[a, LorentzIndex[ind, ___]];

contractWithSinglePair[a_, CartesianPair[CartesianIndex[ind_, dim___], secondSlot_]] :=
	Expand2[a CartesianPair[CartesianIndex[ind, dim], secondSlot], CartesianPair]/; FreeQ[a, CartesianIndex[ind, ___]];

contractWithSinglePair[a_, Pair[LorentzIndex[ind_, dim___], (h:LorentzIndex|Momentum|ExplicitLorentzIndex)[x_, dim___]]] :=
	(a /. LorentzIndex[ind, ___] :> h[x, dim])/; !FreeQ[a, LorentzIndex[ind, ___]];

contractWithSinglePair[a_, Pair[LorentzIndex[ind_, dimL_:4], (h:CartesianIndex|CartesianMomentum)[x_, dimC_:3]]] :=
	(a /. LorentzIndex[ind, ___] :> h[x, dimC])/; !FreeQ[a, LorentzIndex[ind, ___]] && MatchQ[{dimL,dimC},{4,3}|{_Symbol,_Symbol-1}|{_Symbol-4,_Symbol-4}];

contractWithSinglePair[a_, CartesianPair[CartesianIndex[ind_, dim___], (h:CartesianIndex|CartesianMomentum)[x_, dim___]]] :=
	(a /. CartesianIndex[ind, ___] :> h[x, dim])/; !FreeQ[a, CartesianIndex[ind, ___]];

(*
	If dim1 and dim2 are different and this cannot resolved by Pair, then
	it is clearly to dangerous to do blind index replacements!
*)
contractWithSinglePair[a_, Pair[LorentzIndex[ind_, dim1___], (h:LorentzIndex|Momentum|ExplicitLorentzIndex)[x_, dim2___]]]/; dim1=!=dim2 :=
	Expand2[a Pair[LorentzIndex[ind, dim1], h[x, dim2]], Pair];

contractWithSinglePair[a_, Pair[CartesianIndex[ind_, dimL_:4], (h:CartesianIndex|CartesianMomentum)[x_, dimC_:3]]] :=
	Expand2[a CartesianPair[CartesianIndex[ind, dimL], h[x, dimC]], CartesianPair]/; MatchQ[{dimL,dimC},{4,3}|{_Symbol,_Symbol-1}|{_Symbol-4,_Symbol-4}];

(*for things like contractWithProductOfPairs[FV[k - p, mu] SP[k, p], FV[p, mu] SP[k, p]] *)
contractWithSinglePair[a_,b_]:=
	Expand2[a b, {Pair,CartesianPair}]/; FreeQ2[{a},{LorentzIndex,CartesianIndex}] || FreeQ2[{b},{LorentzIndex,CartesianIndex}]

FCPrint[1,"Contract.m loaded."];
End[]
