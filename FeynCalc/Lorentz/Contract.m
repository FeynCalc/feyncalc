(* ::Package:: *)



(* :Title: Contract															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Contraction routines for Lorentz algebra					*)

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

cnVerbose::usage="";
optExpandScalarProduct::usage="";

FCFastContract[x_,OptionsPattern[]]:=
	x /. Pair -> PairContract /. PairContract -> Pair  /. CartesianPair -> CartesianPairContract /. CartesianPairContract -> CartesianPair;

Options[Contract] = {
	EpsContract     	-> True,
	EpsExpand    		-> True,
	Expanding       	-> True,
	ExpandScalarProduct -> True,
	Factoring       	-> False,
	FCE					-> False,
	FCI					-> False,
	FCVerbose			-> False,
	MomentumCombine 	-> True
};

(* Contract[{..., ...}] *)
Contract[expr_List, opts:OptionsPattern[]] :=
	Contract[#,opts]&/@expr;

(* Contract[... == ...] *)
Contract[Equal[a_, b_], opts:OptionsPattern[]] :=
	Contract[a, opts] == Contract[b, opts];


Contract[expr_, opts:OptionsPattern[]] :=
	Block[{ex, tmp, rest1=0,rest2=0,rest3=0,noDummy=0,nodot,
		null1,null2,freeIndList,freeHead,tmpFin,res,expandOpt,
		epsContractOpt, times,tmpList,time,tmpCheck, epsExpandOpt},

		expandOpt 				= OptionValue[Expanding];
		epsContractOpt 			= OptionValue[EpsContract];
		epsExpandOpt 			= OptionValue[EpsExpand];
		optExpandScalarProduct	= OptionValue[ExpandScalarProduct];

		If [OptionValue[FCVerbose]===False,
			cnVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				cnVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		(* At first we try to achieve maximal simplification without doing expansions *)

		FCPrint[1, "Contract: Entering main contract", FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Entering with", ex, FCDoControl->cnVerbose];

		If[	FreeQ2[ex, {CartesianIndex, LorentzIndex, Eps}],
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices from those that don't.", FCDoControl->cnVerbose];
		(* First splitting: Terms that do not need any contractions are not processed further	*)
		{rest1,tmp} = FCSplit[ex, {CartesianIndex, LorentzIndex, Eps},Expanding->False];
		(* TODO isolate terms without LorentzIndex and Eps*)
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[3, "Contract: Terms that contain LorentzIndex or Eps: ", tmp, FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Other terms: ", rest1, FCDoControl->cnVerbose];

		If[!FreeQ[tmp,PairContract],
			FCPrint[1,"Contract: Replacing PairContract with Pair.", FCDoControl->cnVerbose];
			rest1 = rest1 /. PairContract -> Pair;
			tmp = tmp /. PairContract -> Pair;
		];

		If[!FreeQ[tmp,CartesianPairContract],
			FCPrint[1,"Contract: Replacing CartesianPairContract with CartesianPair.", FCDoControl->cnVerbose];
			rest1 = rest1 /. CartesianPairContract -> CartesianPair;
			tmp = tmp /. CartesianPairContract -> CartesianPair;
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices with a FreeIndex DataType", FCDoControl->cnVerbose];
		freeIndList = Cases[res, _LorentzIndex, Infinity] // DeleteDuplicates // Sort //
			Cases[#, LorentzIndex[i_, ___] /; DataType[i, FreeIndex] :> {i, freeHead[i]}, Infinity] & // DeleteDuplicates // Sort;
		If[	freeIndList=!={},
			FCPrint[3, "Contract: List of indices that should not be summed over", freeIndList, FCDoControl->cnVerbose];
			tmp/. Map[(Rule @@ #) &, freeIndList];
			{tmp,rest2} = FCSplit[tmp, {freeHead},Expanding->False];
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[3, "Contract: Terms that contain Lorentz indices with a FreeIndex DataType: ", rest2, FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Other terms: ", tmp, FCDoControl->cnVerbose];

		(* Now we determine terms that contain dummy indices *)
		(* TODO Fish out the eps terms beforehand!!! *)
		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain dummy Lorentz indices", FCDoControl->cnVerbose];
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
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[3, "Contract: Terms that contain dummy Lorentz indices: ", tmp, FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Other terms: ", noDummy, FCDoControl->cnVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Applying DotSimplify where it is needed.", FCDoControl->cnVerbose];
		If[	!FreeQ[tmp, DOT],
			{nodot,tmp} = FCSplit[tmp, {DOT},Expanding->False];
			FCPrint[4, "Contract: Terms that contain DOT: ", tmp, FCDoControl->cnVerbose];
			FCPrint[4, "Contract: Other terms: ", nodot, FCDoControl->cnVerbose];
			tmp = nodot+DotSimplify[tmp, Expanding -> False]
		];

		(*	noDummy might still contain Epsilon tensors that we can simplify.
			This is the only reason why we still care about it. *)
		If[	!FreeQ[noDummy, DOT] && !FreeQ[noDummy,Eps],
			{nodot,noDummy} = FCSplit[noDummy, {DOT},Expanding->False];
			FCPrint[4, "Contract: Rest Terms that contain DOT: ", noDummy, FCDoControl->cnVerbose];
			FCPrint[4, "Contract: Rest Other terms: ", nodot, FCDoControl->cnVerbose];
			noDummy = nodot+DotSimplify[noDummy, Expanding -> False]
		];
		FCPrint[1,"Contract: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];


		FCPrint[3, "Contract: Terms to be processed further: ", tmp, FCDoControl->cnVerbose];

		If[ OptionValue[MomentumCombine] && LeafCount[tmp] < 1000,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying MometumCombine.", FCDoControl->cnVerbose];
			tmp =  MomentumCombine[tmp, FCI->True];
			FCPrint[1,"Contract: MometumCombine done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		FCPrint[3, "Contract: After MometumCombine: ", tmp, FCDoControl->cnVerbose];

		time=AbsoluteTime[];

		If[ !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying PairContract.", FCDoControl->cnVerbose];

				If[	TrueQ[optExpandScalarProduct],
					tmp = tmp /. Pair -> PairContract3 /. PairContract3 -> PairContract/. PairContract ->  Pair,
					tmp = tmp /. Pair -> PairContract/. PairContract ->  Pair
				];

				FCPrint[1,"Contract: mainContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
				FCPrint[3,"Contract: mainContract: After PairContract: ", tmp , FCDoControl->cnVerbose]
		];

		If[ !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],

			If[ MemberQ[{Plus, Times}, Head[tmp]] && !FreeQ2[tmp,{Pair,CartesianPair}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying prepareProductContractions.", FCDoControl->cnVerbose];
				tmp = prepareProductContractions[tmp];
				FCPrint[1,"Contract: mainContract: prepareProductContractions done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
				FCPrint[3,"Contract: mainContract: After prepareProductContractions: ", tmp , FCDoControl->cnVerbose];
				If[	!FreeQ2[tmp,{prepareProductContractions,reduceSumsToProducts,contractWithProductOfPairs,contractWithSinglePair}],
					Message[Contract::fail,"Something went wrong during prepareProductContractions."];
					Abort[]
				]
			]
		];

		If[ !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],

			If[ MemberQ[{Plus, Times}, Head[tmp]] && !FreeQ2[tmp,{Pair,CartesianPair}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying prepareProductContractions.", FCDoControl->cnVerbose];
				tmp = prepareProductContractions[tmp];
				FCPrint[1,"Contract: mainContract: prepareProductContractions done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
				FCPrint[3,"Contract: mainContract: After prepareProductContractions: ", tmp , FCDoControl->cnVerbose];
				If[	!FreeQ2[tmp,{prepareProductContractions,reduceSumsToProducts,contractWithProductOfPairs,contractWithSinglePair}],
					Message[Contract::fail,"Something went wrong during prepareProductContractions."];
					Abort[]
				]
			]
		];

		(*
		(* optimization *)
		If[ Head[tmp === Plus] && Length[tmp > 47],
			If[ !FreeQ[tmp, Eps],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying optimization.", FCDoControl->cnVerbose];
				tmp = tmp //. {
						Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
						Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_, D],d___] :> Eps[c,b,d]
				};
				FCPrint[1,"Contract: mainContract: Optimization done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
				FCPrint[3,"Contract: mainContract: After optimization: ", tmp , FCDoControl->cnVerbose]

			]
		];*)


		If[	!FreeQ[tmp,CartesianIndex],
			FCPrint[1,"Contract: Applying cartesianContract.", FCDoControl->cnVerbose];
			Which[ 	Head[tmp]===Plus,
					tmpFin = cartesianContract[#,opts]&/@tmp,

					Head[tmp]===Times,
					tmpFin = cartesianContract[tmp,opts],

					True,
					tmpFin = cartesianContract[tmp,opts]
			];
			FCPrint[1,"Contract: cartesianContract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
			FCPrint[3, "Contract: After cartesianContract: ", tmpFin, FCDoControl->cnVerbose],

			tmpFin = tmp
		];

		(* Here we are done *)


		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding w.r.t Lorentz and Cartesian indices.", FCDoControl->cnVerbose];
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

			FCPrint[1,"Contract: Expansion done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];


		If[	!FreeQ[tmpFin, Eps],
			FCPrint[1,"Contract: Applying EpsEvaluate.", FCDoControl->cnVerbose];
			tmpFin = EpsEvaluate[tmpFin,FCI->True, EpsExpand->epsExpandOpt];
			FCPrint[3,"Contract: After EpsEvaluate: ", tmpFin, FCDoControl->cnVerbose];
		];

		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Contracting epsilon tensors.", FCDoControl->cnVerbose];
			If[	!EpsContractFreeQ[tmpFin],
				tmpFin = EpsContract[tmpFin,FCI->True]
			];

			If[	!EpsContractFreeQ[noDummy],
				noDummy = EpsContract[noDummy,FCI->True]
			];
			FCPrint[1,"Contract: Epsilon contractions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding in Lorentz and Cartesian indices.", FCDoControl->cnVerbose];
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

			FCPrint[1,"Contract: Expansions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

			If[ epsContractOpt,
				time=AbsoluteTime[];
				FCPrint[1,"Contract: Expanding in Epsilon.", FCDoControl->cnVerbose];
				If[ !FreeQ[tmpFin, Eps],
					tmpFin = Expand[tmpFin, Eps]
				];
				If[ !FreeQ[noDummy, Eps],
					noDummy = Expand[noDummy, Eps]
				];
				FCPrint[1,"Contract: Expansions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose]
			];


		];

		If[	!FreeQ[tmpFin,Pair] && !DummyIndexFreeQ[tmpFin,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying PairContract.", FCDoControl->cnVerbose];
			If[	TrueQ[optExpandScalarProduct],
				tmpFin = tmpFin /. Pair->PairContract3 /. PairContract3 -> PairContract /.PairContract->Pair,
				tmpFin = tmpFin /. Pair-> PairContract /. PairContract->Pair;
			];
			FCPrint[1,"Contract: PairContract done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose]
		];

		If[	!FreeQ[tmpFin,CartesianPair] && !DummyIndexFreeQ[tmpFin,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying CartesianPairContract.", FCDoControl->cnVerbose];
			tmpFin = tmpFin /. CartesianPair-> CartesianPairContract /. CartesianPairContract -> CartesianPair;
			FCPrint[1,"Contract: CartesianPairContract done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];


		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying EpsEvaluate.", FCDoControl->cnVerbose];
			If[	!FreeQ[tmpFin, Eps],
				tmpFin = EpsEvaluate[tmpFin,FCI->True, EpsExpand->epsExpandOpt];
				FCPrint[3,"Contract: After EpsEvaluate: ", tmpFin, FCDoControl->cnVerbose];
			];
			If[	!FreeQ[noDummy, Eps],
				noDummy = EpsEvaluate[noDummy,FCI->True, EpsExpand->epsExpandOpt];
				FCPrint[3,"Contract: After EpsEvaluate: ", noDummy, FCDoControl->cnVerbose];
			];
			FCPrint[1,"Contract: EpsEvaluate done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		(*Here we can unite the two*)
		tmpFin = tmpFin + noDummy;

		FCPrint[3, "Contract: After additional manipulations: ", tmpFin, FCDoControl->cnVerbose];

		res = rest1+rest2+rest3+tmpFin;
		FCPrint[3, "Contract: Preliminary result: ", tmpFin, FCDoControl->cnVerbose];

		If[ OptionValue[Factoring]=!=False,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Factoring the result.", FCDoControl->cnVerbose];
			If[ OptionValue[Factoring] === True,
				res = Factor2[res],
				res = OptionValue[Factoring][res]
			];
			FCPrint[1,"Contract: Factoring done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "Contract: Leaving. ", FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Leaving with : ", res, FCDoControl->cnVerbose];
		res
	]/; Head[expr]=!=List;

(* #################################################################### *)


hasDummyIndices[expr_] :=
	(Cases[expr, (LorentzIndex|CartesianIndex)[i_, ___] :> i, Infinity,Heads->True]//Tally//Transpose//Last//Max//Greater[#, 1]&) /;
	!FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];

hasDummyIndices[expr_] := False/;
	FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];


cartesianContract[x : Except[_Plus], opts:OptionsPattern[]] :=
		Block[ { contractres = x, contractexpandopt, time},

			contractexpandopt = OptionValue[Contract,{opts},Expanding];

			FCPrint[3, "Contract: cartesianContract: Entering", FCDoControl->cnVerbose];
			FCPrint[4, "Contract: cartesianContract: Entering with ",x, FCDoControl->cnVerbose];

			If[	contractexpandopt,
				contractres = Expand2[contractres,CartesianIndex]
			];

			time=AbsoluteTime[];
			FCPrint[3, "Contract: cartesianContract: Applying PairContract.", FCDoControl->cnVerbose];
			contractres = contractres /. Pair -> PairContract /. CartesianPair -> CartesianPairContract /. PairContract->Pair /. CartesianPairContract->CartesianPair;
			FCPrint[3,"Contract: cartesianContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
			FCPrint[4,"Contract: cartesianContract: After PairContract: ", contractres , FCDoControl->cnVerbose];


			FCPrint[3,"Contract: cartesianContract: Leaving.", FCDoControl->cnVerbose];
			FCPrint[4,"Contract: cartesianContract: Leaving with: ", contractres , FCDoControl->cnVerbose];

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

prepareProductContractions[ex_ /; !MemberQ[{Times,Plus},Head[ex]]] :=
	ex;

prepareProductContractions[ex_Plus] :=
	Map[prepareProductContractions[#]&, ex];


(*The input expression is a product*)
prepareProductContractions[expr_Times] :=
	Block[{ex=expr, irrelevantPart, relevantPart, listOfProducts, time, res},

		FCPrint[3,"Contract: prepareProductContractions: Entering.", FCDoControl->cnVerbose];
		FCPrint[4,"Contract: prepareProductContractions: Entering with: ", expr, FCDoControl->cnVerbose];

		If[ !FreeQ2[ex, {DiracGamma, DiracChain, PauliSigma, PauliChain, Eps, Rule}],
			FCPrint[3,"Contract: prepareProductContractions: Expression contains DiracGamma, PauliSigma or Eps. Passing to Contract.", FCDoControl->cnVerbose];
			Return[ex]
		];

		If[	DummyIndexFreeQ[ex, {LorentzIndex, CartesianIndex}],
			FCPrint[3,"Contract: prepareProductContractions: Expression is free of Lorentz indices. Leaving.", FCDoControl->cnVerbose];
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[3,"Contract: prepareProductContractions: Splitting into parts free and not free of Lorentz indices.", FCDoControl->cnVerbose];
		{irrelevantPart,relevantPart} = FCProductSplit[ex,{LorentzIndex,CartesianIndex}];
		FCPrint[3,"Contract: prepareProductContractions: Splitting done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[4,"Contract: prepareProductContractions: Part free of Lorentz indices: ", irrelevantPart, FCDoControl->cnVerbose];
		FCPrint[4,"Contract: prepareProductContractions: Part with Lorentz indices: ", relevantPart, FCDoControl->cnVerbose];

		Switch[Head[relevantPart],
			Plus,
				time=AbsoluteTime[];
				FCPrint[3,"Contract: prepareProductContractions: The indexed part is a sum, applying prepareProductContractions again.", FCDoControl->cnVerbose];
				relevantPart = prepareProductContractions[relevantPart];
				FCPrint[3,"Contract: prepareProductContractions: Done applying prepareProductContractions. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose],
			Times,
				time=AbsoluteTime[];
				FCPrint[3,"Contract: prepareProductContractions: The indexed part is a product, applying reduceSumsToProducts.", FCDoControl->cnVerbose];
				FCPrint[3,"Contract: prepareProductContractions: Applying reduceSumsToProducts.", FCDoControl->cnVerbose];
				listOfProducts = List@@relevantPart;
				relevantPart = Fold[reduceSumsToProducts[#1,#2]&,listOfProducts];
				If[	optExpandScalarProduct,
					res = ExpandScalarProduct[relevantPart,FCI->True];
				];
				FCPrint[3,"Contract: prepareProductContractions: Done applying reduceSumsToProducts. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose],
			_,
				FCPrint[3,"Contract: prepareProductContractions: The indexed part is a single term or something else.", FCDoControl->cnVerbose];
				True
		];

		If[	!FreeQ2[relevantPart,{prepareProductContractions,reduceSumsToProducts}],
			Message[Contract::fail,"Something went wrong when applying reduceSumsToProducts or prepareProductContractions."];
			Abort[]
		];



		res = relevantPart irrelevantPart;
		FCPrint[3,"Contract: prepareProductContractions: Leaving.",  FCDoControl->cnVerbose];
		FCPrint[4,"Contract: prepareProductContractions: Leaving with: ", res, FCDoControl->cnVerbose];

		res
	];


(*
	To match the desired (A1+...An)(B1*B2*...Bn) pattern, we demand that
	the expression in the 1st slot must be more complicated than the one
	in the 2nd one.
*)
reduceSumsToProducts[a_,b_]:=
	reduceSumsToProducts[b, a]/;LeafCount[a]<LeafCount[b];

(*
	Here b still may be a sum.
	The function will recursively call itself until b is becomes a product of Pairs.
*)
reduceSumsToProducts[a_,b_]:=
	Block[{res, bnew},

		FCPrint[3, "Contract: reduceSumsToProducts: Entering.",  FCDoControl->cnVerbose];
		FCPrint[4, "Contract: reduceSumsToProducts: Entering with: {a,b}: ", {a,b}, FCDoControl->cnVerbose];

		If[FreeQ2[b,{LorentzIndex,CartesianIndex}],
			Return[a b]
		];

		If[ TrueQ[MatchQ[SelectNotFree[b, Pair,CartesianPair], HoldPattern[Times][__CartesianPair, __Pair] |
			HoldPattern[Times][__Pair, __CartesianPair] | HoldPattern[Times][__Pair] | HoldPattern[Times][__CartesianPairPair]]] || (Head[b]===Pair) || (Head[b]===CartesianPair),

			(*The simpler piece is just a product of Pairs times some irrelevant term *)
			FCPrint[3, "Contract: reduceSumsToProducts: b factorizes, calling contractWithProductOfPairs.", FCDoControl->cnVerbose];
			FCPrint[4, "Contract: reduceSumsToProducts: {a,b}:", {a,b}, FCDoControl->cnVerbose];
			res = contractWithProductOfPairs[a, b],

			(*
				b is a product, but not w.r.t Pairs, e.g. "x * (FV[p1, mu] + FV[p2, mu])(FV[p1, nu] + FV[p3, nu])"
				We need to collect this w.r.t LorentIndices, since this is what reduceSumsToProducts expects.
			*)
			bnew = Collect2[b, {LorentzIndex,CartesianIndex}];
			If[ Head[bnew] === Plus,
				(*If b does not factorize, use contractWithProductOfPairs*)
				FCPrint[4, "Contract: reduceSumsToProducts: b does not factorize, calling reduceSumsToProducts", FCDoControl->cnVerbose];
				res = Map[reduceSumsToProducts[a,#]&,bnew],

				(*
					If b factorizes, use contractWithProductOfPairs
					Think of sth like [aa ((D - 4) FV[p1, mu] + D FV[p1, mu])
				*)
				FCPrint[4, "Contract: reduceSumsToProducts: b factorizes, calling contractWithProductOfPairs.", FCDoControl->cnVerbose];
				res = contractWithProductOfPairs[a, bnew]

			]
		];

		FCPrint[4, "Contract: reduceSumsToProducts: result: ", res, FCDoControl->cnVerbose];

		If[	optExpandScalarProduct,
			res = ExpandScalarProduct[res,FCI->True];
		];

		If[ !FreeQ2[res, {LorentzIndex,CartesianIndex}],
			res = Expand2[res, {LorentzIndex,CartesianIndex}];
		];

		FCPrint[3, "Contract: reduceSumsToProducts: Leaving.", FCDoControl->cnVerbose];

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
