(* ::Package:: *)



(* :Title: Contract															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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
contract2;
contract3;
End[]


Begin["`Contract`Private`"]

cnVerbose::usage="";
optExpandScalarProduct::usage="";

FCFastContract[x_,OptionsPattern[]]:=
	x /. Pair -> PairContract /. PairContract -> Pair  /. CartesianPair -> CartesianPairContract /. CartesianPairContract -> CartesianPair;


contract3[x_Plus, opts:OptionsPattern[]] :=
	Map[contract3[#,opts]&, x];

contract3[x_ /; (Head[x] =!= Times) && Head[x] =!= Plus,opts:OptionsPattern[]] :=
	Contract[x, contract3->False,opts];

contract3[expr_Times, opts:OptionsPattern[]] :=
	Block[{ex, nonli, lipa, nec = 0, ic,epli, time, res, time2},


		If[	!OptionValue[Contract,{opts},FCI],
			ex = FCI[expr],
			ex = expr
		];


		FCPrint[1,"Contract: contract3: Entering.", FCDoControl->cnVerbose];
		FCPrint[3,"Contract: contract3: Entering with: ", expr, FCDoControl->cnVerbose];

		If[ !FreeQ2[ex, {DiracGamma,DiracChain,PauliSigma,PauliChain,Eps}],
			FCPrint[1,"Contract: contract3: Expression contains DiracGamma, PauliSigma or Eps. Passing to Contract.", FCDoControl->cnVerbose];
			Return[Contract[ex, FCI->True, contract3 -> False,opts]]
		];

		If[ FreeQ[ex, LorentzIndex],
			FCPrint[1,"Contract: contract3: Expression is free of Lorentz indices. Leaving.", FCDoControl->cnVerbose];
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: contract3: Applying Contract without expansions.", FCDoControl->cnVerbose];
		ex = Contract[ex, FCI->True, Expanding -> False, contract3->False, opts];
		FCPrint[1,"Contract: contract3: Contract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		If[ Head[ex] =!= Times,
			FCPrint[1,"Contract: contract3: Expression is not in the factorized form. Passing to Contract.", FCDoControl->cnVerbose];
			Return[Contract[ex, FCI->True, contract3->False]]
		];


		time=AbsoluteTime[];
		FCPrint[1,"Contract: contract3: Splitting into parts free and not free of Lorentz indices.", FCDoControl->cnVerbose];
		nonli = Select[ex, FreeQ[#, LorentzIndex]&];
		lipa  = Select[ex,!FreeQ[#, LorentzIndex]&];

		If[	Factor[nonli lipa - ex]=!=0,
			Message[Contract::fail, "Splitting of the factorized expression in contract3 failed."];
			FCPrint[0,{nonli,lipa,ex}];
			Abort[]
		];
		FCPrint[1,"Contract: contract3: Splitting done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[3,"Contract: contract3: Part free of Lorentz indices: ", nonli, FCDoControl->cnVerbose];
		FCPrint[3,"Contract: contract3: Part with Lorentz indices: ", lipa, FCDoControl->cnVerbose];



		If[ Head[lipa] =!= Times,
			FCPrint[1,"Contract: contract3: Part with Lorentz indices is not factorized!", FCDoControl->cnVerbose];
			If[ Head[lipa] === Plus,
				FCPrint[1,"Contract: contract3: Part with Lorentz indices is a sum. Applying contract3 again.", FCDoControl->cnVerbose];
				nec = contract3[lipa, opts],
				FCPrint[1,"Contract: contract3: Part with Lorentz indices is not a sum. Passing to Contract.", FCDoControl->cnVerbose];
				nec = Contract[lipa, FCI->True, contract3->False,opts]
			];

			res = nec nonli;
			FCPrint[1,"Contract: contract3: Leaving.", FCDoControl->cnVerbose];
			FCPrint[3,"Contract: contract3: Leaving with: ",res, FCDoControl->cnVerbose];
			Return[res]
		];


		If[ Length[lipa] < 2,
			time2=AbsoluteTime[];
			FCPrint[1,"Contract: contract3: Applying Contract.", FCDoControl->cnVerbose];
			nec = Contract[lipa, FCI->True, contract3->False, opts];
			FCPrint[1,"Contract: contract3: Contract done. Timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->cnVerbose],



			(* The expression is of the form A1*A2*A3*...*An, where each factor contains Lorentz indices.*)
			time2=AbsoluteTime[];
			FCPrint[1,"Contract: contract3: Applying contractProduct.", FCDoControl->cnVerbose];

			nec = lipa[[1]];
			(* nec = A1 *)
			For[ic = 2, ic <= Length[lipa], ic++,
				FCPrint[1,"ic = ", ic, " out of ",Length[lipa]];
				(*Print[{lipa[[ic]],nec}];*)
				(* if A1 is simpler than Ai, *)
				If[ LeafCount[nec] < LeafCount[lipa[[ic]]] || !FreeQ[lipa[[ic]], Twist2GluonOperator],
					nec = contractProduct[lipa[[ic]], nec, contract3->False,opts],
					nec = contractProduct[nec, lipa[[ic]], contract3->False,opts]
				];


				If[	optExpandScalarProduct,
					FCPrint[2,"expand scalar products"];
					nec = ExpandScalarProduct[nec,FCI->False];
					FCPrint[2,"expand scalar products done"]
				];

				If[ !FreeQ[nec, LorentzIndex],
					FCPrint[2,"expanding LorentzIndex now"];
					time = TimeUsed[];
					nec = Expand[nec, LorentzIndex];
					FCPrint[2,"expanding LorentzIndex DONE ",
							TimeUsed[] - time];
				];
			];
			FCPrint[1,"Contract: contract3: contractProduct done. Timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->cnVerbose]
		];

		nec = nec nonli;

		nec
	];

(* #################################################################### *)

Options[contract2] = {
	Collecting -> False
};

(* bb is assumed to be collected w.r.t. to LorentzIndex !!! *)
contract2[a_, bb_, ops___Rule] :=
	Block[ {sel, ct, rc, lco, lct, lastct, nop, b = bb, col, conT},
		col = Collecting /. {ops} /. Options[contract2];
		If[ Head[a] =!= Times,
			rc = contractProduct[a, b],
			lco[x_,y_] :=
				If[ Length[x]>Length[y],
					True,
					False
				];
			sel = Select[a, FreeQ[#, LorentzIndex]&];
			ct  = a/sel;
			nop = Select[ct, FreeQ[#, Plus]&];
			ct = ct/nop;
			If[ Head[ct] =!= Times,
				rc = sel contractProduct[ct nop, b],
				ct = Sort[List @@ ct, lco];
				If[ nop =!= 1,
					lastct = contractLongShort[b, nop, ops],
					lastct = b nop
				];
				lct = Length[ct];
				If[ lct === 1,
					rc = sel contractLongLong[ct[[1]], lastct]
				];
				If[ lct > 1,
					rc = sel contractProduct[Times @@ Take[ct, lct-1],
									ct[[lct]], lastct ]
				];
			];
		];
		FCPrint[2,"lct = ",lct];
		If[ !FreeQ[rc, LorentzIndex],
			rc = Contract[rc, Expanding -> False];
		];
		If[ !FreeQ[rc, LorentzIndex],
			FCPrint[1,"contracting agagin at the end of contract2."];
			rc = Contract[rc]
		];
		rc
	];

contract2[a_] :=
	Block[ {sel, ct, rc, lco, lct, lastct, nop},
		If[ Head[a] =!= Times,
			rc = Contract[a],
			lco[x_,y_] :=
				If[ Length[x]>Length[y],
					True,
					False
				];
			sel = Select[a, FreeQ[#, LorentzIndex]&];
			ct  = a/sel;
			nop = Select[ct, FreeQ[#, Plus]&];
			ct = ct/nop;
			If[ Head[ct] =!= Times,
				rc = sel Contract[ct nop],
				ct = Sort[List @@ ct, lco];
				If[ nop =!= 1,
					lastct = contractLongShort[Last[ct], nop],
					lastct = Last[ct] nop;
				];
				lct = Length[ct];
				If[ lct === 2,
					rc = sel contractLongLong[ct[[1]], lastct]
				];
				If[ lct > 2,
					rc = sel contractProduct[Times @@ Take[ct, lct-2], ct[[lct-1]], lastct]
				];
			];
		];
		FCPrint[2,"lct = ",lct];
		If[ !FreeQ[rc, LorentzIndex],
			FCPrint[1,"contracting agagin at the end of contract2 "];
			rc = Contract[rc]
		];
		rc
	];

(* ******************************************************************** *)


(* Added 3/11-2002 to contract also denominators. F.Orellana.
	Unfortunately it slows down things, so we might want to add an option
	to disble it...*)
(* This seems artificial.
	It can never occur by normal Feynman rule application.
	If anybody needs it they should copy this functions and call it differently.
	Commented out September 16th 2003 by Rolf Mertig, in order to not slow
	down things. Contract is a very vital function ...

Contract[x__, opts___Rule] := (Contract[x /. Times[a___, b : Pair[_, __]^-1, c___] :>
	inv[(1/Times @@ Select[{a, b, c}, MatchQ[#, _^-1] &])](Times @@
			Select[{a, b, c}, ! MatchQ[#, _^-1] &]), opts] /. inv -> ((1/#)&))/;
	!FreeQ[{x}, _Pair^-1];
*)


Options[Contract] = {
	Collecting      	-> True,
	contract3       	-> True,
	EpsContract     	-> True,
	EpsExpand    		-> True,
	Expanding       	-> True,
	ExpandScalarProduct -> True,
	Factoring       	-> False,
	FCE					-> False,
	FCI					-> False,
	FCVerbose			-> False,
	MomentumCombine 	-> True,
	Rename          	-> False,
	Schouten        	-> 0
};

Contract[l_List, opts:OptionsPattern[]] :=
	Contract[#,opts]&/@l;

(*	Main entry point	*)
Contract[expr_, z:OptionsPattern[]] :=
	Block[{ex, tmp, rest1=0,rest2=0,rest3=0,noDummy=0,nodot,
		null1,null2,freeIndList,freeHead,tmpFin,res,expandOpt,
		epsContractOpt,renameOpt,schoutenOpt,times,tmpList,time,tmpCheck, epsExpandOpt},

		expandOpt 		= OptionValue[Expanding];
		epsContractOpt 	= OptionValue[EpsContract];
		epsExpandOpt 	= OptionValue[EpsExpand];
		renameOpt 		= OptionValue[Rename];
		schoutenOpt 	= OptionValue[Schouten];
		optExpandScalarProduct = OptionValue[ExpandScalarProduct];

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

		(*
			Now we should choose the best strategy to evaluate the expression in tmp.
			Essentially, it can be a sum, a product or a standalone term. If it is a sum
			we evaluate it term by term. The evaluation function is also applicable to
			products or standalone terms.
		*)

		If[	!FreeQ[tmp,LorentzIndex],
			FCPrint[1,"Contract: Applying mainContract.", FCDoControl->cnVerbose];
			Which[ 	Head[tmp]===Plus,
					tmp = mainContract[#,z]&/@tmp,

					Head[tmp]===Times,
					tmp = mainContract[tmp,z],

					True,
					tmp = mainContract[tmp,z]
			];
			FCPrint[1,"Contract: mainContract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
			FCPrint[3, "Contract: After mainContract: ", tmp, FCDoControl->cnVerbose]
		];

		If[	!FreeQ[tmp,CartesianIndex],
			FCPrint[1,"Contract: Applying cartesianContract.", FCDoControl->cnVerbose];
			Which[ 	Head[tmp]===Plus,
					tmpFin = cartesianContract[#,z]&/@tmp,

					Head[tmp]===Times,
					tmpFin = cartesianContract[tmp,z],

					True,
					tmpFin = cartesianContract[tmp,z]
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

		If[	renameOpt && !FreeQ[tmpFin, Eps],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Renaming dummy indices in epsilon tensors.", FCDoControl->cnVerbose];
			tmpFin = doubleindex[Expand2[ EpsEvaluate[tmpFin,FCI->True, EpsExpand->epsExpandOpt], Eps]];
			FCPrint[1,"Contract: Renaming done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
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
				tmpFin = tmpFin /. Pair->FeynCalc`Package`pairContract3NoExpand /. FeynCalc`Package`pairContract3NoExpand -> PairContract /.PairContract->Pair;
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

		If[ schoutenOpt =!= 0 && epsContractOpt && !FreeQ[tmpFin, Eps] && !FreeQ[tmpFin, Pair],
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying Schouten's identity.", FCDoControl->cnVerbose];
			tmpFin = Schouten[tmpFin, schoutenOpt];
			If[ renameOpt,
				tmpFin = doubleindex[tmpFin]
			];
			FCPrint[1,"Contract: Schouten done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

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

(* Contract[A1==An] *)
Contract[Equal[a_, b_], opts:OptionsPattern[]] :=
	Contract[a, FCI->True, opts] == Contract[b, FCI->True, opts];

(* Contract[A1,A2,A3] *)
contractProduct[a_, b: Except[_Rule], c: Except[_Rule], ops:OptionsPattern[]] :=
	contractProduct[contractProduct[b, c, ops], a, ops];

contractProduct[x_, y_ /; Head[y]=!=Rule, opts:OptionsPattern[]] :=
	(y Contract[x, FCI->True, opts]) /; FreeQ2[y, {LorentzIndex,Eps}];

(* Contract[A1,A2*...*An]
contractLongShort is for contracting products of products,
contractLongLong is for contacting products of sums!
 *)
contractProduct[a_, b_Times, opts:OptionsPattern[]] :=
	Block[ {bb},
		If[ MatchQ[b, Apply[HoldPattern, {Times__Pair}]],
			contractLongShort[ a, b ,opts],
			If[ MatchQ[b, HoldPattern[Times__Pair]],
				contractLongShort[ a, b ,opts],
				bb = Collect3[b, Pair, Factoring-> False];
				If[ Head[bb] === Plus,
					contractLongLong[a, bb],
					contractLongShort[a, bb, opts]
				]
			]
		]
	];


contractProduct[a_, b: Except[_Times | _Plus | _Rule], opts:OptionsPattern[]] :=
	Contract[a b, FCI->True, opts];

(* Contract[X,A1+...+An] *)
contractProduct[a_, b_Plus, opts:OptionsPattern[]] :=
	If[ OptionValue[Contract,{opts},Collecting],
		contractLongLong[a,
			If[ FreeQ[List@@b, Plus],
				b,
				Collect2[b, LorentzIndex]
			]
		],
		contractLongLong[a, b]
	];

hasDummyIndices[expr_] :=
	(Cases[expr, (LorentzIndex|CartesianIndex)[i_, ___] :> i, Infinity,Heads->True]//Tally//Transpose//Last//Max//Greater[#, 1]&) /;
	!FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];

hasDummyIndices[expr_] := False/;
	FreeQ2[{expr}, {LorentzIndex,CartesianIndex}];

mainContract[x : Except[_Plus], opts:OptionsPattern[]] :=
		Block[ { contractres = x,
				contractexpandopt, es, time,
				optContract3, schout, contractfactoring },

			contractexpandopt = OptionValue[Contract,{opts},Expanding];
			contractfactoring = OptionValue[Contract,{opts},Factoring];
			optContract3 = OptionValue[Contract,{opts},contract3];

			FCPrint[1, "Contract: mainContract: Entering", FCDoControl->cnVerbose];
			FCPrint[3, "Contract: mainContract: Entering with ",x, FCDoControl->cnVerbose];


			(* NEW: September 16th 2003: adding contract3 directly here ... *)
			If[ optContract3,
				If[ MemberQ[{Plus, Times}, Head[contractres]],
					time=AbsoluteTime[];
					FCPrint[1, "Contract: mainContract: Applying contract3.", FCDoControl->cnVerbose];
					contractres = contract3[contractres,FCI->True,opts];
					FCPrint[1,"Contract: mainContract: contract3 done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
					FCPrint[3,"Contract: mainContract: After contract3: ", contractres , FCDoControl->cnVerbose]
				]
			];


			If[ !DummyIndexFreeQ[contractres,{LorentzIndex,CartesianIndex}],
				time=AbsoluteTime[];
				FCPrint[1, "Contract: mainContract: Applying PairContract.", FCDoControl->cnVerbose];

				If[	TrueQ[optExpandScalarProduct],
					contractres = contractres /. Pair -> PairContract3 /. PairContract3 -> PairContract/.
								PairContract -> sceins /. sceins -> Pair,
					contractres = contractres /. Pair -> FeynCalc`Package`pairContract3NoExpand /. FeynCalc`Package`pairContract3NoExpand -> PairContract/.
								PairContract -> sceins /. sceins -> Pair
				];

				FCPrint[1,"Contract: mainContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
				FCPrint[3,"Contract: mainContract: After PairContract: ", contractres , FCDoControl->cnVerbose]
			];

			(* optimization *)
			If[ Head[contractres === Plus] && Length[contractres > 47],
				If[ !FreeQ[contractres, Eps],
					time=AbsoluteTime[];
					FCPrint[1, "Contract: mainContract: Applying optimization.", FCDoControl->cnVerbose];
					es = {	Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
							Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_, D],d___] :> Eps[c,b,d]
					};
					contractres = contractres //. es;
					FCPrint[1,"Contract: mainContract: Optimization done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
					FCPrint[3,"Contract: mainContract: After optimization: ", contractres , FCDoControl->cnVerbose]

				]
			];


			time=AbsoluteTime[];
			FCPrint[1, "Contract: mainContract: Applying another optimization.", FCDoControl->cnVerbose];

			If[ contractexpandopt && !optContract3 && !FreeQ[contractres, LorentzIndex],
				(* NEW October 2003, this speeds things up in general*)
				contractres = Expand[Expand[contractres, LorentzIndex] //. simplerules];
				contractres = contractres /.
								{((yy_Plus)  /;!FreeQ[yy, LorentzIndex])^2 :>
								((contractProduct @@ {yy/.PairContract->Pair, yy/.PairContract->Pair,opts}
								) /. Pair -> PairContract /. PairContract -> Pair)
								};
			];
			FCPrint[1,"Contract: mainContract: Anotzher optimization done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

			contractres
		];



cartesianContract[x : Except[_Plus], opts:OptionsPattern[]] :=
		Block[ { contractres = x, contractexpandopt, time},

			contractexpandopt = OptionValue[Contract,{opts},Expanding];

			FCPrint[1, "Contract: cartesianContract: Entering", FCDoControl->cnVerbose];
			FCPrint[3, "Contract: cartesianContract: Entering with ",x, FCDoControl->cnVerbose];

			If[	contractexpandopt,
				contractres = Expand2[contractres,CartesianIndex]
			];

			time=AbsoluteTime[];
			FCPrint[1, "Contract: cartesianContract: Applying PairContract.", FCDoControl->cnVerbose];
			contractres = contractres /. Pair -> PairContract /. CartesianPair -> CartesianPairContract /. PairContract->Pair /. CartesianPairContract->CartesianPair;
			FCPrint[1,"Contract: cartesianContract: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
			FCPrint[3,"Contract: cartesianContract: After PairContract: ", contractres , FCDoControl->cnVerbose];


			FCPrint[1,"Contract: cartesianContract: Leaving.", FCDoControl->cnVerbose];
			FCPrint[3,"Contract: cartesianContract: Leaving with: ", contractres , FCDoControl->cnVerbose];

			contractres
		];

(* contractLongLongdef *)
(* b must be collected w.r.t LorentzIndex,
	covers cases for
	X*Pair[...] and X*Y, where X can be a sum or not and
	Y is not a sum *)
contractLongLong[a_, b_ /; Head[b] =!= Plus] :=
	If[ Head[b] === Pair,
		contractLongShort[a, b],
		If[ Head[a] === Plus,
			contractLongLong[b, a],
			Contract[a b, FCI->True]
		]
	];

(*	b must be collected w.r.t LorentzIndex,
	covers X*(A1+...+An)	*)
contractLongLong[lon_, shor_Plus] :=
	Block[ {neew = {}, long = lon, short = shor, tet, ij},
		FCPrint[1,"Long contraction ", Length[long], " * ", Length[short], " \n ",UseWriteString->True];
		For[ij = 1, ij <= Length[short], ij++,
			FCPrint[3,"stdout"," | ", ij, "  XXX | ",UseWriteString->True];      ;
			FCPrint[1, "before contractLongShort "];
			tet = contractLongShort[long, short[[ij]] ];
			FCPrint[1, "after contractLongShort "];
			If[ !FreeQ[tet, LorentzIndex],
				tet = tet /. Pair->pairsave /. pair2 -> Pair
			];
			If[ !FreeQ[tet, LorentzIndex],
				FCPrint[1,"expanding in contractLongLong ",UseWriteString->True];
				tet = Expand[Expand[tet] /. Pair->pairsave /. pair2 -> Pair];
			(*
				tet = Expand2[tet, LorentzIndex] /. Pair->pairsave /. pair2 -> Pair;
		*)
				];
			If[ Head[tet] === Plus,
				neew  = Join[neew, Apply[List, tet]],
				AppendTo[neew, tet]
			];
		];

		FCPrint[2,"applying plus to neew "];
		neew = Apply[Plus, neew];
		FCPrint[2,"exiting contractLongLong"];
		neew
	];


(* local easy contraction rules *) (* paird *)

SetAttributes[{pairsave, pair2,fdi}, Orderless];

fdi[] = 4;

fdi[_Symbol] :=
	4;

fdi[xx_, xx_] :=
	xx;

fdi[4, _Symbol] :=
	4;

fdi[4, _Symbol- 4] :=
	0;

fdi[xx_Symbol- 4, xx_] :=
	xx - 4;


pairsave[a_, b_] :=
	pairsave[a, b] =
	If[ FreeQ[{a,b},LorentzIndex] && optExpandScalarProduct,
		ExpandScalarProduct[Pair[a,b],FCI->True],
		pair2[a, b]
	];
pair2[LorentzIndex[a_, di1___], LorentzIndex[a_, di2___]] :=
	fdi[di1, di2];

pair2/: pair2[LorentzIndex[a_, dim1___], LorentzIndex[b_, dim2___]]^2 :=
	fdi[dim1, dim2]/; a=!=b;

pair2/: pair2[LorentzIndex[_,de1___], Momentum[b_, de2___]]^2 :=
	Pair[Momentum[b, fdi[de1,de2]], Momentum[b,fdi[de1,de2]]];

pair2/: pair2[LorentzIndex[al_,___], z_] pair2[LorentzIndex[al_,___], w_] :=
	pair2[z, w];

(* ???????? BLOEDSINN; PairContract does it
(*NEW*)
	pair2/: pair2[LorentzIndex[al_,di___], z_] Twist2GluonOperator[ww__] :=
			(Twist2GluonOperator[ww] /. LorentzIndex[al,di] -> z) /;
			!FreeQ[{ww}, LorentzIndex[al,di]];
*)

(* contractLongShort can still have products in the first argument *)
(* by construction the second argument will always be either a
	product or just Pair[  ,  ]
*)

contra3a[xx_, {pr_, prl__}, opts:OptionsPattern[]] :=
	contra3a[contra3a[xx, {pr}], {prl}, opts];

contra3b[xx_, {alien_ /; Head[alien] =!= pair2}, OptionsPattern[]] :=
	Expand2[xx alien, Pair];

contra3c[xx_, {Pair[LorentzIndex[mu_,di___], alpha_]}, OptionsPattern[]] :=
	Block[ {nxx},
		If[ FreeQ[xx, LorentzIndex[mu,___]],
			nxx = Expand2[xx Pair[LorentzIndex[mu, di], alpha], Pair],
			(* else *)
			FCPrint[1,"contra3c1111check"];
			nxx = xx;
			FCPrint[1,"contra3c2222check"];
			FCPrint[2,"contra3c : Length of xx now ", Length[nxx]];
			nxx = nxx /. LorentzIndex[mu, ___] -> alpha;
			FCPrint[2,"contra3c3333check"];

		];
		nxx
	];

(* If y is a single object that is not a Pair (e.g. Dot) *)
contractLongShort[x_, y: Except[_Times | _Pair], opts:OptionsPattern[]] :=
	Contract[x y, FCI->True, opts];

(* If x is a single object that is not a Pair (e.g. Dot) *)
contractLongShort[x: Except[_Times | _Plus], y_, opts:OptionsPattern[]] :=
	Contract[x y, FCI->True, Expanding -> False, opts];

(* If x is a product *)
contractLongShort[x_Times, y_, opts:OptionsPattern[]] :=
	( (x/#) contit[#, y, opts] )&[Select[x, !FreeQ[#, LorentzIndex]&] ];

(* If x is a sum *)
contractLongShort[x_Plus, y_, opts:OptionsPattern[]] :=
	contract22[x, y /. Pair -> pairsave, opts] /.
	contra4 -> contra3a /.  contra3a -> contra3b /. Pair -> pairsave /.  contra3b -> contra3c /. pair2 -> Pair;

list2[x_] :=
	If[ Head[x] === Times,
		List @@ x,
		{x}
	];

contract22[_, 0, OptionsPattern[]] :=
	0;

contract22[xx_, yy_Pair, opts:OptionsPattern[]] :=
	contra3a[xx, {yy}, opts] /.  contra3a -> contra3b /. Pair -> pairsave /.
	contra3b -> contra3c /. pair2 -> Pair;

contract22[xx_, yy_Times, opts:OptionsPattern[]] :=
	( (yy/#) contra4[xx, list2[#], opts] )&[Select[yy, !FreeQ[#, LorentzIndex]&]];


contit[xx_ , yy_, opts:OptionsPattern[]] :=
	If[ FreeQ[xx, LorentzIndex],
		xx Contract[yy, FCI->True, opts],
		If[ Head[xx] =!= Times,
			Contract[xx yy, FCI->True, opts],
			If[ Length[xx] =!= 2,
				Contract[xx yy, FCI->True, opts],
				If[ (Head[xx[[1]]] === Plus) && (Head[xx[[2]]] === Plus),
					FCPrint[2,"contracting a product of a ",Length[xx[[1]]], " term sum  by a",
						Length[xx[[2]]], " term sum"];
					(* that's the common situation !! *)
					Apply[ Plus, Flatten[Table[ (xx[[1, i]] xx[[2, j]] yy) /.
						Pair -> pairsave /. pair2  -> Pair, {i,1,Length[xx[[1]]]},
						{j,1,Length[xx[[2]]]}]]],
					Contract[xx yy, FCI->True, opts]
				]
			]
		]
	];

sceins[0,_] :=
	0;
sceins[a_LorentzIndex b_, c_] :=
	b sceins[a, c];
sceins[a_Momentum b_, c_] :=
	b sceins[a, c];

simplerules = {
	Pair[LorentzIndex[a_, dim_:4], b_] Pair[LorentzIndex[a_, dim_:4], c_] :> Pair[b, c],
	Pair[LorentzIndex[x_, dim_:4], LorentzIndex[x_, dim_:4]] :> dim,
	Pair[LorentzIndex[_, _:4], y_]^2 :> Pair[y, y]
};

(* #################################################################### *)

ident3[a_,_] :=
	a;

(* decide whether the (first) appearance of inds in expr is ordered *)
(*ordqdef*)
ordq[expr_,inds_List] :=
	Block[ {pos, min},
		pos = Position[expr, #]& /@ inds;
		pos = pos /. {} -> Sequence[];
		If[ Length[pos]>0,
			pos = Map[First,pos]
		];
		min = Min[Length/@pos];
		pos = Map[Take[#,min]&, pos];
		OrderedQ[pos]
	];


eps2rules = {Eps[LorentzIndex[_,dia___], b_Momentum, c_Momentum, d_Momentum]^2 :>
				Eps[LorentzIndex[$MU[1], dia], b, c, d]^2,

			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___], c_Momentum, d_Momentum]^2/; a=!=b :>
				Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia], c, d]^2,

			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___], LorentzIndex[c_,dia___], d_Momentum]^2/; a=!=b && b=!=c :>
				Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia], LorentzIndex[$MU[3], dia], d]^2,

			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___], LorentzIndex[c_,dia___], LorentzIndex[d_,dia___]]^2/; a=!=b && b=!=c && c=!=d  :>
			Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia], LorentzIndex[$MU[3], dia], LorentzIndex[$MU[4], dia]]^2
};

(*  doubleindexdef *)
(* For canonizing dummy indices between Eps and other functions *)

doubleindex[0] =
	0;

doubleindex[x_] :=
	Block[ {xy = x, suli = {}, muUU},
		For[i = 1, i < 7, i ++,
			If[ EvenQ[Length[Position[x, $MU[i]]]] && !FreeQ[x, $MU[i]],
				AppendTo[suli, RuleDelayed @@ {$MU[i], muUU[i]}]
			];
		];
		If[ Length[suli] > 0,
			FCPrint[1,"suli == ",suli];
			xy = xy /. suli
		];
		xy = doubleindex0[x];

		If[ xy === 0,
			FCPrint[1,"doubleindexTROUBLE???????????? "];
			Print["entering with", x];
		];
		xy
	];

doubleindex0[x_] :=
	Block[ {double2, double3a},
		If[ FreeQ[x, Eps],
			x,
			If[ Head[x] === Plus,
				Map[doubleindex, x],
				double2[y_] :=
					double3a[y /. {Eps :> eepp} , 1] /. double3a -> double3;
				double3a[y_, i_] :=
					double3a[y, i+1] /; Length[Position[y, $MU[i]]] > 0;
				double2[x] /.  eepp -> Eps /. double3 -> ident3/.  eepp -> Eps
			]
		] /. eps2rules
	];

double2[x_] :=
	If[ Length[Position[x, $MU]] > 0,
		double3a[x/.Eps->eepp/.$MU->FCGV[ToString[Unique["lI"]]], 1] /.
		double3a-> double3,
		double3a[x/.Eps->eepp, 1] /.  double3a-> double3
	];

double3a[x_, i_] :=
	If[ FreeQ[x, $MU[i+1]],
		double3a[x, i+1],
		double3a[x, i+2]
	] /; !FreeQ[x, $MU[i]];

lorhipa[a_,___] :=
	LorentzIndex[a,  BlankNullSequence[]];

double3[ m_. eepp[a1___, LorentzIndex[be_, di___], a2___], j_ ] :=
	(m/.be->$MU[j]) Eps[a1,LorentzIndex[$MU[j],di],a2]/; (!FreeQ[m, LorentzIndex[be, ___]]) && FreeQ2[m, Select[{a1,a2}, Head[#]===LorentzIndex&] /. LorentzIndex -> lorhipa];

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___,  LorentzIndex[mu2_, di2___],a3___], j_ ] :=
	((m/.mu1->$MU[j]/.mu2->$MU[j+1]) Eps[a1,LorentzIndex[$MU[j],di1],a2, LorentzIndex[$MU[j+1],di2],a3])/;
	(FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] && (!FreeQ[m, LorentzIndex[mu1, ___]]) && (!FreeQ[m, LorentzIndex[mu2, ___]]) &&
		(FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/. LorentzIndex -> lorhipa]) && ordq[m, {mu1,mu2}]);

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___, LorentzIndex[mu2_, di2___],a3___], j_ ] :=
	((m/.mu2->$MU[j]/.mu1->$MU[j+1]) Eps[a1,LorentzIndex[$MU[j+1],di1],a2, LorentzIndex[$MU[j],di2],a3])/;
	(	FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] &&
		(!FreeQ[m, LorentzIndex[mu1, ___]]) &&
		(!FreeQ[m, LorentzIndex[mu2, ___]]) &&
		(FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/. LorentzIndex -> lorhipa]) && ordq[m, {mu2,mu1}]);

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___, LorentzIndex[mu2_, di2___], a3___, LorentzIndex[mu3_, di3___], a4___ ], j_ ] :=
	Block[ {dte,a,b,c},
		dte = (  (m/.mu1->$MU[j]/.mu2->$MU[j+1]/.mu3->$MU[j+2]) Eps[a1,LorentzIndex[$MU[j],di1], a2, LorentzIndex[$MU[j+1],di2],a3, LorentzIndex[$MU[j+2],di3], a4]);
		a = $MU[j];
		b = $MU[j+1];
		c = $MU[j+2];
		Which[ordq[m, {mu1,mu2,mu3}],
			dte,
			ordq[m, {mu1,mu3,mu2}],
			dte /.  {b :>c, c:>b},
			ordq[m, {mu2,mu1,mu3}],
			dte /.  {a:>b,b:>a},
			ordq[m, {mu2,mu3,mu1}],
			dte /.  {b:>a, c:>b, a:>c},
			ordq[m, {mu3,mu1,mu2}],
			dte /.  {c:>a, a:>b, b:>c},
			ordq[m, {mu3,mu2,mu1}],
			dte /.  {c:>a, a:>c}
			]
	]/; FreeQ2[{m,a1,a2,a3,a4}, {$MU[j], $MU[j+1], $MU[j+2]}] &&
		(!FreeQ2[m, LorentzIndex[mu1,___]] &&
		!FreeQ2[m, LorentzIndex[mu2,___]] &&
		!FreeQ2[m, LorentzIndex[mu3,___]]
		) && FreeQ2[m, Select[{a1,a2,a3,a4}, Head[#]===LorentzIndex&]/. LorentzIndex -> lorhipa];

double3[ m_. eepp[LorentzIndex[mu1_,di1___],LorentzIndex[mu2_,di2___], LorentzIndex[mu3_,di3___],LorentzIndex[mu4_,di4___]], _ ] :=
	Block[ {dte,a,b,c,d},
		dte = (m/.mu1->$MU[1]/.mu2->$MU[2]/.mu3->$MU[3]/.mu4->$MU[4]) Eps[LorentzIndex[$MU[1],di1],  LorentzIndex[$MU[2],di2], LorentzIndex[$MU[3],di3], LorentzIndex[$MU[4],di4]];
		a = $MU[1];
		b = $MU[2];
		c = $MU[3];
		d = $MU[4];
		Which[
			ordq[m, {mu1, mu2, mu3, mu4}],
				ReplaceAll[dte, {a -> a, b -> b, c -> c, d -> d}],
			ordq[m, {mu1, mu2, mu4, mu3}],
				ReplaceAll[dte, {a -> a, b -> b, c -> d, d -> c}],
			ordq[m, {mu1, mu3, mu2, mu4}],
				ReplaceAll[dte, {a -> a, b -> c, c -> b, d -> d}],
			ordq[m, {mu1, mu3, mu4, mu2}],
				ReplaceAll[dte, {a -> a, b -> c, c -> d, d -> b}],
			ordq[m, {mu1, mu4, mu2, mu3}],
				ReplaceAll[dte, {a -> a, b -> d, c -> b, d -> c}],
			ordq[m, {mu1, mu4, mu3, mu2}],
				ReplaceAll[dte, {a -> a, b -> d, c -> c, d -> b}],
			ordq[m, {mu2, mu1, mu3, mu4}],
				ReplaceAll[dte, {a -> b, b -> a, c -> c, d -> d}],
			ordq[m, {mu2, mu1, mu4, mu3}],
				ReplaceAll[dte, {a -> b, b -> a, c -> d, d -> c}],
			ordq[m, {mu2, mu3, mu1, mu4}],
				ReplaceAll[dte, {a -> b, b -> c, c -> a, d -> d}],
			ordq[m, {mu2, mu3, mu4, mu1}],
				ReplaceAll[dte, {a -> b, b -> c, c -> d, d -> a}],
			ordq[m, {mu2, mu4, mu1, mu3}],
				ReplaceAll[dte, {a -> b, b -> d, c -> a, d -> c}],
			ordq[m, {mu2, mu4, mu3, mu1}],
				ReplaceAll[dte, {a -> b, b -> d, c -> c, d -> a}],
			ordq[m, {mu3, mu1, mu2, mu4}],
				ReplaceAll[dte, {a -> c, b -> a, c -> b, d -> d}],
			ordq[m, {mu3, mu1, mu4, mu2}],
				ReplaceAll[dte, {a -> c, b -> a, c -> d, d -> b}],
			ordq[m, {mu3, mu2, mu1, mu4}],
				ReplaceAll[dte, {a -> c, b -> b, c -> a, d -> d}],
			ordq[m, {mu3, mu2, mu4, mu1}],
				ReplaceAll[dte, {a -> c, b -> b, c -> d, d -> a}],
			ordq[m, {mu3, mu4, mu1, mu2}],
				ReplaceAll[dte, {a -> c, b -> d, c -> a, d -> b}],
			ordq[m, {mu3, mu4, mu2, mu1}],
				ReplaceAll[dte, {a -> c, b -> d, c -> b, d -> a}],
			ordq[m, {mu4, mu1, mu2, mu3}],
				ReplaceAll[dte, {a -> d, b -> a, c -> b, d -> c}],
			ordq[m, {mu4, mu1, mu3, mu2}],
				ReplaceAll[dte, {a -> d, b -> a, c -> c, d -> b}],
			ordq[m, {mu4, mu2, mu1, mu3}],
				ReplaceAll[dte, {a -> d, b -> b, c -> a, d -> c}],
			ordq[m, {mu4, mu2, mu3, mu1}],
				ReplaceAll[dte, {a -> d, b -> b, c -> c, d -> a}],
			ordq[m, {mu4, mu3, mu1, mu2}],
				ReplaceAll[dte, {a -> d, b -> c, c -> a, d -> b}],
			ordq[m, {mu4, mu3, mu2, mu1}],
				ReplaceAll[dte, {a -> d, b -> c, c -> b, d -> a}]
		]
	] /; (!FreeQ2[m, LorentzIndex[mu1,___]] && !FreeQ2[m, LorentzIndex[mu2,___]] && !FreeQ2[m, LorentzIndex[mu3,___]] && !FreeQ2[m, LorentzIndex[mu4,___]]) && FreeQ2[m, {$MU[1], $MU[2], $MU[3], $MU[4]}];

FCPrint[1,"Contract.m loaded."];
End[]
