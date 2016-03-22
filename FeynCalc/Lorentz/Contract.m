(* ::Package:: *)



(* :Title: Contract															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Contraction routines for Lorentz algebra					*)

(* ------------------------------------------------------------------------ *)

Contract::usage =
"Contract[expr] contracts pairs of Lorentz indices of metric tensors, \
four-vectors and (depending on the option EpsContract) of \
Levi-Civita tensors in expr. For the contraction of Dirac matrices \
with each other use DiracSimplify. \n \n
Contract[exp1, exp2] contracts (exp1*exp2), where exp1 and exp2 may be \
larger products of sums of  metric tensors and 4-vectors. \
Contract[exp1, exp2] should be used for polarization sums, where \
exp2 should be the product (or expanded sum) of the polarization \
sums for the vector bosons. \n \n
Contract is also an option for DoPolarizationSums. When set to True, \
Contract is applied to the amplitude immediately after replacing \
polarization vectors by the corresponding sum. This usually improves \
performance.";

Contract2::usage =
"Contract2[expr] (still experimental).";

Contract3::usage =
"Contract3[expr] (still experimental).";

Contract::fail =
"Error! Contract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Rename::usage =
"Rename is an option for Contract. If set to True, dummy indices in Eps \
are renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]


Begin["`Contract`Private`"]

cnVerbose::usage="";

fci[x_] :=
	If[ (FeynCalcInternal /. Options[Contract]) === True,
		FeynCalcInternal[x],
		x
	];



Contract3[x_Plus, opts:OptionsPattern[]] :=
	Map[Contract3[#,opts]&, x];

Contract3[x_ /; (Head[x] =!= Times) && Head[x] =!= Plus,opts:OptionsPattern[]] :=
	Contract[x, Contract3->False,opts];

Contract3[x_Times, opts:OptionsPattern[]] :=
	If[ !FreeQ[x, DiracGamma | Eps],
		Contract[x, Contract3 -> False,opts],
		If[ FreeQ[fci[x], LorentzIndex],
			fci[x],
			Block[ {nx = x, nonli, lipa, nec = 0, ic,epli},
				nx = Contract[x , Expanding -> False, Contract3->False, opts];
				If[ Head[nx] =!= Times,
					nec = Contract[nx, Contract3->False],
					nonli = Select[nx, FreeQ[#, LorentzIndex]&];
					lipa  = Select[nx,!FreeQ[#, LorentzIndex]&];
					If[ Head[lipa] =!= Times,
						If[ Head[lipa] === Plus,
							nec = Contract3[lipa (*epli*),opts],
							nec = Contract[lipa (*epli*), Contract3->False,opts]
						],
						If[ Length[lipa] < 2,
							nec = Contract[lipa (*epli*), Contract3->False, opts],
							nec = lipa[[1]](*epli*);
							For[ic = 2, ic <= Length[lipa], ic++,
								FCPrint[2,"ic = ", ic, " out of ",Length[lipa]];
								If[ LeafCount[nec] < LeafCount[lipa[[ic]]] ||
									If[ True,
										!FreeQ[lipa[[ic]], Twist2GluonOperator],
										False
									],
									nec = Contract[lipa[[ic]], nec, Contract3->False,opts],
									nec = Contract[nec, lipa[[ic]], Contract3->False,opts]
								];
								FCPrint[2,"expand scalar products"];
								nec = ExpandScalarProduct[nec];

								FCPrint[2,"expand scalar products done"];

								If[ !FreeQ[nec, LorentzIndex],
									FCPrint[2,"expanding LorentzIndex now"];
									tim = TimeUsed[];
									nec = Expand[nec, LorentzIndex];
									FCPrint[2,"expanding LorentzIndex DONE ",
											TimeUsed[] - tim];
								];
							];
						];
					];
					nec = nec nonli;
				];
				nec
			]
		]
	];

(* #################################################################### *)

Options[Contract2] = {
	Collecting -> False
};

(* bb is assumed to be collected w.r.t. to LorentzIndex !!! *)
Contract2[a_, bb_, ops___Rule] :=
	Block[ {sel, ct, rc, lco, lct, lastct, nop, b = bb, col, conT},
		col = Collecting /. {ops} /. Options[Contract2];
		If[ Head[a] =!= Times,
			rc = Contract[a, b],
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
				rc = sel Contract[ct nop, b],
				ct = Sort[List @@ ct, lco];
				If[ nop =!= 1,
					lastct = contract21[b, nop, ops],
					lastct = b nop
				];
				lct = Length[ct];
				If[ lct === 1,
					rc = sel contractLColl[ct[[1]], lastct]
				];
				If[ lct > 1,
					rc = sel Contract[Times @@ Take[ct, lct-1],
									ct[[lct]], lastct ]
				];
			];
		];
		FCPrint[2,"lct = ",lct];
		If[ !FreeQ[rc, LorentzIndex],
			rc = Contract[rc, Expanding -> False];
		];
		If[ !FreeQ[rc, LorentzIndex],
			FCPrint[1,"contracting agagin at the end of Contract2 "];
			rc = Contract[rc]
		];
		rc
	];

Contract2[a_] :=
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
					lastct = contract21[Last[ct], nop],
					lastct = Last[ct] nop;
				];
				lct = Length[ct];
				If[ lct === 2,
					rc = sel contractLColl[ct[[1]], lastct]
				];
				If[ lct > 2,
					rc = sel Contract[Times @@ Take[ct, lct-2],
									ct[[lct-1]], lastct ]
				];
			];
		];
		FCPrint[2,"lct = ",lct];
		If[ !FreeQ[rc, LorentzIndex],
			FCPrint[1,"contracting agagin at the end of Contract2 "];
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
	Collecting      -> True,
	Contract3       -> True,
	EpsContract     -> True,
	Expanding       -> True,
	Factoring       -> False,
	FCI				-> True,
	FCVerbose		-> False,
	MomentumCombine -> True,
	Rename          -> False,
	Schouten        -> 0
};

Contract[l_List, opts:OptionsPattern[]] :=
	Contract[#,opts]&/@l;

(*	Main entry point	*)
Contract[y_, z:OptionsPattern[]] :=
	Block[{ex=fci[y],tmp,rest1=0,rest2=0,rest3=0,noDummy=0,nodot,
		null1,null2,freeIndList,freeHead,tmpFin,res,expandOpt,
		epsContractOpt,renameOpt,schoutenOpt,times,tmpList,time,tmpCheck},

		expandOpt 		= OptionValue[Expanding];
		epsContractOpt 	= OptionValue[EpsContract];
		renameOpt 		= OptionValue[Rename];
		schoutenOpt 	= OptionValue[Schouten];

		If [OptionValue[FCVerbose]===False,
			cnVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				cnVerbose=OptionValue[FCVerbose]
			];
		];

		If[expandOpt,
			ex = Expand2[ex,LorentzIndex]
		];

		FCPrint[1, "Contract: Entering main contract", FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Entering with", ex, FCDoControl->cnVerbose];

		If[	FreeQ2[ex, {LorentzIndex,Eps}],
			Return[ex]
		];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices from those that don't.", FCDoControl->cnVerbose];
		(* First splitting: Terms that do not need any contractions are not processed further	*)
		{rest1,tmp} = FCSplit[ex, {LorentzIndex,Eps},Expanding->False];
		(* TODO isolate terms without LorentzIndex and Eps*)
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		FCPrint[3, "Contract: Terms that contain LorentzIndex or Eps: ", tmp, FCDoControl->cnVerbose];
		FCPrint[3, "Contract: Other terms: ", rest1, FCDoControl->cnVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices with a FreeIndex DataType", FCDoControl->cnVerbose];
		freeIndList = Cases[tmp+null1+null2,LorentzIndex[ind_,_:4]/; DataType[ind,FreeIndex]:> {ind,freeHead[ind]},Infinity]//Union;
		If[	freeIndList=!={},
			FCPrint[3, "Contract: List of indices that should not be summed over", freeIndList, FCDoControl->cnVerbose];
			tmp/. Map[(Rule @@ #) &, freeIndList];
			{tmp,rest2} = FCSplit[tmp, {freeHead},Expanding->False];
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		(* Assuming that numer of terms Upper and Lower is rather small*)
		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain Lorentz indices with Upper or Lower", FCDoControl->cnVerbose];
		If[	!FreeQ2[tmp, {Upper,Lower}],
			{tmp,rest3} = FCSplit[tmp, {Upper,Lower},Expanding->False];
			FCPrint[3, "Contract: Terms that contain Upper or Lower: ", rest3, FCDoControl->cnVerbose];
			FCPrint[3, "Contract: Other terms: ", tmp, FCDoControl->cnVerbose];
			If[rest3=!=0,
				rest3 = Contract1[rest3]
			]
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

		(* Now we determine terms that contain dummy indices *)
		(* TODO Fish out the eps terms beforehand!!! *)
		time=AbsoluteTime[];
		FCPrint[1,"Contract: Separating terms that contain dummy Lorentz indices", FCDoControl->cnVerbose];
		tmpCheck = tmp;
		tmp = tmp /. Power[a_, b_Integer?Positive] /; !FreeQ[a, LorentzIndex] :> Apply[times, Table[a, {i, b}]];
		tmpList = Apply[List,tmp+null1+null2];

		tmp = Select[tmpList, hasDummyIndices];
		noDummy = Complement[tmpList,tmp];

		(*noDummy contains epsilon tensors like LC[a,b,c,d]LC[e,f,g,h]*)
		tmp = Total[tmp]/.times->Times/.null1|null2->0;
		noDummy = Total[noDummy]/.times->Times/.null1|null2->0;

		(* check that we didn't loose any terms *)
		If[tmp+noDummy =!=tmpCheck,
			Message[Contract::fail,"Extraction of dummy indices failed!"];
			Abort[]
		];
		FCPrint[1,"Contract: Splitting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];


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
			tmp =  MomentumCombine[tmp];
			FCPrint[1,"Contract: MometumCombine done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		FCPrint[3, "Contract: After MometumCombine: ", tmp, FCDoControl->cnVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"Contract: Applying contracT.", FCDoControl->cnVerbose];
		tmpFin = contracT[tmp,z];
		FCPrint[1,"Contract: contracT done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];


		FCPrint[3, "Contract: After contracT: ", tmpFin, FCDoControl->cnVerbose];

		(* Here we are done *)


		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding w.r.t Lorentz indices.", FCDoControl->cnVerbose];
			If[!FreeQ[tmpFin, LorentzIndex],
				tmpFin = Expand[tmpFin,LorentzIndex]
			];

			If[!FreeQ[noDummy, LorentzIndex],
				noDummy = Expand[noDummy,LorentzIndex]
			];
			FCPrint[1,"Contract: Expansion done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];


		If[ !FreeQ[tmpFin, Eps] && renameOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Renaming dummy indices in epsilon tensors.", FCDoControl->cnVerbose];
			tmpFin = doubleindex[Expand[ tmpFin//EpsEvaluate, Eps]];
			FCPrint[1,"Contract: Renaming done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Contracting epsilon tensors.", FCDoControl->cnVerbose];
			If[	!FreeQ[tmpFin, Eps],
				tmpFin = FCLoopIsolate[EpsEvaluate[tmpFin],{Eps},Head->tmpHead,PaVe->False,FCI->True,
					ExpandScalarProduct->False,DiracGammaExpand->False,DotSimplify->False,FeynAmpDenominatorSplit->False,
					Factoring->False]/. tmpHead-> epsCleverCon

			];

			If[	!FreeQ[noDummy, Eps],
				noDummy = FCLoopIsolate[EpsEvaluate[noDummy],{Eps},Head->tmpHead,PaVe->False,FCI->True,
					ExpandScalarProduct->False,DiracGammaExpand->False,DotSimplify->False,FeynAmpDenominatorSplit->False,
					Factoring->False]/. tmpHead-> epsCleverCon
			];
			FCPrint[1,"Contract: Epsilon contractions done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];

		If[ expandOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Expanding in Lorentz indices.", FCDoControl->cnVerbose];
			If[	!FreeQ[tmpFin, LorentzIndex],
				tmpFin = Expand[tmpFin, LorentzIndex]
			];
			If[	!FreeQ[noDummy, LorentzIndex],
				noDummy = Expand[noDummy,LorentzIndex]
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

		If[ epsContractOpt,
			time=AbsoluteTime[];
			FCPrint[1,"Contract: Applying EpsEvaluate.", FCDoControl->cnVerbose];
			If[	!FreeQ[tmpFin, Eps],
				tmpFin = tmpFin//EpsEvaluate;
			];
			If[	!FreeQ[noDummy, Eps],
				noDummy = noDummy//EpsEvaluate;
			];
			FCPrint[1,"Contract: EpsEvaluate done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
		];


		time=AbsoluteTime[];
		FCPrint[1,"Contract: Applying PairContract.", FCDoControl->cnVerbose];
		tmpFin = tmpFin /. Pair->PairContract3 /. PairContract3 -> PairContract /.PairContract->Pair;
		FCPrint[1,"Contract: PairContract done: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];

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

		FCPrint[1, "Contract: Leaving. ", FCDoControl->cnVerbose];
		FCPrint[1, "Contract: Leaving with :", res, FCDoControl->cnVerbose];
		res
	]/; Head[y]=!=List;

Contract[a_, b_ /;Head[b] =!= Rule, c_ /; Head[c] =!= Rule, ops:OptionsPattern[]] :=
	Block[ {lc, new = 0, i},

		If [OptionValue[FCVerbose]===False,
			cnVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				cnVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[2, "Contract: Before calling another Contract ", FCDoControl->cnVerbose];

		lc = Contract[b, c, ops];
		FCPrint[2, "Contract: Before calling another Contract ", FCDoControl->cnVerbose];
		new = Contract[lc, a, ops];

		FCPrint[2, "Contract: Leaving ", FCDoControl->cnVerbose];
		new
	];

Contract[x_, y_ /; Head[y]=!=Rule, c:OptionsPattern[]] :=
	(Contract[fci[x], c] y) /; FreeQ2[fci[y], {LorentzIndex,Eps}];

Contract[x_, y_Times, opts:OptionsPattern[]] :=
	Block[ {a = fci[x], b = fci[y], bb},
		If[ MatchQ[b, Apply[HoldPattern, {Times__Pair}]],
			contract21[ a, b ,opts],
			If[ MatchQ[b, HoldPattern[Times__Pair]],
				contract21[ a, b ,opts],
				bb = Collect3[b, Pair, Factoring-> False];
				If[ Head[bb] === Plus,
					contractLColl[a, bb],
					contract21[a, bb, opts]
				]
			]
		]
	];



Contract[a_, b_ /; ((Head[b]=!=Times) && (Head[b] =!= Plus) && (Head[b] =!= Rule)), c:OptionsPattern[]] :=
	Contract[ a b, c ];

Contract[a_, b_Plus, OptionsPattern[]] :=
	If[ OptionValue[Collecting] === True,
		contractLColl[fci[a],
			If[ FreeQ[List@@b, Plus],
				fci[b],
				Collect2[fci[b], LorentzIndex]
			]
					],
		contractLColl[fci[a], fci[b]]
	];


Contract[Equal[a_, b_], ops___Rule] :=
	Contract[a,ops] == Contract[b, ops];

hasDummyIndices[expr_] :=
	(Cases[expr, LorentzIndex[ind_, ___] :> ind, Infinity,Heads->True]//Tally//Transpose//Last//Max//Greater[#, 1]&) /;
	!FreeQ[{expr}, LorentzIndex];

hasDummyIndices[expr_] := False/;
	FreeQ[{expr},LorentzIndex];

contracT[x_,opts:OptionsPattern[]] :=
		Block[ { contractres = x,
				contractexpandopt, es, time,
				contract3, schout, contractfactoring },

			contractexpandopt = OptionValue[Contract,{opts},Expanding];
			contractfactoring = OptionValue[Contract,{opts},Factoring];
			contract3 = OptionValue[Contract,{opts},Contract3];

			FCPrint[1, "Contract: contracT: Entering", FCDoControl->cnVerbose];
			FCPrint[3, "Contract: contracT: Entering with ",x, FCDoControl->cnVerbose];

			(* NEW: September 16th 2003: adding Contract3 directly here ... *)
			If[ contract3 && contractexpandopt,
				If[ MemberQ[{Plus, Times}, Head[contractres]],
					time=AbsoluteTime[];
					FCPrint[1, "Contract: contracT: Applying Contract3.", FCDoControl->cnVerbose];
					contractres = Contract3[contractres,opts];
					FCPrint[1,"Contract: contracT: Contract3 done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
					FCPrint[3,"Contract: contracT: After Contract3: ", contractres , FCDoControl->cnVerbose]
				]
			];

			time=AbsoluteTime[];
			FCPrint[1, "Contract: contracT: Applying PairContract.", FCDoControl->cnVerbose];
			contractres = contractres /. Pair -> PairContract3 /. PairContract3 -> PairContract/.
							PairContract -> sceins /. sceins -> Pair;
			FCPrint[1,"Contract: contracT: PairContract done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
			FCPrint[3,"Contract: contracT: After PairContract: ", contractres , FCDoControl->cnVerbose];

			(* optimization *)
			If[ Head[contractres === Plus] && Length[contractres > 47],
				If[ !FreeQ[contractres, Eps],
					time=AbsoluteTime[];
					FCPrint[1, "Contract: contracT: Applying optimization.", FCDoControl->cnVerbose];
					es = {	Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
							Pair[LorentzIndex[a_, D], b_] Eps[c___,LorentzIndex[a_, D],d___] :> Eps[c,b,d]
					};
					contractres = contractres //. es;
					FCPrint[1,"Contract: contracT: Optimization done. Timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->cnVerbose];
					FCPrint[3,"Contract: contracT: After optimization: ", contractres , FCDoControl->cnVerbose]

				]
			];

			If[ contractexpandopt && !contract3 && !FreeQ[contractres, LorentzIndex],
				(* NEW October 2003, this speeds things up in general*)
				contractres = Expand[Expand[contractres, LorentzIndex] //. simplerules];
				contractres = contractres /.
								{((yy_Plus)  /;!FreeQ[yy, LorentzIndex])^2 :>
								((Contract @@ {yy/.PairContract->Pair, yy/.PairContract->Pair,opts}
								) /. Pair -> PairContract /. PairContract -> Pair)
								};
			];

			contractres
		];


(* contractLColldef *)
contractLColl[a_, b_ /; Head[b] =!= Plus] :=
	If[ Head[b] === Pair,
		contract21[a, b],
		If[ Head[a] === Plus,
			contractLColl[b, a],
			Contract[a b]
		]
	];

contractLColl[lon_, shor_Plus] :=
	Block[ {neew = {}, long = lon,
		short = shor, tet},
		FCPrint[1,"Long contraction ", Length[long], " * ", Length[short], " \n ",UseWriteString->True];
		For[ij = 1, ij <= Length[short], ij++,
			FCPrint[3,"stdout"," | ", ij, "  XXX | ",UseWriteString->True];      ;
			FCPrint[1, "before contract21 "];
			tet = contract21[long, short[[ij]] ];
			FCPrint[1, "after contract21 "];
			If[ !FreeQ[tet, LorentzIndex],
				tet = tet /. Pair->pairsave /. pair2 -> Pair
			];
			If[ !FreeQ[tet, LorentzIndex],
				FCPrint[1,"expanding in contractLColl ",UseWriteString->True];
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
		FCPrint[2,"exiting contractLColl"];
		neew
	];

(* local easy contraction rules *) (* paird *)
fdi[] = 4;
fdi[_Symbol] :=
	4;
fdi[xx_Symbol, xx_Symbol] :=
	xx;
fdi[4, _Symbol] :=
	4;
fdi[_Symbol, 4] :=
	4;

SetAttributes[{pairsave, pair2}, Orderless];
pairsave[a_, b_] :=
	pairsave[a, b] =
	If[ FreeQ[{a,b},LorentzIndex],
		ExpandScalarProduct[a,b],
		pair2[a, b]
	];
pair2[LorentzIndex[a_, di1___], LorentzIndex[a_, di2___]] :=
	fdi[di1, di2];

pair2/: pair2[LorentzIndex[a_, dim1___], LorentzIndex[b_, dim2___]]^2 :=
	fdi[dim1, dim2];

pair2/: pair2[LorentzIndex[a_,de1___], Momentum[b_, de2___]]^2 :=
	Pair[Momentum[b, fdi[de1,de2]], Momentum[b,fdi[de1,de2]]];

pair2/: pair2[LorentzIndex[al_,di___], z_] pair2[LorentzIndex[al_,di2___],
												w_] := pair2[z, w];
(* ???????? BLOEDSINN; PairContract does it
(*NEW*)
	pair2/: pair2[LorentzIndex[al_,di___], z_] Twist2GluonOperator[ww__] :=
			(Twist2GluonOperator[ww] /. LorentzIndex[al,di] -> z) /;
			!FreeQ[{ww}, LorentzIndex[al,di]];
*)

(* contract21 can still have products in the first argument *)
(* by construction the second argument will always be either a
	product or just Pair[  ,  ]
*)

contra3a[xx_, {pr_, prl__}] :=
	contra3a[contra3a[xx, {pr}], {prl}];

contra3b[xx_, {alien_ /; Head[alien] =!= pair2}] :=
	Expand2[xx alien, Pair];

contra3c[xx_, {Pair[LorentzIndex[mu_,di___], alpha_]} ] :=
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

contract21[z_, yy_ /; ((Head[yy] =!= Pair) && Head[yy] =!= Times), opts:OptionsPattern[]] :=
	Contract[z yy, opts];

contract21[xx_Plus, yy_, OptionsPattern[]] :=
	contract22[xx, yy /. Pair -> pairsave] /.
	contra4 -> contra3a /.  contra3a -> contra3b /.
	Pair -> pairsave /.  contra3b -> contra3c /. pair2 -> Pair;

list2[x_] :=
	If[ Head[x] === Times,
		List @@ x,
		{x}
	];

contract22[_, 0] :=
	0;

contract22[xx_, yy_Pair] :=
	contra3a[xx, {yy}] /.  contra3a -> contra3b /. Pair -> pairsave /.
	contra3b -> contra3c /. pair2 -> Pair;

contract22[xx_, yy_Times] :=
	( (yy/#) contra4[xx, list2[#]] )&[Select[yy, !FreeQ[#, LorentzIndex]&]];

contract21[xx_ /;(Head[xx] =!= Plus) && (Head[xx] =!= Times), yy_] :=
	Contract[xx yy,Expanding -> False];

contract21[xxx_Times, yyy_] :=
	( (xxx/#) contit[#, yyy] )&[Select[xxx, !FreeQ[#, LorentzIndex]&] ];


contit[xx_ , yy_] :=
	If[ FreeQ[xx, LorentzIndex],
		xx Contract[yy],
		If[ Head[xx] =!= Times,
			Contract[xx yy],
			If[ Length[xx] =!= 2,
				Contract[xx yy],
				If[ (Head[xx[[1]]] === Plus) && (Head[xx[[2]]] === Plus),
					iCcount = 1;
					FCPrint[2,"contracting a product of a ",Length[xx[[1]]], " term sum  by a",
						Length[xx[[2]]], " term sum"];
					(* that's the common situation !! *)
					Apply[ Plus, Flatten[Table[ (xx[[1, ii]] xx[[2, jj]] yy) /.
						Pair -> pairsave /. pair2  -> Pair, {ii,1,Length[xx[[1]]]},
						{jj,1,Length[xx[[2]]]}]]],
					Contract[xx yy]
				]
			]
		]
	];


(* #################################################################### *)

(* contractlidef *)
contractli[x_] :=
	MemSet[contractli[x],x] /; FreeQ[x//Hold,LorentzIndex];

contractli[x_] :=
	Contract[ x, Expanding->True, Factoring->False, EpsContract->False];

conall[ x_ ] :=
	Contract[ x, Expanding->True, EpsContract->True, Factoring->False];

epsCleverCon[expr_]:=
	expr //. Power[Eps[a__],n_] :> epsHold[epscon[a]^n] //.
	Eps[a__] Eps[b__]/; Length[Intersection[{a},{b}]]===3 :> epsHold[epscon[a]epscon[b]] //.
	Eps[a__] Eps[b__]/; Length[Intersection[{a},{b}]]===2 :> epsHold[epscon[a]epscon[b]] //.
	Eps[a__] Eps[b__]/; Length[Intersection[{a},{b}]]===1 :> epsHold[epscon[a]epscon[b]] /.
	Eps -> epscon /. epscon -> Eps /. epsHold->Identity;

epscon/: epscon[a1_,a2_,a3_,a4_,OptionsPattern[Eps]]^2 :=
	((( - ($LeviCivitaSign)^2 Det[{{PairContract[a1,a1],PairContract[a1,a2],PairContract[a1,a3],PairContract[a1,a4]},
	{PairContract[a2,a1],PairContract[a2,a2],PairContract[a2,a3],PairContract[a2,a4]},
	{PairContract[a3,a1],PairContract[a3,a2],PairContract[a3,a3],PairContract[a3,a4]},
	{PairContract[a4,a1],PairContract[a4,a2],PairContract[a4,a3],PairContract[a4,a4]}}]//Expand)/.PairContract->Pair ));


epscon/: epscon[a1_,a2_,a3_,a4_,opts:OptionsPattern[Eps]]^n_Integer?Positive :=
	((hold[epscon[a1,a2,a3,a4,opts]^2]*epscon[a1,a2,a3,a4,opts]^(n-2))/.
	hold->Identity )/; n>=3 && FreeQ[{a1,a2,a3,a4},LorentzIndex];

epscon/: epscon[a1_,a2_,a3_,a4_,OptionsPattern[Eps]]^n_Integer?Positive :=
	(Message[Contract::fail,"Epsilon tensor to a power higher than two with \
uncontracted Lorentz indices violates Einsein summation convention!"]; Abort[])/; n>=3 && !FreeQ[{a1,a2,a3,a4},LorentzIndex];

epscon/: epscon[a1_,a2_,a3_,a4_,OptionsPattern[Eps]] epscon[b1_,b2_,b3_,b4_,OptionsPattern[Eps]] :=
	( - ($LeviCivitaSign)^2 Det[{{PairContract[a1,b1],PairContract[a1,b2],PairContract[a1,b3],PairContract[a1,b4]},
	{PairContract[a2,b1],PairContract[a2,b2],PairContract[a2,b3],PairContract[a2,b4]},
	{PairContract[a3,b1],PairContract[a3,b2],PairContract[a3,b3],PairContract[a3,b4]},
	{PairContract[a4,b1],PairContract[a4,b2],PairContract[a4,b3],PairContract[a4,b4]}}]//Expand)/.PairContract->Pair;

sceins[0,_] :=
	0;
sceins[a_LorentzIndex b_, c_] :=
	b sceins[a, c];
sceins[a_Momentum b_, c_] :=
	b sceins[a, c];


dim[] = 4;
dim[d_] :=
	d;

(* do this immediately, Oct. 2003 *)
simplerules = {
	Pair[LorentzIndex[a_, di___], b_] Pair[LorentzIndex[a_, di___], c_] :> Pair[b, c],
	Pair[LorentzIndex[x_, di___], LorentzIndex[x_, di___]] :> dim[di],
	Pair[LorentzIndex[x_, di___], y_]^2 :> Pair[y, y]
};

ident3[a_,_] :=
	a;

(* #################################################################### *)

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


eps2rules = {Eps[LorentzIndex[a_,dia___], b_Momentum,
				c_Momentum, d_Momentum, opts:OptionsPattern[]]^2 :>
			Eps[LorentzIndex[$MU[1], dia], b, c, d, opts]^2,
			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
				c_Momentum, d_Momentum, opts:OptionsPattern[]]^2 :>
			Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
				c, d, opts]^2,
			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
				LorentzIndex[c_,dia___], d_Momentum, opts:OptionsPattern[]]^2 :>
			Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
				LorentzIndex[$MU[3], dia], d, opts]^2,
			Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
				LorentzIndex[c_,dia___], LorentzIndex[d_,dia___], opts:OptionsPattern[]]^2 :>
			Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
				LorentzIndex[$MU[3], dia], LorentzIndex[$MU[4], dia], opts
				]^2
			};

	(*  doubleindexdef *)
	(* For canonizing dummy indices between Eps and other functions *)

doubleindex[0] =
	0;

doubleindex[x_] :=
	Block[ {xy = x, suli = {}, muUU},
		For[ijj = 1, ijj < 7, ijj ++,
			If[ EvenQ[Length[Position[x, $MU[ijj]]]] && !FreeQ[x, $MU[ijj]],
				AppendTo[suli, RuleDelayed @@ {$MU[ijj], muUU[ijj]}]
			];
		];
		If[ Length[suli] > 0,
			FCPrint[1,"suli == ",suli];
			xy = xy /. suli
		];
		xy = doubleindex0[x];
(*
						If[!FreeQ[x, Eps] && !FreeQ[x, $MU],
							doubleindex0[doubleindex0[x]],
							doubleindex0[x]
						];
*)
		If[ xy === 0,
			FCPrint[1,"doubleindexTROUBLE???????????? "];
			Print["entering with", x];
		];
(*
*)
		xy
	];

doubleindex0[x_] :=
	Block[ {double2, double3a},
		If[ FreeQ[x, Eps],
			x,
			If[ Head[x] === Plus,
				Map[doubleindex, x],
				double2[y_] :=
					double3a[y /. {Eps :> eepp} , 1
							] /. double3a -> double3;
				double3a[y_, i_] :=
					double3a[y, i+1] /;
					Length[Position[y, $MU[i]]] > 0;
				double2[x] /.  eepp -> Eps /.
				double3 -> ident3/.  eepp -> Eps
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
	(m/.be->$MU[j]) Eps[a1,LorentzIndex[$MU[j],di],a2]/;
	(!FreeQ[m, LorentzIndex[be, ___]]) &&
	FreeQ2[m, Select[{a1,a2}, Head[#]===LorentzIndex&] /. LorentzIndex -> lorhipa];

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___,  LorentzIndex[mu2_, di2___],a3___], j_ ] :=
	((m/.mu1->$MU[j]/.mu2->$MU[j+1]) Eps[a1,LorentzIndex[$MU[j],di1],a2, LorentzIndex[$MU[j+1],di2],a3])/;
	(	FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] &&
		(!FreeQ[m, LorentzIndex[mu1, ___]]) &&
		(!FreeQ[m, LorentzIndex[mu2, ___]]) &&
		(FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/. LorentzIndex -> lorhipa]) && ordq[m, {mu1,mu2}]);

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___, LorentzIndex[mu2_, di2___],a3___], j_ ] :=
	((m/.mu2->$MU[j]/.mu1->$MU[j+1]) Eps[a1,LorentzIndex[$MU[j+1],di1],a2, LorentzIndex[$MU[j],di2],a3])/;
	(	FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] &&
		(!FreeQ[m, LorentzIndex[mu1, ___]]) &&
		(!FreeQ[m, LorentzIndex[mu2, ___]]) &&
		(FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/. LorentzIndex -> lorhipa]) && ordq[m, {mu2,mu1}]);

double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___, LorentzIndex[mu2_, di2___], a3___,
	LorentzIndex[mu3_, di3___], a4___ ], j_ ] :=
	Block[ {dte,a,b,c},
		dte = (  (m/.mu1->$MU[j]/.mu2->$MU[j+1]/.mu3->$MU[j+2]) *
			Eps[a1,LorentzIndex[$MU[j],di1], a2,
					LorentzIndex[$MU[j+1],di2],a3,
					LorentzIndex[$MU[j+2],di3], a4]);
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

double3[ m_. eepp[LorentzIndex[mu1_,di1___],LorentzIndex[mu2_,di2___],
					LorentzIndex[mu3_,di3___],LorentzIndex[mu4_,di4___]], _ ] :=
	Block[ {dte,a,b,c,d},
		dte = (m/.mu1->$MU[1]/.mu2->$MU[2]/.mu3->$MU[3]/.mu4->$MU[4]) *
		Eps[LorentzIndex[$MU[1],di1],  LorentzIndex[$MU[2],di2],
			LorentzIndex[$MU[3],di3], LorentzIndex[$MU[4],di4]];
		a = $MU[1];
		b = $MU[2];
		c = $MU[3];
		d = $MU[4];
		Which[
		ordq[m, {mu1, mu2, mu3, mu4}], ReplaceAll[dte,
			{a -> a, b -> b, c -> c, d -> d}],
		ordq[m, {mu1, mu2, mu4, mu3}],
		ReplaceAll[dte, {a -> a, b -> b, c -> d, d -> c}],
		ordq[m, {mu1, mu3, mu2, mu4}], ReplaceAll[dte,
			{a -> a, b -> c, c -> b, d -> d}], ordq[m, {mu1, mu3, mu4, mu2}],
		ReplaceAll[dte, {a -> a, b -> c, c -> d, d -> b}],
		ordq[m, {mu1, mu4, mu2, mu3}], ReplaceAll[dte,
			{a -> a, b -> d, c -> b, d -> c}], ordq[m, {mu1, mu4, mu3, mu2}],
		ReplaceAll[dte, {a -> a, b -> d, c -> c, d -> b}],
		ordq[m, {mu2, mu1, mu3, mu4}], ReplaceAll[dte,
			{a -> b, b -> a, c -> c, d -> d}], ordq[m, {mu2, mu1, mu4, mu3}],
		ReplaceAll[dte, {a -> b, b -> a, c -> d, d -> c}],
		ordq[m, {mu2, mu3, mu1, mu4}], ReplaceAll[dte,
			{a -> b, b -> c, c -> a, d -> d}], ordq[m, {mu2, mu3, mu4, mu1}],
		ReplaceAll[dte, {a -> b, b -> c, c -> d, d -> a}],
		ordq[m, {mu2, mu4, mu1, mu3}], ReplaceAll[dte,
			{a -> b, b -> d, c -> a, d -> c}], ordq[m, {mu2, mu4, mu3, mu1}],
		ReplaceAll[dte, {a -> b, b -> d, c -> c, d -> a}],
		ordq[m, {mu3, mu1, mu2, mu4}], ReplaceAll[dte,
			{a -> c, b -> a, c -> b, d -> d}], ordq[m, {mu3, mu1, mu4, mu2}],
		ReplaceAll[dte, {a -> c, b -> a, c -> d, d -> b}],
		ordq[m, {mu3, mu2, mu1, mu4}], ReplaceAll[dte,
			{a -> c, b -> b, c -> a, d -> d}], ordq[m, {mu3, mu2, mu4, mu1}],
		ReplaceAll[dte, {a -> c, b -> b, c -> d, d -> a}],
		ordq[m, {mu3, mu4, mu1, mu2}], ReplaceAll[dte,
			{a -> c, b -> d, c -> a, d -> b}], ordq[m, {mu3, mu4, mu2, mu1}],
		ReplaceAll[dte, {a -> c, b -> d, c -> b, d -> a}],
		ordq[m, {mu4, mu1, mu2, mu3}], ReplaceAll[dte,
			{a -> d, b -> a, c -> b, d -> c}], ordq[m, {mu4, mu1, mu3, mu2}],
		ReplaceAll[dte, {a -> d, b -> a, c -> c, d -> b}],
		ordq[m, {mu4, mu2, mu1, mu3}], ReplaceAll[dte,
			{a -> d, b -> b, c -> a, d -> c}], ordq[m, {mu4, mu2, mu3, mu1}],
		ReplaceAll[dte, {a -> d, b -> b, c -> c, d -> a}],
		ordq[m, {mu4, mu3, mu1, mu2}], ReplaceAll[dte,
			{a -> d, b -> c, c -> a, d -> b}], ordq[m, {mu4, mu3, mu2, mu1}],
		ReplaceAll[dte, {a -> d, b -> c, c -> b, d -> a}]
		]
	] /; (!FreeQ2[m, LorentzIndex[mu1,___]] &&
		!FreeQ2[m, LorentzIndex[mu2,___]] &&
		!FreeQ2[m, LorentzIndex[mu3,___]] &&
		!FreeQ2[m, LorentzIndex[mu4,___]]
	) && FreeQ2[m, {$MU[1], $MU[2], $MU[3], $MU[4]}];

FCPrint[1,"Contract.m loaded."];
End[]
