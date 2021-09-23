(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSimplify										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Simplifies loop integrals by doing shifts and detects
				integrals that vanish by symmetry.							*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSimplify::usage =
"FeynAmpDenominatorSimplify[exp] tries to simplify each PropagatorDenominator
in a canonical way. FeynAmpDenominatorSimplify[exp, q1] simplifies all
FeynAmpDenominators in exp in a canonical way, including momentum shifts.
Scaleless integrals are discarded.";

FDS::usage =
"FDS is shorthand for FeynAmpDenominatorSimplify.";

FDS::twoloopsefail =
"FDS detected a fatal error while converting the 2-loop self energy integral `1` \n
into canonicla form. Evaluation aborted!"

FDS::failmsg = "Error! FDS has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

DetectLoopTopologies::usage=
"DetectLoopTopologies is an option for FDS. If set to True, FDS will try to
recognize some special multiloop topologies and apply appropriate
simplifications";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

lenso
feynsimp
feynord

End[]

Begin["`FeynAmpDenominatorSimplify`Private`"]

fdsVerbose::usage="";
loopHead::usage="";
cLoopHead::usage="";
sLoopHead::usage="";
mLoopHead::usage="";
null1::usage="";
null2::usage="";

FDS = FeynAmpDenominatorSimplify;

Options[FeynAmpDenominatorSimplify] = {
	ApartFF 					-> False,
	Collecting 					-> True,
	DetectLoopTopologies		-> True,
	ExpandScalarProduct			-> True,
	FCE							-> False,
	FCI							-> False,
	FCVerbose					-> False,
	Factoring					-> Factor,
	FeynAmpDenominatorCombine	-> True,
	IntegralTable 				-> {},
	Rename 						-> True
};

SetAttributes[FeynAmpDenominatorSimplify, Listable];

FeynAmpDenominatorSimplify[expr_, qs___/;FreeQ[{qs},Momentum], opt:OptionsPattern[]] :=
	Block[ {ex,rest,loopInts,intsUnique, solsList,res,repRule,time, topoCheckUnique,
		topoCheck, multiLoopHead, solsList2, intsTops, intsTops2, optCollecting, power, fadList,
		fadListEval, fadHold, intsUniqueClassified},

		If [OptionValue[FCVerbose]===False,
			fdsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fdsVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!FreeQ2[$ScalarProducts, {qs}],
			Message[FDS::failmsg, "Some loop momenta have scalar product rules attached to them. Evaluation aborted!"];
			Abort[]
		];


		optCollecting = OptionValue[Collecting];

		If[ !OptionValue[FCI],
			ex = FeynCalcInternal[expr],
			ex = expr
		];

		FCPrint[1,"FDS: Entering FDS. ", FCDoControl->fdsVerbose];
		FCPrint[3,"FDS: Entering with: ", ex, FCDoControl->fdsVerbose];

		If[ FreeQ[ex,FeynAmpDenominator],
			FCPrint[1,"FDS: Nothing to do.", FCDoControl->fdsVerbose];
			Return[ex]
		];

		ex = ex /. Power[FeynAmpDenominator[a__], b_ ] :> power[fadHold[a],b];

		fadList = Cases2[ex+null1+null2,FeynAmpDenominator,power];

		fadListEval = fadList /. {power[fadHold[a__],n_] /; n>1 :> FeynAmpDenominator[Sequence@@(Table[a,{i,1,n}])]} /.
			power -> Power /. fadHold -> FeynAmpDenominator;

		If[	!FreeQ[fadListEval, PD],
			fadListEval = fadListEval /. PD -> procan /. procan -> PD
		];

		If[	!FreeQ[fadListEval, StandardPropagatorDenominator],
			fadListEval = fadListEval /. StandardPropagatorDenominator ->  procanSFAD /.  procanSFAD -> StandardPropagatorDenominator
		];

		If[	!FreeQ[fadListEval, CartesianPropagatorDenominator],
			fadListEval = fadListEval /. CartesianPropagatorDenominator ->  procanCFAD /.  procanCFAD -> CartesianPropagatorDenominator
		];

		FCPrint[3,"FDS: After fixing the signs of squared momenta in the propagators: ", fadListEval, FCDoControl->fdsVerbose];

		If[ Length[{qs}]===0,
			FCPrint[1,"FDS: No loop momenta were given.", FCDoControl->fdsVerbose];
			fadListEval = fadListEval /. FeynAmpDenominator -> feyncan;
			repRule = Thread[Rule[fadList,fadListEval]];
			Return[ex /.  Dispatch[repRule]]
		];

		repRule = Thread[Rule[fadList,fadListEval]];
		ex = ex /.  Dispatch[repRule];

		FCPrint[3,"FDS: New ex: ", ex, FCDoControl->fdsVerbose];


		(*	Extract unique loop integrals	*)
		FCPrint[1,"FDS: Extracting unique loop integrals. ", FCDoControl->fdsVerbose];
		time=AbsoluteTime[];

		(*TODO Need to speed this up for cases where there is only one single FAD! *)
		(*TODO Drop scaleless for 1-loop??? *)
		(* Here we can exploit the possible factorization in a multiloop integral *)

		{rest,loopInts,intsUnique} = FCLoopExtract[ex, {qs},loopHead, FCI->True, PaVe->False, FCLoopBasisSplit->True,
			ExpandScalarProduct->True, Full->False, GFAD->False, CFAD->True, SFAD->True];

		FCPrint[1, "FDS: Done extracting unique loop integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdsVerbose];
		FCPrint[1, "FDS: Number of the unique integrals: ", Length[intsUnique], FCDoControl->fdsVerbose];
		FCPrint[3, "FDS: List of unique integrals ", intsUnique, FCDoControl->fdsVerbose];

		(* All the mixed integrals will be ignored *)
		intsUniqueClassified = integralClassify/@intsUnique;

		FCPrint[3, "FDS: List of the unique integrals after their classification ", intsUniqueClassified, FCDoControl->fdsVerbose];

		loopInts = loopInts /. Thread[Rule[intsUnique,intsUniqueClassified]];
		intsUnique = intsUniqueClassified;

		FCPrint[1,"FDS: Simplifying the unique integrals. ", FCDoControl->fdsVerbose];
		time=AbsoluteTime[];
		solsList = intsUnique /. {
			(loopHead|sLoopHead|cLoopHead)[z_,{l_}] :> fdsOneLoop[z,l],
			mLoopHead[z_,{l_}] :> (ToSFAD[z,FCI->True] /. FeynAmpDenominator :> feynord[{l}]),
			loopHead[z_,{l1_,l2_}] :> multiLoopHead[oldFeynAmpDenominatorSimplify[z,l1,l2,opt]],
			loopHead[z_,{l1_,l2_, l3__}] :> multiLoopHead[fdsMultiLoop[z,l1,l2,l3]],
			sLoopHead[z_,{_,__}] :> z,
			cLoopHead[z_,{_,__}] :> z
		};

		solsList = (FeynAmpDenominatorCombine[#]/. FeynAmpDenominator -> feynord[{qs}])&/@solsList;

		FCPrint[1, "FDS:Done simpifying the unique loop integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdsVerbose];
		FCPrint[3, "FDS: List of the simplified integrals ", solsList, FCDoControl->fdsVerbose];

		If[!FreeQ[solsList,fdsOneLoop],
			Message[FDS::failmsg,"fdsOneLoop couldn't be applied to some of the unique integrals."];
			Abort[]
		];

		(*	Tailored simplifications for speical 2-loop topologies	*)
		If[ OptionValue[DetectLoopTopologies] && !FreeQ[solsList,multiLoopHead] && FreeQ2[solsList,{StandardPropagatorDenominator,CartesianPropagatorDenominator}] ,
			FCPrint[3, "FDS: Simplifying special topologies.", FCDoControl->fdsVerbose];


			(* Isolate single loop integrals in every solution *)
			solsList = solsList /.
				{multiLoopHead[x__]:> Map[topoCheck,Expand2[x,FeynAmpDenominator]+null1+null2]}/. topoCheck[null1|null2] -> 0;

			topoCheckUnique = Cases[solsList,topoCheck[x__],Infinity]//Sort//DeleteDuplicates;
			FCPrint[3, "FDS: Unique terms ", topoCheckUnique, FCDoControl->fdsVerbose];


			intsTops=getTopology[#,{qs}]&/@(topoCheckUnique/.topoCheck->Identity);
			FCPrint[3, "FDS: Topologies ", intsTops, FCDoControl->fdsVerbose];

			intsTops2 = checkTopology/@intsTops;
			FCPrint[3, "FDS: Checked topologies ", intsTops2, FCDoControl->fdsVerbose];

			If[ intsTops2=!={},
				solsList2 = MapIndexed[
						(* Here one could add more topologies if needed *)
						Which[	#1==="2-loop-self-energy",
									(fds2LoopsSE[(First[topoCheckUnique[[#2]]]/.topoCheck->Identity),qs,(Total[intsTops[[#2]]])[[3]][[1]]]/. FeynAmpDenominator -> feyncan),
								#1==="generic",
									(First[topoCheckUnique[[#2]]]/. topoCheck->Identity),
								True,
									Message[FDS::failmsg,"Unknown 2-loop topology!"];
									Abort[]

						]&,intsTops2],
				solsList2= {}
			];

			solsList = solsList /. Thread[Rule[topoCheckUnique,solsList2]],

			(* Otherwise just remove the multiLoopHead heads *)
			solsList = solsList/.multiLoopHead->Identity
		];

		repRule = Thread[Rule[intsUnique,solsList]];
		FCPrint[3, "FDS: Replacement rule: ", repRule, FCDoControl->fdsVerbose];

		(*	Substitute the simplified integrals back into the original expression	*)
		res = rest + (loopInts/.repRule);

		(*
			Some integrals might be identical up to a renaming of the loop momenta, e.g.
			1/[(q1^2-m1^2)(q2^2-m2^2)] and 1/[(q2^2-m1^2)(q1^2-m2^2)]. In the following
			we check all possible exchanges of the loop momentum variables and sort them
			canonically.
		*)

		If[	OptionValue[Rename] && Length[{qs}]>=2,

			time=AbsoluteTime[];
			FCPrint[1, "FDS: fdsMultiLoop: Checking symmetries under renamings of the loop momenta.", FCDoControl->fdsVerbose];

			(* 	The option FeynAmpDenominatorCombine effectively decides whether the detected factorization of a loop integral
				will be preserved (FeynAmpDenominatorCombine->False) or broken (FeynAmpDenominatorCombine->True) *)
			{rest,loopInts,intsUnique} = FCLoopExtract[res, {qs},loopHead, FCI->True, PaVe->False, FCLoopBasisSplit->False,
				ExpandScalarProduct->True, Full->False, FeynAmpDenominatorCombine->OptionValue[FeynAmpDenominatorCombine]];

			solsList = intsUnique /. loopHead[x_]:> renameLoopMomenta[x,{qs}];
			repRule = Thread[Rule[intsUnique,solsList]];

			res = rest + (loopInts/.repRule);
			FCPrint[1, "FDS: Done checking symmetries under renamings of the loop momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdsVerbose];
			FCPrint[3, "FDS: fdsMultiLoop: After the renaming of the loop momenta: ", res, FCDoControl->fdsVerbose]
		];

		(*TODO: DO NOT EXPAND IN THE LOOP MOMENTA!!!! *)
		If [OptionValue[ExpandScalarProduct],
			FCPrint[1,"FDS: Applying ExpandScalarProduct. ", FCDoControl->fdsVerbose];
			time=AbsoluteTime[];
			res = ExpandScalarProduct[res, FCI->True];
			FCPrint[1, "FDS: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdsVerbose];
			FCPrint[3, "FDS: After ExpandScalarProduct: ", res, FCDoControl->fdsVerbose]
		];

		If[	optCollecting=!=False,
			FCPrint[1,"FDS: Applying Collect2. ", FCDoControl->fdsVerbose];
			time=AbsoluteTime[];


			If[ TrueQ[optCollecting===True],
				res = Collect2[res,FeynAmpDenominator, Factoring->OptionValue[Factoring]],
				res = Collect2[res,optCollecting, Factoring->OptionValue[Factoring]]
			];

			FCPrint[1, "FDS: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdsVerbose];
			FCPrint[3, "FDS: After Collect2: ", res, FCDoControl->fdsVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FDS: Leaving with: ", res, FCDoControl->fdsVerbose];
		res
	];



renameLoopMomenta[int_,qs_List]:=
	Block[{	permutations,listOfRenamings,equivalentInts,
			lmoms,nLoops,rule,res},

		lmoms = Select[qs,!FreeQ[int,#]&];
		nLoops = Length[lmoms];

		permutations = Permute[lmoms, PermutationGroup[Cycles[{#}]&/@Subsets[Range[nLoops], {2, nLoops}]]];

		If[	permutations==={{}},
			Return[int]
		];

		listOfRenamings=Sort[Thread[rule[Table[lmoms, {i,1,Length[permutations]}], permutations]] /.rule[x_List,y_List] :> Thread[rule[x,y]] /.
			rule[a_, a_] :> Unevaluated[Sequence[]]] /. rule -> Rule;

		equivalentInts = Map[(int/. # /. FeynAmpDenominator -> feynsimp[lmoms] /. FeynAmpDenominator -> feynord[lmoms])&, listOfRenamings];

		res = equivalentInts//Sort//First;

		res
	];

procan[a_, m_] :=
	Block[{tt, one, numfac},
		tt = Factor2[one MomentumExpand[a]];
		numfac = NumericalFactor[tt];
		If[TrueQ[numfac < 0 && MatchQ[numfac, _Rational | _Integer]],
			PD[Expand[-tt /. one -> 1], m],
			PD[Expand[tt /. one -> 1], m]
		]
	];


procanSFAD[0, rest__]:=
	StandardPropagatorDenominator[0, rest];

procanCFAD[0, rest__]:=
	CartesianPropagatorDenominator[0, rest];


procanSFAD[a_, rest__] :=
	Block[{tt, one, numfac},
		tt = Factor2[one MomentumExpand[a]];
		numfac = NumericalFactor[tt];
		If[	!FreeQ[numfac,Complex],
			Return[StandardPropagatorDenominator[Expand[tt /. one -> 1], rest]];
		];
		If[TrueQ[numfac < 0 && MatchQ[numfac, _Rational | _Integer]],
			StandardPropagatorDenominator[Expand[-tt /. one -> 1], rest],
			StandardPropagatorDenominator[Expand[tt /. one -> 1], rest]
		]
	]/; a=!=0;

procanCFAD[a_, rest__] :=
	Block[{tt, one, numfac},
		tt = Factor2[one MomentumExpand[a]];
		numfac = NumericalFactor[tt];
		If[	!FreeQ[numfac,Complex],
			CartesianPropagatorDenominator[Expand[tt /. one -> 1], rest]
		];
		If[TrueQ[numfac < 0 && MatchQ[numfac, _Rational | _Integer]],
			CartesianPropagatorDenominator[Expand[-tt /. one -> 1], rest],
			CartesianPropagatorDenominator[Expand[tt /. one -> 1], rest]
		]
	]/; a=!=0;

fdsOneLoop[loopInt : (_. FeynAmpDenominator[props__]), q_]:=
	Block[{tmp,res,tmpNew,solsList,repRule,sps,fad},

		(*
			The input of fdsOneLoop is guaranteed to contain
			only a signle 1-loop integral. This makes many things simpler.
		*)

		FCPrint[3, "FDS: fdsOneLoop: Entering with: ", loopInt, FCDoControl->fdsVerbose];

		(*
			Order the propagators such, that the massless propagator with the smallest
			number of the external momenta goes first. This is not the standard ordering,
			but it is useful as the first step to bring the integral into canonical form
		*)
		tmp = loopInt /. FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord2[{q}];
		FCPrint[3, "FDS: fdsOneLoop: After first ordering of the propagators: ", tmp, FCDoControl->fdsVerbose];

		(*	Integrals that are antisymmetric under q->-q are removed	*)
		tmp = removeAnitsymmetricIntegrals[tmp,{q}, {q}];

		If[	tmp===0,
			Return[0]
		];

		FCPrint[3, "FDS: fdsOneLoop: After removing antisymmetric integrals: ", tmp, FCDoControl->fdsVerbose];

		(*
			Special trick for same mass propagators to avoid terms like 1/[q^2-m^2] [(q-p)^2-m^2]^3 instead of
			1/[q^2-m^2]^3 [(q-p)^2-m^2]
		*)
		If[	!FreeQ[tmp,PropagatorDenominator],
			tmp = tmp/. {a_. FeynAmpDenominator[(v: PD[Momentum[q, dim___],m_]..),(w: PD[Momentum[q, dim___]+ pe_,m_]..), r___]/;
				FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] && m=!=0  :>
						((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.Momentum->extractm))} /.
						FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]] /.
						FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord[{q}]
		];

		If[	!FreeQ[tmp,StandardPropagatorDenominator],
			tmp = tmp/. {a_. FeynAmpDenominator[(v: StandardPropagatorDenominator[Momentum[q,dim___],0,m_,s_]..),
												(w: StandardPropagatorDenominator[Momentum[q,dim___]+ pe_,0,m_, s_]..), r___]/;
				FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] && m=!=0  :>
						((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.Momentum->extractm))} /.
						FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]] /.
						FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord[{q}]
		];

		If[	!FreeQ[tmp,CartesianPropagatorDenominator],
			tmp = tmp/. {a_. FeynAmpDenominator[(v: CartesianPropagatorDenominator[CartesianMomentum[q,dim___],0,m_,s_]..),
												(w: CartesianPropagatorDenominator[CartesianMomentum[q,dim___]+ pe_,0,m_, s_]..),r___]/;
				FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] && m=!=0  :>
						((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.CartesianMomentum->extractm))} /.
						FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]]/. FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord[{q}]
		];

		FCPrint[3, "FDS: fdsOneLoop: After using special trick for same mass propagators:  ", tmp, FCDoControl->fdsVerbose];

		(*
			Special trick for massless propagators to avoid terms like 1/q^2 [(q-p)^2]^3 instead of
			1/[q^2]^3 [(q-p)^2]
		*)
		If[ !FreeQ[tmp,PropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[(v: PD[Momentum[q,dim___],0]..),(w: PD[Momentum[q,dim___]+ pe_,0]..), r___]/;
			FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] :>
					((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.Momentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]] /.
					FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord2[{q}]
		];

		If[ !FreeQ[tmp,StandardPropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[(v: StandardPropagatorDenominator[Momentum[q,dim___],0,0,s_]..),
											(w: StandardPropagatorDenominator[Momentum[q,dim___]+ pe_,0,0,s_]..),r___]/;
			FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] :>
					((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.Momentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]] /.
					FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord2[{q}]
		];

		If[ !FreeQ[tmp,CartesianPropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[(v: CartesianPropagatorDenominator[CartesianMomentum[q,dim___],0,0,s_]..),
											(w: CartesianPropagatorDenominator[CartesianMomentum[q,dim___]+ pe_,0,0,s_]..), r___]/;
			FreeQ[pe,q] && pe=!=0 && Length[{v}] < Length[{w}] :>
					((a FeynAmpDenominator[v,w,r])/. q :> - q - (pe/.CartesianMomentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]] /.
					FeynAmpDenominator -> feynsimp[{q}] /. FeynAmpDenominator -> feynord2[{q}]
		];

		FCPrint[3, "FDS: fdsOneLoop: After using special trick for massless propagators:  ", tmp, FCDoControl->fdsVerbose];

		(*	Perform a shift to make the very first propagator free of external momenta	*)
		If[ !FreeQ[tmp, PropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[PD[Momentum[q, d___]+pe_, m_],r___]/; FreeQ[pe,q] :>
					((a FeynAmpDenominator[PD[Momentum[q, d]+pe, m], r])/. q :> q - (pe/.Momentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]];
		];

		If[ !FreeQ[tmp, StandardPropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, d___]+pe_,x_, m_, s_],r___]/; FreeQ[pe,q] :>
					((a FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, d]+pe, x, m, s], r])/. q :> q - (pe/.Momentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]];
		];

		If[ !FreeQ[tmp, CartesianPropagatorDenominator],
		tmp = tmp/. {a_. FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[q, d___]+pe_,x_, m_, s_], r___]/; FreeQ[pe,q] :>
					((a FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[q, d]+pe, x, m, s], r])/. q :> q - (pe/.CartesianMomentum->extractm))} /.
					FeynAmpDenominator[a__]:>MomentumExpand[FeynAmpDenominator[a]];
		];

		If[	tmp===0,
			Return[0]
		];

		(*	Prior to removing scaleless integrals we want to ensure that the loop integral is of a particular form	*)
		If[	!MatchQ[tmp, (a_. b_FeynAmpDenominator)/;FreeQ[a,FeynAmpDenominator]],
			Message[FeynAmpDenominatorSimplify::failmsg, "The loop integral is not in the expected form."];
			Abort[]
		];

		FCPrint[3, "FDS: fdsOneLoop: After shifting the very first propagator:  ", tmp, FCDoControl->fdsVerbose];

		{sps,fad} = FCProductSplit[tmp,{FeynAmpDenominator}];

		(* If FAD has no dependence on the loop momentum, the integral is clearly scaleless	*)
		If[	FreeQ[fad,q],
			Return[0]
		];


		(*
			An integral that contains only Cartesian eikonal propagators vanishes by symmetry q->-q, while
			massless Cartesian tadpoles vanish in DR. This is true also for mixed integrals.
		*)
		fad = fad /. {
			FeynAmpDenominator[x : CartesianPropagatorDenominator[0, _, _, {_, _}] ..] /; !FreeQ[{x}, CartesianMomentum[q, __]] -> 0,
			FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[q, ___], 0, 0, _]..] -> 0
		};

		(*
			It is not safe to remove certain scaleless integrals (like 1/q^2) if the integral depends both on
			Lorentzian and Cartesian quantities.
		*)
		If[	FCLoopMixedIntegralQ[sps*fad]===False,

			(*	An integral that contains only Lorentzian eikonal propagators vanishes by symmetry q->-q	*)
			fad = fad /. {FeynAmpDenominator[x : StandardPropagatorDenominator[0, _, _, {_, _}] ..] /; !FreeQ[{x}, Momentum[q, __]] -> 0};

			(*	Remove massless Lorentzian tadpoles (vanish in DR)	*)
			If[!$KeepLogDivergentScalelessIntegrals,
				fad = fad /. {
					FeynAmpDenominator[PD[Momentum[q,___], 0]..] -> 0,
					FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q,___], 0, 0 ,_]..] -> 0
				},
				If[	sps===1,
					fad = fad /. {
						FeynAmpDenominator[l:PD[Momentum[q,___], 0]..]/;Length[{l}]=!=2 -> 0,
						FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q,___], 0, 0 , {1,_}]..]/;Length[{l}]=!=2 :> 0
					}
				]
			]
		];

		tmp = sps*fad;

		FCPrint[3, "FDS: fdsOneLoop: After removing massless tadpoles:  ", tmp, FCDoControl->fdsVerbose];

		(*
			If the integral topology is more complicated than a massive tadpole, then
			we possibly need to do some shifts.
		*)
		If[ Length[Union[{props}]]>1 && FCLoopEikonalPropagatorFreeQ[tmp],
			FCPrint[3, "FDS: fdsOneLoop: The integral is at least a bubble:  ", tmp, FCDoControl->fdsVerbose];
			tmp=fdsOneLoopsGeneric[tmp,q]
		];

		FCPrint[3, "FDS: fdsOneLoop: After doing additional shifts: ", tmp, FCDoControl->fdsVerbose];

		If[	tmp===0,
			Return[0],
			res = tmp
		];
		FCPrint[3, "FDS: fdsOneLoop: res: ", res, FCDoControl->fdsVerbose];
		(*
			After the shifts our single integral usually turns into a sum of
			integrals with different numerators. Some of them might vanish by symmetry
		*)
		res = Expand2[ExpandScalarProduct[res,Momentum->{q},EpsEvaluate->True, Full->False, FCI->True],q];
		FCPrint[3, "FDS: fdsOneLoop: res: ", res, FCDoControl->fdsVerbose];

		tmpNew = FCLoopExtract[res, {q},loopHead, DropScaleless->True,FCI->True, PaVe->False,
			ExpandScalarProduct->True, Full->False, FCI->True ];
		FCPrint[3, "FDS: fdsOneLoop: tmpNew: ", tmpNew, FCDoControl->fdsVerbose];

		solsList = Map[removeAnitsymmetricIntegrals[#,{q}, {q}]&,(tmpNew[[3]]/.loopHead->Identity)];
		FCPrint[3, "FDS: fdsOneLoop: solsList: ", solsList, FCDoControl->fdsVerbose];


		repRule = Thread[Rule[tmpNew[[3]],solsList]];

		FCPrint[3, "FDS: fdsOneLoop: repRule: ", repRule, FCDoControl->fdsVerbose];

		res = tmpNew[[1]] + (tmpNew[[2]]/.repRule);
		FCPrint[3, "FDS: fdsOneLoop: res: ", res, FCDoControl->fdsVerbose];

		(*	Finally, order all the propagators canonically	*)
		res = res /. FeynAmpDenominator :> feynord[{q}];
		FCPrint[3, "FDS: fdsOneLoop: Final ordering: ", res, FCDoControl->fdsVerbose];
		FCPrint[3, "FDS: fdsOneLoop: Leaving with: ", res, FCDoControl->fdsVerbose];

		res
	]/; !FreeQ[{props},q];


(* 	Iterative algorithm tailored for 1-loop integrals that tries to determine the correct shifts
	to bring each integral to the "canonical" form. Pure heuristics, since without knowing the
	precise topology, there is no way to guess the true canonical form of the given integral.	*)
fdsOneLoopsGeneric[expr : (_. FeynAmpDenominator[props__]), q_] :=
	Block[ {prs, prs2, canonicalProps,shiftList,res},

		FCPrint[3, "FDS: fdsOneLoopsGeneric: Entering with ", expr, FCDoControl->fdsVerbose];

		(*	Convert the integral to a more suitable form, i.e. 1/[q^2-m1^2][(q+p)^2-m2^2]
			becomes  {q,q+p}	*)
		prs = Expand/@({props} /. {
			PropagatorDenominator[z_, _ : 0] :> (z /. Momentum[a_, _ : 4] :> a),
			StandardPropagatorDenominator[z_, _ ,_ ,_] :> (z /. Momentum[a_, _ : 4] :> a),
			CartesianPropagatorDenominator[z_, _ ,_ ,_] :> (z /. CartesianMomentum[a_, _ : 3] :> a)
		});

		(* List of possible canonical propagators *)
		canonicalProps = createOneLoopCanonicalPropsList[prs,q,True];

		(* The number of shifts is limited to the number of independent propagators reduced by 1 *)
		shiftList = FixedPoint[fdsOneLoopsShiftMaker[#[[1]],q,canonicalProps,#[[2]]]&,{prs,{}},Length[prs]-1][[2]];
		shiftList = shiftList/.Momentum->extractm;

		FCPrint[3, "FDS: fdsOneLoopsGeneric: List of the shifts to be applied: ", shiftList, FCDoControl->fdsVerbose];

		(* Apply the shifts succesively *)
		res = expr;
		If[	shiftList=!={},
			Do[res=MomentumExpand[res/.shiftList[[i]]/. FeynAmpDenominator :> feynsimp[{q}]],{i,Length[shiftList]}]
		];

		(*	After that, order all the propagators canonically	*)
		res = res /. FeynAmpDenominator :> feynord[{q}];

		(*	This is for cases where some of the external momenta have negative signs, such that we end up
			with sth like 1/[q^2(q-p1)^2(q-p2)^2(q+p3)^2(q+p4)^2]. In this case we have some freedom
			to chose whether we want to do the q->-q shift or not. Here we try to decide, if such a shift
			makes things look simpler	*)
		prs2=Cases[res+null1+null2,FeynAmpDenominator[pros__]:>
			Expand/@Union[{pros} /. PropagatorDenominator[z_, _ : 0] :>
				(z /. Momentum[a_, _ : 4] :> a)/.q->0 /. 0->Unevaluated[Sequence[]]],Infinity]//First;

		If[prs2=!={},

			(* Do the shift, if the number of "+"-terms is larger than the number of the "-" terms *)
			If[Length[Select[prs2,nufaQ]]>Length[Complement[prs2,Select[prs2,nufaQ]]],
				res=MomentumExpand[res/.q->-q/. FeynAmpDenominator :> feynsimp[{q}]]
			];

			(* If both numbers are equal, the relative sign in the second propagator decides *)
			If[Length[Select[prs2,nufaQ]]===Length[Complement[prs2,Select[prs2,nufaQ]]] && Length[prs2]>=2,
				If[nufaQ[prs2[[2]]],
					res=MomentumExpand[res/.q->-q/. FeynAmpDenominator :> feynsimp[{q}]]
				]
			]
		];

		FCPrint[3, "FDS: fdsOneLoopsGeneric: Leaving with ", res, FCDoControl->fdsVerbose];
		res
	];


(*	Chooses useful shifts to simplify the integral. Safe for memoization.	*)
fdsOneLoopsShiftMaker[prs_List, q_, canonicalProps_List, acceptedShifts_List] :=
	fdsOneLoopsShiftMaker[prs, q, canonicalProps, acceptedShifts]=
		Block[{needShift, newCanonicalProps, P, allShifts,goodShifts,shift,res,betterForm,weightedShifts,weightedShift},

			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: Entering with ", prs, " ", FCDoControl->fdsVerbose];
			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: Already accepted shifts ", acceptedShifts, " ", FCDoControl->fdsVerbose];

			needShift = Select[prs, (! checkOneLoopProp[#,canonicalProps]) &];
			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: Propagators that might need a shift ", needShift, FCDoControl->fdsVerbose];

			newCanonicalProps = Union[canonicalProps,createOneLoopCanonicalPropsList[prs,q,False]];
			allShifts = Flatten[Map[createOneLoopShiftingList[#,q,newCanonicalProps] &, needShift]];
			weightedShifts=Map[checkOneLoopShift[prs, #,q, Union[canonicalProps,newCanonicalProps]]&,allShifts]//Union;

			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: List of all shifts that produce canonical propagators (weighted) ",
				weightedShifts, FCDoControl->fdsVerbose];

			(* This is again pure heuristics. We always accept shifts that increase the number of canonical
			propagators. If this number remains unchanged, we still might accept the shift, provided that
			it makes the integral look more simple	*)
			goodShifts=Cases[Sort[Select[Map[checkOneLoopShift[prs, #,q, newCanonicalProps]&,allShifts],
				(#[[2]]>=0 && #[[4]] && If[#[[2]]===0,#[[3]]>=0,True] && (#[[2]]+#[[3]]>=0) )&],
				sortWeightedShifts],{x_,_,_,_}:>x];


			FCPrint[3, "fdsOneLoopsShiftMaker: List of shifts that increase the number of canonical propagators ",
				goodShifts, FCDoControl->fdsVerbose];

			(* Avoid redoing the same shifts multiple times or undoing the previous shift *)
			If[acceptedShifts=!={},
				goodShifts = Complement[goodShifts,{Last[acceptedShifts],
					Last[acceptedShifts]/.Rule[q,q+b_]:>Rule[q,q-Expand[b]]}];
			];

			(*	The list of shifts is custom sorted, so the first shift is supposed to be the best one	*)
			If[goodShifts=!={},
				shift = First[goodShifts],
				shift = {}
			];

			(* If there are no useful shifts, a sign change might also help *)
			If[	shift==={},
				weightedShift=checkOneLoopShift[prs, {q->-q},q, newCanonicalProps];
				If[	weightedShift[[2]]+weightedShift[[3]]>0 && weightedShift[[4]],
					shift = q->-q
				]
			];

			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: Chosen shift: ", shift, FCDoControl->fdsVerbose];

			res = {signFixOneLoop[Expand[prs/.shift],q],Flatten[Join[acceptedShifts,{shift}]]};
			FCPrint[3, "FDS: fdsOneLoopsShiftMaker: Leaving with ", res, FCDoControl->fdsVerbose];

			res
		];

(*	Checks if the given propagator is in the canonical form.
	Safe for memoization.	*)
checkOneLoopProp[x_, canonicalProps_]:=
	checkOneLoopProp[x, canonicalProps]=
		MatchQ[x, Alternatives @@ canonicalProps];

(*
	Generate some posssible canonical propagators out of the given
	list of all momenta and their prefactors. This is of course
	pure heuristics, since a priori we don't know how the momenta
	realy flow through the diagram...
	Safe for memoization.
*)
createOneLoopCanonicalPropsList[prs_,q_,ext_]:=
	createOneLoopCanonicalPropsList[prs,q,ext]=
		Block[{moms,extra,fu,res},
			fu[x_]:=
				Map[{q - #[[2]], q + #[[2]], q - Abs[#[[1]]] #[[2]], q + Abs[#[[1]]] #[[2]]} &, x];

			If[ext===True,
				extra=Map[{#[[1]] + #[[2]], #[[1]] - #[[2]]} &,
					Subsets[(prs/.q->0/. 0->Unevaluated[Sequence[]]), {2}]]//Flatten//Union;
				extra=Map[{q-#,q+#}&,extra]//Flatten,
				extra = {}
			];
			moms = Map[(Thread[List[CoefficientArrays[#]//Normal//Rest//Total,Variables[#]]]) &, Rest[prs/.q->0]];
			res = Map[fu[#]&, moms]//Flatten;
			res = Join[{q},Union[Join[extra,res]]]
		];


(*	Determines the sign of the 1st external momentum in each propagator. Safe for memoization.	*)
nufaQ[_Momentum | _Symbol] :=
	True;

nufaQ[xx_Times]:=
	nufaQ[xx]=
		NumericalFactor[xx] > 0;

nufaQ[xx_Plus]:=
	nufaQ[xx]=
		NumericalFactor[xx[[1]]] > 0;


(* Fixes (-q+p)^2 to (q-p)^2. Safe for memoization.	*)
signFixOneLoop[prs_List,q_]:=signFixOneLoop[prs,q]=
	Map[If[!FreeQ[#,-q],Expand[-#],#]&,prs];

(*	Tries to quantify shits according to the way how they make the integral simpler.

	First quantifier is the diference between the number of propagators in the canonical form
	after and before the shift. The larger this value is, the better. Negative value means that
	the shift makes things only worse by decreasing the number of canonical propagators


	Second quantifier is the diference between the total number of external momenta in all the propagators
	before and after the shift. The larger this value is, the better. Negative value means that
	the shift makes things only worse by increasing the total number of external momenta, such that
	the integral looks more complicated than it actually is.

	The last quantifier is a check, whether after the shift the integral still contains a propagator
	of type 1/(q^2-m^2). If it evaluates to False, the shift is apriori not useful.

	Safe for memoization.
*)
checkOneLoopShift[prs_List,shift_,q_, canonicalProps_]:=
	checkOneLoopShift[prs,shift,q, canonicalProps]=
		Block[{before,after,qPropAvailable,nCanProps,propComplexity},
			before = Select[prs, (checkOneLoopProp[#,canonicalProps]) &];
			after = Select[signFixOneLoop[prs /. shift,q], (checkOneLoopProp[#,canonicalProps]) &];
			nCanProps = Length[after]-Length[before];
			qPropAvailable = MemberQ[Expand[signFixOneLoop[prs /. shift,q]], q];
			propComplexity = Total[NTerms/@prs] - Total[NTerms/@signFixOneLoop[prs /. shift,q]];
			{shift,nCanProps,propComplexity,qPropAvailable}
		];

(* Determines the shift needed to arrive to the particular propagator. Safe for memoization.	*)
createOneLoopShiftingList[xx_, q_, canonicalProps_] :=
	createOneLoopShiftingList[xx, q, canonicalProps]=
		Block[{dummyP},
			Map[Solve[(xx /. q -> dummyP) == #, dummyP] &, canonicalProps] /. dummyP -> q
		];

(* Sorts the list of possible shifts according to the given criteria	*)
sortWeightedShifts[{x1_,a1_,b1_,x2_},{y1_,a2_,b2_,y2_}]:=
	sortWeightedShifts[{x1,a1,b1,x2},{y1,a2,b2,y2}]=
		Which[

			x2===True && y2===False,
				True,
			x2===False && y2===True,
				False,
			x2===y2,
				If[	a1+b1=!=a2+b2,
					a1+b1>a2+b2,
					b1>b2
				],
			True,
			Message[FDS::failmsg,"Can't sort the list of shifts."];
			Abort[]
		];


(*	Checks if the integral vanishes by symmetry (multiloop version).
	Memoization should be done via FCUseCache	*)
removeAnitsymmetricIntegrals[int_, qmin_List, qs_List]:=
	If[	TrueQ[Expand[(MomentumExpand[EpsEvaluate[int/.Thread[Rule[qmin, -qmin]] ,FCI->True]] /.
		FeynAmpDenominator -> feynsimp[qs] /. FeynAmpDenominator -> feynord2[qs])+int]===0],
		0,
		int
	]/; int=!=0;

removeAnitsymmetricIntegrals[0,{__}, {__}]:=
	0;

(*
	Sort propagators in a FeynAmpDenominator.
	Propagators with the smallest number of momenta go first. If the number of
	momenta is the same, do the lexicographic ordering of the momenta. If the
	momenta are the same, do the lexicographic ordering of the masses.

	Safe for memoization. The second slots of SFADs and CFADs are not an issue,
	as no momentum expansions are involved.
*)
lenso[PD[x_, m1_], PD[y_, m2_]]:=
	lenso[PD[x, m1], PD[y, m2]]=
		Which[
			NTerms[x] =!= NTerms[y],
				NTerms[x] < NTerms[y],
			NTerms[x] === NTerms[y],
				If[	x=!=y,
					OrderedQ[{x,y}],
					OrderedQ[{m1,m2}]
				],
			True,
			Message[FDS::failmsg,"Can't sort the list of propagators."];
			Abort[]
		];

lenso[	(h1:StandardPropagatorDenominator|CartesianPropagatorDenominator)[x1_, x2_, m1_, s1_],
		(h2:StandardPropagatorDenominator|CartesianPropagatorDenominator)[y1_, y2_, m2_, s2_]]:=
	lenso[	h1[x1, x2, m1, s1], h2[y1, y2, m2, s2]]=
		Which[
			h1===StandardPropagatorDenominator && h2===CartesianPropagatorDenominator,
				True,
			h1===CartesianPropagatorDenominator && h2===StandardPropagatorDenominator,
				False,
			NTerms[x1+x2] =!= NTerms[y1+y2],
					NTerms[x1+x2] < NTerms[y1+y2],
				NTerms[x1+x2] === NTerms[y1+y2],
					If[	x1+x2=!=y1+y2,
						OrderedQ[{x1+x2,y1+y2}],
						OrderedQ[{m1,m2}]
					],
			True,
			Message[FDS::failmsg,"Can't sort the list of propagators."];
			Abort[]
		];


(*
	Special sorting for fdsOneLoopGeneral. The massless propagator with the
	smallest number of external momenta is put first. Safe for memoization.
	The second slots of SFADs and CFADs are not an issue, as no momentum
	expansions are involved.
*)
lenso2[PD[x_, m1_], PD[y_, m2_]]:=
	lenso2[PD[x, m1], PD[y, m2]]=
		Which[
			m1=!=0 && m2=!=0,
				OrderedQ[{m1,m2}],
			m1===0 && m2=!=0,
				True,
			m1=!=0 && m2===0,
				False,
			m1===0 && m2===0,
				If[ NTerms[x] < NTerms[y],
						True,
						If[ NTerms[x] === NTerms[y],
							OrderedQ[{x,y}],
							False
						],
						False
				],
			True,
			Message[FDS::failmsg,"Can't sort the list of propagators."];
			Abort[]
		];

lenso2[	(h1:StandardPropagatorDenominator|CartesianPropagatorDenominator)[x1_, x2_, m1_, s1_],
		(h2:StandardPropagatorDenominator|CartesianPropagatorDenominator)[y1_, y2_, m2_, s2_]]:=
	lenso2[	h1[x1, x2, m1, s1], h2[y1, y2, m2, s2]]=
		Which[
			h1===StandardPropagatorDenominator && h2===CartesianPropagatorDenominator,
				True,
			h1===CartesianPropagatorDenominator && h2===StandardPropagatorDenominator,
				False,
			m1=!=0 && m2=!=0,
				OrderedQ[{m1,m2}],
			m1===0 && m2=!=0,
				True,
			m1=!=0 && m2===0,
				False,
			m1===0 && m2===0,
				If[ NTerms[x1+x2] < NTerms[y1+y2],
						True,
						If[ NTerms[x1+x2] === NTerms[y1+y2],
							OrderedQ[{x1+x2,y1+y2}],
							False
						],
						False
				],
			True,
			Message[FDS::failmsg,"Can't sort the list of propagators."];
			Abort[]
		];

(*
	Sort the propagators into some canonical order. Safe for memoization.
	The second slots of SFADs and CFADs are not an issue, as no momentum
	expansions are involved.
*)
feynord[a__] :=
	MemSet[feynord[a],
		FeynAmpDenominator @@ Sort[{a}, lenso]
	]/;!FreeQ2[{a},{PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}];

feynord[qs_List][a__] :=
	MemSet[feynord[qs][a],
		FeynAmpDenominator @@
		Join[Sort[SelectNotFree[{a}, Sequence@@qs], lenso], Sort[SelectFree[{a}, Sequence@@qs], lenso]]
	]/;!FreeQ2[{a},{PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}];

feynord2[qs_List][a__] :=
	MemSet[feynord2[qs][a],
		FeynAmpDenominator @@
		Join[Sort[SelectNotFree[{a}, Sequence@@qs], lenso2], Sort[SelectFree[{a}, Sequence@@qs], lenso2]]
	]/;!FreeQ2[{a},{PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}];

(* 	Replaces 1/[(-q+x)^2-m^2] by 1/[(q-x)^2-m^2] (multiloop version).

	Safe for memoization despite MomentumExpand, since it is applied only
	inside FeynAmpDenominator, where feynsimpthere are no Pair's	*)

feynsimpAux[q_,pe_,di_,lmoms_,head_]:=
	q===First[Sort[Intersection[Cases[-head[q,di] + pe + null1 + null2, head[qq_, ___] :> qq,Infinity], lmoms]]]

feynsimp[lmoms_List][a__PD] :=
	MemSet[feynsimp[lmoms][a],
		Block[{pd},
		FeynAmpDenominator@@(Expand[MomentumExpand[{a}]] //. {
			PD[-Momentum[q_,di_:4] + pe_:0, m_]/; MemberQ[lmoms,q] && FreeQ2[pe,lmoms] :> pd[Momentum[q,di] - pe, m],
			PD[-Momentum[q_,di_:4] + pe_:0, m_]/; MemberQ[lmoms,q] && feynsimpAux[q, pe, di,lmoms, Momentum] :>
				pd[Momentum[q,di] - pe, m]
		} /. pd -> PD)
		]
	];

(*	If there are not only PropagatorDenominators inside FeynAmpDenominator, then
	the memoization is not safe anymore! *)
feynsimp[lmoms_List][a__ /; !MatchQ[{a},{__PD}](* && FreeQ[{a},GenericPropagatorDenominator]*)] :=
	Block[{pd, stpd, cpd},
		FeynAmpDenominator@@(Expand[MomentumExpand[{a}]] //. {

			PD[-Momentum[q_,di_:4] + pe_:0, m_]/; MemberQ[lmoms,q] && FreeQ2[pe,lmoms] && scetPropagatorFreeQ[{a}, q]  :>
				pd[Momentum[q,di] - pe, m],

			PD[-Momentum[q_,di_:4] + pe_:0, m_]/; MemberQ[lmoms,q] && feynsimpAux[q, pe, di,lmoms, Momentum]
				&& scetPropagatorFreeQ[{a}, q]  :>
				pd[Momentum[q,di] - pe, m],

			StandardPropagatorDenominator[-Momentum[q_,di_:4] + pe_:0, x1_, x2_, x3_]/; MemberQ[lmoms,q] && FreeQ2[pe,lmoms] && scetPropagatorFreeQ[{a}, q]  :>
				stpd[Momentum[q,di] - pe, (x1 /. q -> - q) , x2, x3],

			StandardPropagatorDenominator[-Momentum[q_,di_:4] + pe_:0, x1_, x2_, x3_]/; MemberQ[lmoms,q] && feynsimpAux[q, pe, di,lmoms, Momentum] && scetPropagatorFreeQ[{a}, q]  :>
				stpd[Momentum[q,di] - pe, (x1 /. q -> - q) , x2, x3],


			CartesianPropagatorDenominator[-CartesianMomentum[q_,di_:3] + pe_:0, x1_, x2_, x3_]/; MemberQ[lmoms,q] && FreeQ2[pe,lmoms] && scetPropagatorFreeQ[{a}, q]  :>
				cpd[CartesianMomentum[q,di] - pe, (x1 /. q -> - q) , x2, x3],

			CartesianPropagatorDenominator[-CartesianMomentum[q_,di_:3] + pe_:0, x1_, x2_, x3_]/; MemberQ[lmoms,q] && feynsimpAux[q, pe, di,lmoms, CartesianMomentum] && scetPropagatorFreeQ[{a}, q]  :>
				cpd[CartesianMomentum[q,di] - pe, (x1 /. q -> - q) , x2, x3]


		} /. {pd -> PD, stpd -> StandardPropagatorDenominator, cpd -> CartesianPropagatorDenominator})
	];

(*
	Check for the presence of the propagators like 1/(p.q+I eta) where the shift q-> -q
	would yields 1/(p.q+I eta) ==== 1/(-p.q+I eta) =!= -1/(p.q+I eta)
*)
scetPropagatorFreeQ[{a__}, l_]:=
	(Cases[{a}, (StandardPropagatorDenominator|CartesianPropagatorDenominator)[0, x_ /; ! FreeQ[x, l], _, _], Infinity] === {})


fdsMultiLoop[loopInt : (_. FeynAmpDenominator[props__]), qs__]:=
	Block[{	tmp,res,tmpNew,solsList,repRule},

		(*	The input of fdsMultiLoop is guaranteed to contain
			only a single loop integral. This makes many things simpler.	*)

		tmp = loopInt;

		FCPrint[3, "FDS: fdsMultiLoop: Entering with: ", tmp, FCDoControl->fdsVerbose];


		(* 	Order the propagators such, that the massless propagator with the smallest
			number of the external momenta goes first. This is not the standard ordering,
			but it is useful as the first step to bring the integral into canonical form	*)
		tmp = tmp /. FeynAmpDenominator -> feynsimp[{qs}] /. FeynAmpDenominator -> feynord2[{qs}];
		FCPrint[3, "FDS: fdsMultiLoop: After first ordering of the propagators: ", tmp, FCDoControl->fdsVerbose];

		(*	Integrals that are antisymmetric under qi->-qi are removed	*)
		tmp = Fold[removeAnitsymmetricIntegrals[#1, #2, {qs}] &, tmp, Subsets[{qs}]];

		FCPrint[3, "FDS: fdsMultiLoop: After removing antisymmetric integrals: ", tmp, FCDoControl->fdsVerbose];

		If[	tmp===0,
			Return[0],
			res = tmp
		];
		(*
		FCPrint[3, "FDS: fdsMultiLoop: res: ", res, FCDoControl->fdsVerbose];
		(* 	After the shifts our single integral usually turns into a sum of
			integrals with different numerators. Some of them might vanish by symmetry	*)

		res = Expand2[ExpandScalarProduct[res,Momentum->{qs},EpsEvaluate->True],qs];
		FCPrint[3, "FDS: fdsMultiLoop: res: ", res, FCDoControl->fdsVerbose];

		tmpNew = FCLoopExtract[res, {q},loopHead, DropScaleless->True,FCI->True, PaVe->False];
		FCPrint[3, "FDS: fdsOneLoop: tmpNew: ", tmpNew, FCDoControl->fdsVerbose];

		solsList = Map[removeAnitsymmetricIntegrals[#,q]&,(tmpNew[[3]]/.loopHead->Identity)];
		FCPrint[3, "FDS: fdsOneLoop: solsList: ", solsList, FCDoControl->fdsVerbose];

		repRule = Thread[Rule[tmpNew[[3]],solsList]];
		FCPrint[3, "FDS: fdsOneLoop: repRule: ", repRule, FCDoControl->fdsVerbose];

		res = tmpNew[[1]] + (tmpNew[[2]]/.repRule);
		FCPrint[3, "FDS: fdsOneLoop: res: ", res, FCDoControl->fdsVerbose];
		*)
		(*	Finally, order all the propagators canonically	*)
		res = res /. FeynAmpDenominator :> feynord[{qs}];
		FCPrint[3, "FDS: fdsMultiLoop: Final ordering: ", res, FCDoControl->fdsVerbose];
		FCPrint[3, "FDS: fdsMultiLoop: Leaving with: ", res, FCDoControl->fdsVerbose];

		res
	]/; !FreeQ2[{props},{qs}];

oldFeynAmpDenominatorSimplify[ex_, q1_, q2_/;Head[q2]=!=Rule, opt:OptionsPattern[]] :=
	Block[ {exp=ex, ot, pot,topi, topi2, bas, basic,res,pru,oneONE,fadalll,fadallq1q2,amucheck},

		ot = Flatten[OptionValue[Options[FDS],{opt},IntegralTable]];

		pru = (a_Plus)^(w_/;Head[w] =!= Integer) :>
		(PowerExpand[Factor2[oneONE*a]^w] /. oneONE -> 1);

		fadallq1q2[zy_Times, zq1_,zq2_] :=
			SelectFree[zy,{zq1,zq2}] fadalll[SelectNotFree[zy,{zq1,zq2}], zq1,zq2];

		fadalll[0,__] :=
			0;

		fadalll[zexp_Plus, zq1_, zq2_ ] :=
			Map[fadallq1q2[#, zq1,zq2]&,zexp] /. fadallq1q2 -> fadalll;

		fadalll[zexp_/;Head[zexp] =!= Plus, zq1_, zq2_ ] :=
			If[ !FreeQ[zexp, OPESum],
				(* to improve speed *)
				zexp /. FeynAmpDenominator -> amucheck[zq1,zq2] /.	amucheck ->  nopcheck,
				(* normal procedure *)
				translat[ zexp /. FeynAmpDenominator -> amucheck[zq1,zq2] /. amucheck ->  nopcheck,  zq1, zq2] /.
				FeynAmpDenominator -> feynsimp[{zq1}] /. FeynAmpDenominator -> feynsimp[{zq2}] /.
				FeynAmpDenominator -> feynord
			]  /. {
				((_. + _. Pair[Momentum[zq1,___],Momentum[OPEDelta,___]])^(vv_/; Head[vv] =!= Integer)) *
				FeynAmpDenominator[pr1___, PD[_. + Momentum[zq1,_:4], _], pr2___	] :> 0 /; FreeQ[{pr1, pr2}, zq1]
			};

		(*Seems that for some reason we are not allowed to touch those named blanks*)
		amucheck[k1_, _][PD[_. Momentum[k1_,___] + bb_. ,0].., b___] :=
			0 /; FreeQ[{b}, k1] && bb=!=0;

		amucheck[k1_, _][b___,PD[_. Momentum[k1_,___] + bb_. ,0]..] :=
			0 /; FreeQ[{b}, k1] && bb=!=0;

		amucheck[_, k2_][b___,PD[_. Momentum[k2_,___] + bb_. ,0]..] :=
			0 /; FreeQ[{b}, k2] && bb=!=0;

		amucheck[_, k2_][PD[_. Momentum[k2_,___] + bb_. ,0].., b___] :=
			0 /; FreeQ[{b}, k2] && bb=!=0;

		pe[qu1_,qu2_, prop_] :=
			Block[ {pet = SelectFree[Cases2[prop//MomentumExpand,Momentum]/.
			Momentum[a_,___]:>a, {qu1,qu2} ]},
				If[ Length[pet]>0,
					First[pet],
					pet
				]
			];

		basic = {
			(* stuff/(q2-q1) -> stuff'/(q1), where stuff is free of q2 *)
			FCIntegral[anyf_[a___, Momentum[q1,di_], b___]*
			FeynAmpDenominator[c___,pro:PD[Momentum[q2,di_]-Momentum[q1,di_],_].., d___]] :>
				Calc[(anyf[a,Momentum[q1,di], b] FeynAmpDenominator[c,pro,d]) /.q1-> -q1+q2] /;
				FreeQ[{a,anyf,b,c,d}, q1],

			(* stuff/(q2-q1) -> stuff'/(q2), where stuff is free of q1 *)
			FCIntegral[anyf_[a___, Momentum[q2,di_], b___]*
			FeynAmpDenominator[c___,pro:PD[Momentum[q2,di_]-Momentum[q1,di_],_].., d___]] :>
				Calc[(anyf[a,Momentum[q2,di], b] FeynAmpDenominator[c,pro,d]) /. q2-> -q2+q1] /;
				FreeQ[{a,anyf,b,c,d}, q2],

			(* stuff*q1.x/q1-m^2 vanishes by symmetry, where stuff is free of q1 *)
			FCIntegral[anyf_[a___,Momentum[q1,___], b___]*
			FeynAmpDenominator[ c___, PD[Momentum[q1,___], _]..,d___]] :>
			(FCPrint[3,"Amu 1"]; 0) /; FreeQ[{a,anyf,b,c,d},q1],

			(* stuff*q2.x/q2-m^2 vanishes by symmetry, where stuff is free of q2 *)
			FCIntegral[anyf_[a___,Momentum[q2,___], b___]*
			FeynAmpDenominator[ c___, PD[Momentum[q2,___], _]..,d___]] :>
			(FCPrint[3,"Amu 2"]; 0) /; FreeQ[{a,anyf,b,c,d},q2],


			FCIntegral[_. FeynAmpDenominator[aa__ ]] :>
			(FCPrint[3,"Amu 3"]; 0) /;
			(FreeQ[{aa}, PD[_,em_/;em=!=0]] &&
			((Sort[{q1,q2}] === (Cases2[{aa}//MomentumExpand, Momentum]	/. Momentum[a_, ___] :> a)) ||
			(Sort[{q1,q2}] === (SelectFree[Cases2[({aa}/.{q1 :> -q1+pe[q1,q2,{aa}]}/.
			{q2 :> -q2+pe[q1,q2,{aa}]})//MomentumExpand,Momentum],OPEDelta] /. Momentum[a_, ___] :> a)))),

			FCIntegral[_. FeynAmpDenominator[PD[Momentum[q1,___],0].., aa__]] :>
			(FCPrint[3,"Amu 4"]; 0) /; FreeQ[{aa},q1],
			(*NEW*)
			FCIntegral[any_. FeynAmpDenominator[aa__ ] ] :>
			Calc[(any FeynAmpDenominator[aa] ) /. {q1 :> -q1+pe[q1,q2,{aa}]} /.
			{q2 :> -q2+pe[q1,q2,{aa}]}]/;
			(!FreeQ[{aa}, PD[_,em_/;em=!=0]] && (((Sort[{q1,q2}]) === (
			(SelectFree[Cases2[({aa}/.{q1 :> (-q1+(pe[q1,q2,{aa}]))}/.
			{q2 :> -q2+pe[q1,q2,{aa}]})//MomentumExpand,Momentum],OPEDelta] /.
			Momentum[a_, ___] :> a))))),
			(*01 1999*)
			FCIntegral[any_. FeynAmpDenominator[aa___,
			PD[Momentum[pe_,dim_] + Momentum[q1, dim_] + Momentum[q2,dim_], em_],
			b___ ] ] :>
			Calc[(any FeynAmpDenominator[aa, PD[pe+Momentum[q1,dim]+Momentum[q2,dim],em],b]
			) /. q1->q1-q2] /;
			FreeQ[{a,b}, PD[_. Momentum[q1,_] + _. Momentum[pe,_],_]]
		};



		ot = Join[ot, basic];


		If[ ot =!= {},
			pot = ot /. Power2->Power;
			topi[y_ /; FreeQ2[y,{q1,q2,Pattern}]] :=
				y;

			topi[y_Plus] :=
				Map[topi,y];
			topi[y_Times] :=
				SelectFree[y,{q1,q2}] topi2[SelectNotFree[y,{q1,q2}]];
			FCPrint[3,"FDS: oldFeynAmpDenominatorSimplify: Before topi", exp, "", FCDoControl->fdsVerbose];
			exp = topi[exp] /. topi -> topi2 /. topi2[a_] :> FCIntegral[a];
			FCPrint[3,"FDS: oldFeynAmpDenominatorSimplify: After topi", exp, "", FCDoControl->fdsVerbose];
			exp = exp /. basic /. FCIntegral -> Identity;
			FCPrint[3,"FDS: oldFeynAmpDenominatorSimplify: After basic", exp, "", FCDoControl->fdsVerbose];
			exp = topi[exp] /. topi -> topi2 /. topi2[a_] :>
					FCIntegral[a//FeynCalcExternal];
			exp = exp /. ot /. ot /. pot /. pot /. FCIntegral[b_] :>
					FCI[b];
		];

		FCPrint[3,"FDS: oldFeynAmpDenominatorSimplify: Before fadall", exp, "", FCDoControl->fdsVerbose];

		If[ Head[exp] =!= Plus,
			res = FixedPoint[fadalll[#, q1, q2]&, Expand2[exp, q1], 7] /. pru,
			res = SelectFree[exp, {q1,q2}] + (FixedPoint[fadalll[#, q1, q2]&,	exp-SelectFree[exp,{q1,q2}], 7] /. pru)
		];

		FCPrint[3,"FDS: oldFeynAmpDenominatorSimplify: Before ApartFF", res, "", FCDoControl->fdsVerbose];

		If[ (ApartFF /. {opt} /. Options[FDS]),
			res = ApartFF[res,{q1,q2}]
		];

		FCPrint[1,"FDS: oldFeynAmpDenominatorSimplify: Leaving 2-loop FDS with", res, FCDoControl->fdsVerbose];
		res
	];


(* extracts the number of unique propagators, as well as the dependence on the loop and external momenta *)
getTopology[_. FeynAmpDenominator[props__],	lmoms_List] :=
	{Length[Union[{props}]], Select[lmoms,!FreeQ[{props},#]&], Union[Cases[MomentumExpand[{props}], Momentum[x_, _ : 4] /;
		FreeQ2[x, lmoms] :> x, Infinity]]} /; Length[Union[lmoms]] === Length[lmoms];

(* for some special topologies, where the outcome is known, one may use different simplification rules  *)
checkTopology[{prnum_, lmoms_List, emoms_List}] :=
	Which[	2 <= prnum <= 5 && Length[lmoms] === 2 && Length[emoms] === 1,
			"2-loop-self-energy",
			True,
			"generic"
	]

(* 	2-loop self-energy integrals can have at most 5 unique independent propagators, hence
	not more than 5 shifts are needed *)

fds2LoopsSE[expr_,q1_,q2_,_]:=
	expr/; FreeQ[expr,q1] || FreeQ[expr,q2];

fds2LoopsSE[expr_,q1_,q2_,p_]:=
	(MomentumExpand[FixedPoint[fds2LoopsSE2[#,q1,q2,p]&,expr,5]]/. {
		PropagatorDenominator[-Momentum[q1,d_:4], m_]:>PropagatorDenominator[Momentum[q1,d], m],
		PropagatorDenominator[-Momentum[q2,d_:4], m_]:>PropagatorDenominator[Momentum[q2,d], m],
		PropagatorDenominator[Momentum[p, d_:4]-Momentum[q1, e_:4], m_]:>PropagatorDenominator[Momentum[q1, e]-Momentum[p, d],m],
		PropagatorDenominator[Momentum[p, d_:4]-Momentum[q2, e_:4], m_]:>PropagatorDenominator[Momentum[q2, e]-Momentum[p, d],m],
		PropagatorDenominator[Momentum[q2, d_:4]-Momentum[q1, e_:4], m_]:>PropagatorDenominator[Momentum[q1, e]-Momentum[q2, d],m]
	})/;!FreeQ[expr,q1] && !FreeQ[expr,q2];

(* 	special iterative algorithm tailored for 2-loop self-energy integrals that determines the correct shifts
	to bring each integral to the canonical form *)
fds2LoopsSE2[expr : (_. FeynAmpDenominator[props__]), q1_, q2_, p_] :=
	Block[ {tmp, prs, needShift, newL, lcs, check, noShift, rep, P, checkShift, reps,  signFix},

		(* Checks if the given propagator is already in the canonical form *)
		check[x_] :=
			MatchQ[x, q1 | q2 | -q1 | -q2 | q1 - q2 | q2 - q1 | q1 - p | p - q1 |q2 - p | p - q2];

		(* Fixes the absolute sign in the propagators *)
		signFix[z_List] :=
			FixedPoint[Replace[#, {-q1 -> q1, -q2 -> q2, q2 - q1 -> q1 - q2, p - q1 -> q1 - p}, 1]&,z,5];

		(* Checks if the given shift increases the number of propagators in the canonical form *)
		checkShift[rrule_] :=
			(newL =Sort[signFix[Select[(prs /. rrule), check]]];
			lcs = LongestCommonSequence[signFix[noShift],newL];
			FCPrint[4,"FDS: fds2LoopsSE2: checkShift: Shifting rule ", rrule, "|",  FCDoControl->fdsVerbose];
			FCPrint[4,"FDS: fds2LoopsSE2: checkShift: Already canonical propagators ", signFix[noShift], "|",  FCDoControl->fdsVerbose];
			FCPrint[4,"FDS: fds2LoopsSE2: checkShift: Propagators before the shift ", prs, "|",  FCDoControl->fdsVerbose];
			FCPrint[4,"FDS: fds2LoopsSE2: checkShift: Canonical propagators after the shift ", newL, "|",  FCDoControl->fdsVerbose];
			((( Complement[signFix[noShift],lcs]==={} && Complement[newL,lcs]=!={}) ||
				Select[Sort[signFix[(prs /. rrule)]], (! check[#]) &]==={}) && Sort[signFix[Select[(prs /. rrule), check]]]=!={}));

		(* List of unique propagators in the integral *)
		prs = Union[{props} /. PropagatorDenominator[z_, _: 0] :> (z /. Momentum[a_, _ : 4] :> a)];
		prs = Expand/@prs;

		(* List of propagators where we need a shift *)
		needShift = Select[prs, (! check[#]) &];

		(* List of propagators that are already in the canonical form *)
		noShift = Select[prs, check];

		FCPrint[4,"FDS: fds2LoopsSE2: Entering with ", expr,  FCDoControl->fdsVerbose];

		(* If all the propagators are already in the canonical form, there is nothing to do	*)
		If[ needShift === {},
			Return[expr]
		];

		(* If the integral misses a q1^2-mi^2 propagator, do the appropriate shift  *)
		If[ !MemberQ[noShift, q1] && !MemberQ[noShift, -q1],
			FCPrint[4,"FDS: fds2LoopsSE2: Entering q1 ",  FCDoControl->fdsVerbose];
			(* Extract suitable propagators *)
			tmp = Select[needShift, !FreeQ[#, q1] &];
			reps = {};
			If[ tmp=!={},
				(* Generate possibly acceptable replacement rules *)
				reps = Join[reps, Map[(rep = (Solve[(# /. q1 -> P) == q1, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[reps, Map[(rep = (Solve[(# /. q1 -> P) == -q1, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				(* Apply the first suitable replacement rule *)
				If[ reps=!={},
					FCPrint[4,"FDS: fds2LoopsSE2: Shifting ", expr, " to ", reps[[1]],  FCDoControl->fdsVerbose];
					Return[MomentumExpand[expr /. reps[[1]]]]
				]
			]
		];


		(* If the integral misses a q2^2-mi^2 propagator, do the appropriate shift  *)
		If[ !MemberQ[noShift, q2] && !MemberQ[noShift, -q2],
			FCPrint[4,"FDS: fds2LoopsSE2: Entering q2 ",  FCDoControl->fdsVerbose];
			(* Extract suitable propagators *)
			tmp = Select[needShift, !FreeQ[#, q2] &];
			reps = {};
			If[ tmp=!={},
				(* Generate possibly acceptable replacement rules *)
				reps = Join[reps, Map[(rep = (Solve[(# /. q2 -> P) == q2, P] /. P -> q2);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[reps, Map[(rep = (Solve[(# /. q2 -> P) == -q2, P] /. P -> q2);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				(* Apply the first suitable replacement rule *)
				If[ reps=!={},
					FCPrint[4,"FDS: fds2LoopsSE2: Shifting ", expr, " to ", reps[[1]],  FCDoControl->fdsVerbose];
					Return[MomentumExpand[expr /. reps[[1]]]]
				]
			]
		];

		(* If the integral misses a (q1-p)^2-mi^2 propagator, do the appropriate shift  *)
		If[ !MemberQ[noShift, q1 - p] && !MemberQ[noShift, p - q1],
			FCPrint[4,"FDS: fds2LoopsSE2: Entering q1-p",  FCDoControl->fdsVerbose];
			(* Extract suitable propagators *)
			tmp = Select[needShift, !FreeQ[#, q1] &];
			reps = {};
			If[ tmp=!={},
				(* Generate possibly acceptable replacement rules *)
				reps = Join[reps, Map[(rep = (Solve[(# /. q1 -> P) == q1-p, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[reps, Map[(rep = (Solve[(# /. q1 -> P) == p-q1, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				(* Sometimes a simple sign change is more effective then a real shift, check if this is the case *)
				If[ checkShift[q1 -> -q1] &&
					(MemberQ[(needShift /. q1 -> -q1), q1 - p] || MemberQ[(needShift /. q1 -> -q1), p - q1]),
					PrependTo[reps, q1 -> -q1]
				];
				If[ checkShift[{q1 -> -q1, q2 -> -q2}] &&
					(MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), q1 - p] || MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), p - q1]),
					Flatten[PrependTo[reps, {q1 -> -q1, q2 -> -q2}]]
				];


				(* Apply the first suitable replacement rule *)
				If[ reps=!={},
					FCPrint[4,"FDS: fds2LoopsSE2: Shifting ", expr, " to ", reps[[1]],  FCDoControl->fdsVerbose];
					Return[MomentumExpand[expr /. reps[[1]]]]
				]
			]
		];

		(* If the integral misses a (q2-p)^2-mi^2 propagator, do the appropriate shift  *)
		If[ !MemberQ[noShift, q2 - p] && !MemberQ[noShift, p - q2],
			FCPrint[4,"FDS: fds2LoopsSE2: Entering q2-p",  FCDoControl->fdsVerbose];
			(* Extract suitable propagators *)
			tmp = Select[needShift, !FreeQ[#, q2] &];
			reps = {};
			If[ tmp=!={},
				(* Generate possibly acceptable replacement rules *)
				reps = Join[reps, Map[(rep = (Solve[(# /. q2 -> P) == q2-p, P] /. P -> q2);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[reps, Map[(rep = (Solve[(# /. q2 -> P) == p-q2, P] /. P -> q2);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				(* Sometimes a simple sign change is more effective then a real shift, check if this is the case *)
				If[ checkShift[q2 -> -q2] &&
					(MemberQ[(needShift /. q2 -> -q2), q2 - p] || MemberQ[(needShift /. q2 -> -q2), p - q2]),
					PrependTo[reps, q2 -> -q2]
				];
				If[ checkShift[{q1 -> -q1, q2 -> -q2}] &&
					(MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), q2 - p] || MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), p - q2]),
					Flatten[PrependTo[reps, {q1 -> -q1, q2 -> -q2}]]
				];

				(* Apply the first suitable replacement rule *)
				If[ reps=!={},
					FCPrint[4,"FDS: fds2LoopsSE2: Shifting ", expr, " to ", reps[[1]],  FCDoControl->fdsVerbose];
					Return[MomentumExpand[expr /. reps[[1]]]]
				]
			];
		];

		(* Finally, if the q1-q2 propagator is missing  *)
		If[ ! MemberQ[noShift, q2 - q1] && ! MemberQ[noShift, q1 - q2],
			FCPrint[4,"FDS: fds2LoopsSE2: Entering q1-q2",  FCDoControl->fdsVerbose];
			(* Extract suitable propagators *)
			tmp = Select[needShift, ! FreeQ[#, q1] &];
			reps = {};
			If[tmp=!={},
				(* Generate possibly acceptable replacement rules *)
				reps = Join[reps,Map[(rep = (Solve[(# /. q1 -> P) == q1-q2, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[Map[(rep = (Solve[(# /. q1 -> P) == q2-q1, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
			];

			tmp = Select[needShift, ! FreeQ[#, q2] &];
			If[tmp=!={},
				reps = Join[reps,
				Map[(rep = (Solve[(# /. q1 -> P) == q1-q2, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
				reps = Join[reps,
				Map[(rep = (Solve[(# /. q1 -> P) == q2-q1, P] /. P -> q1);
					If[Flatten[rep]=!={} && checkShift[rep[[1]]],
						rep[[1]],
						Unevaluated[Sequence[]]
					]) &, tmp]];
			];

			(* Sometimes a simple sign change is more effective then a real shift, check if this is the case *)
			If[ checkShift[q1 -> -q1] &&
				(MemberQ[(needShift /. q1 -> -q1), q1 - q2] ||	MemberQ[(needShift /. q1 -> -q1), q1 - q2]),
				PrependTo[reps, q1 -> -q1]
			];

			If[ checkShift[q2 -> -q2] &&
				(MemberQ[(needShift /. q2 -> -q2), q1 - q2] || MemberQ[(needShift /. q2 -> -q2), q1 - q2]),
				PrependTo[reps, q2 -> -q2]
			];
			If[ checkShift[{q1 -> -q1, q2 -> -q2}] &&
					(MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), q1 - q2] || MemberQ[(needShift /. {q1 -> -q1, q2 -> -q2}), q2 - q1]),
					Flatten[PrependTo[reps, {q1 -> -q1, q2 -> -q2}]]
			];

			(* Apply the first suitable replacement rule *)
			If[ reps=!={},
				FCPrint[4,"FDS: fds2LoopsSE2: Shifting ", expr, " to ", reps[[1]],  FCDoControl->fdsVerbose];
				Return[MomentumExpand[expr /. reps[[1]]]]
			];

		];

		FCPrint[4,"FDS: fds2LoopsSE2:  No shifts possible, leaving with ", expr,  FCDoControl->fdsVerbose];
		expr
	];




feyncan[a__] :=
	Apply[FeynAmpDenominator, Sort[Sort[{a}], lenso]];

checkfd[FeynAmpDenominator[b__PD]] :=
	If[ FreeQ[{b}, PD[(z_Plus) /; Length[z]>2,_]],
		If[ FreeQ[{b}, PD[a_ + (f_/;(f^2 > 1)) c_Momentum,_]],
			True,
			False
		],
		False
	];

extractm[a_, ___] :=
	a;

mfd[xxx__] :=
	MomentumExpand[FeynAmpDenominator[xxx]];

prmomex[xx_] :=
	MemSet[prmomex[xx], xx /. FeynAmpDenominator -> mfd];

tran[a_, x_, y_, z__] :=
	tran[a /. (Rule @@ ( {x, y} /. Momentum -> extractm)), z];

tran[a_, {x_, y_, w_, z_}] :=
	MemSet[tran[a,{x,y,w,z}],
		Block[ {tem, re},
			If[ (Head[a] =!= Times) && (Head[a] =!= FeynAmpDenominator),
				re = a,
				tem = prmomex[SelectNotFree[a, FeynAmpDenominator] /.
				{RuleDelayed @@ ( {x, y} /. Momentum -> extractm),
					RuleDelayed @@ ( {w, z} /. Momentum -> extractm)}];
				If[ checkfd[tem] === False,
					re = a,
					re  = ExpandScalarProduct[prmomex[a /.
					{RuleDelayed @@ ( {x, y} /. Momentum -> extractm),
					RuleDelayed @@ ( {w, z} /. Momentum -> extractm)}],
					FCI -> True];
				];
			];
			re
		]
	];

tran[a_, x_, y_] :=
	MemSet[tran[a,x,y],
		Block[ {tem, re},
			If[ (Head[a] =!= Times) && (Head[a] =!= FeynAmpDenominator),
				re = a,
				(* do only translations if no three terms appear in PropagatorDe..*)
				tem = prmomex[SelectNotFree[a, FeynAmpDenominator] /.
				(Rule @@ ( {x, y} /. Momentum -> extractm))];
				If[ checkfd[tem] === False,
					re = a,
					re  = ExpandScalarProduct[prmomex[a /. (Rule @@ ( {x, y} /. Momentum -> extractm ))],
					FCI -> True]
				];
			];
			re
		]
	];



qtr[xx_Plus,q1_] :=
	Map[qtr[#,q1]&, xx];





qtr[a_ OPESum[xx_,yy_], q1_] :=
	(a OPESum[qtr[xx, q1],yy]) /; FreeQ[a, q1];

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power))[(
			Pair[Momentum[OPEDelta, di___], Momentum[pi_, di___]] -
			Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] +
			Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]) , w_], q1_] :=
	Block[ {tt},
		tt = ExpandScalarProduct[(fa powe[(
		Pair[Momentum[OPEDelta, di], Momentum[pi, di]] -
		Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
		Pair[Momentum[OPEDelta, di], Momentum[q1, di]]), w]
		) /. q1->(-q1+pe-pi),FCI -> True];
		tt = PowerSimplify[tt];
		If[ FreeQ[tt, (pow_ /; (pow === Power2 || pow ===
			Power))[(a_Plus),v_ /; Head[v] =!= Integer]],
			If[ !FreeQ[tt,Eps],
				EpsEvaluate[tt],
				tt
			],
			fa powe[(Pair[Momentum[OPEDelta, di], Momentum[pi, di]] -
			Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
			Pair[Momentum[OPEDelta, di], Momentum[q1, di]]), w]
		];
		tt
	];

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power))[(
			-Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] +
			Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]) , w_], q1_] :=
	Block[ {tt},
		tt = ExpandScalarProduct[(fa powe[(
		-Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
		Pair[Momentum[OPEDelta, di], Momentum[q1, di]]), w]
		) /. q1->(-q1+pe),FCI -> True];
		If[ FreeQ[tt, (pow_ /; (pow === Power2 ||
		pow ===    Power))[(a_Plus),v_] ],
			If[ !FreeQ[tt,Eps],
				EpsEvaluate[tt],
				tt
			],
			fa powe[(-Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
			Pair[Momentum[OPEDelta, di], Momentum[q1, di]]),w]
		];
		tt
	];

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power))[(
			Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] -
			Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]) , w_], q1_] :=
	Block[ {tt},
		tt = ExpandScalarProduct[(fa powe[(
			Pair[Momentum[OPEDelta, di], Momentum[pe, di]] -
			Pair[Momentum[OPEDelta, di], Momentum[q1, di]]), w]
			) /. q1->(-q1+pe),FCI -> True];
		If[ FreeQ[tt, (pow_ /; (pow === Power2 ||
		pow === Power))[(a_Plus),v_] ],
			If[ !FreeQ[tt,Eps],
				EpsEvaluate[tt],
				tt
			],
			fa powe[( Pair[Momentum[OPEDelta, di], Momentum[pe, di]] -
			Pair[Momentum[OPEDelta, di], Momentum[q1, di]]),w]
		];
		tt
	];

(* check if via simple translations a tadpole appears *)
pro1[x_, 0] :=
	x;
(* check for A_mu *)



nopcheck[q1_, q2_][pr__PD] :=
	If[ !FreeQ[{pr}, PD[_, ma_ /; ma =!= 0]],
		FeynAmpDenominator[pr],
		Block[ {prp, vv, class, p, lev},
			lev[PD[a_, 0], PD[b_, 0]] :=
				If[ Length[Variables[a]] < Length[Variables[b]],
					True,
					False
				];
			prp = {pr} /. PD -> pro1;
			(* check  for reducible tadpoles *)
			If[ (Length[Union[SelectFree[prp, q1]]] === 1 && (* only one prop.*)
				FreeQ[SelectNotFree[prp, q1], q2]) ||  (* no overlap  *)
				(Length[Union[SelectFree[prp, q2]]] === 1 && (* only one prop.*)
				FreeQ[SelectNotFree[prp, q2], q1]), (* no overlap  *)
				0,
				vv = Variables[prp];
				If[ Length[vv] < 3,
					0,
					If[ SelectFree[vv, {q1,q2}] =!= {},
						p = SelectFree[vv, {q1, q2}][[1, 1]]
					];
					class = Sort[Union[Map[Variables, MomentumExpand[
							{ prp /. {q1 :> q2, q2 :> q1},
								prp /. {q1 :>  (q1 + p) , q2 :>  (q2 +  p)},
								prp /. {q1 :>  (q1 + p) , q2 :>  (q2 -  p)},
								prp /. {q1 :>  (q1 - p) , q2 :>  (q2 +  p)},
								prp /. {q1 :>  (q1 - p) , q2 :>  (q2 -  p)},
								prp /. {q1 :> (-q1 + p) , q2 :> (-q2 + p)},
								prp /. {q1 :> (-q1 + p) , q2 :> (-q2 - p)},
								prp /. {q1 :> (-q1 - p) , q2 :> (-q2 + p)},
								prp /. {q1 :> (-q1 - p) , q2 :> (-q2 - p)}
							}]]], lev];
					If[ Length[class[[1]]] < 3,
						0,
						FeynAmpDenominator[pr]
					]
				]
			]
		]
	];



trach[x_Symbol,__] :=
	x;
trach[a_,b_,c_] :=
	FixedPoint[trachit[#, b,c]&,a , 8];

trachit[x_, q1_, q2_] :=
	MemSet[trachit[x, q1, q2],
		Block[ {nx, dufa, qqq, q1pw, q2pw},
			FCPrint[3,"FDS: trachit: Entering with ", x];
			FCPrint[3,"FDS: trachit: Loop momenta are ", q1," " ,q2];
			nx = x /.( (n_. Pair[Momentum[q1, dim___], any_
								] + more_. )^(w_ /; Head[w]=!=Integer)
						) :> (q1pw[n Pair[Momentum[q1, dim], any] + more,w]);
			nx = nx /.( (n_. Pair[Momentum[q2,dim___], any_
									]+ more_. )^(w_ /; Head[w]=!=Integer)
						) :> (q2pw[n Pair[Momentum[q2, dim], any] + more,w]);
			nx = nx dufa;
			If[ Head[nx] =!= Times,
				nx = nx /. dufa -> 1,
				nx = (SelectFree[nx, {q1, q2}]/.dufa -> 1) *
						( qqq[SelectNotFree[nx, {q1, q2}]] /.
							{
							(* special stuff *)
							(* f42 *)
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q1, dim___] + Momentum[pe_ /; pe =!= q2, dim___], m_], bb___PD]] :>
								tran[fa FeynAmpDenominator[aa, PD[-Momentum[q1, dim] - Momentum[pe, dim], m], bb],	q1, -q1, q2, q2],

							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] + Momentum[pe_ /; pe =!= q1, dim___], m_], bb___PD]] :>
								tran[fa FeynAmpDenominator[aa, PD[-Momentum[q2, dim] - Momentum[pe, dim], m], bb], q1, q1, q2, -q2]
							}/.
							{
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q1, dim___] - Momentum[pe_ /; pe =!= q2, dim___], m_], bb___PD]]:>
								((tran[fa FeynAmpDenominator[aa, PD[Momentum[q1, dim] - Momentum[pe, dim], m], bb], q1, -q1 + pe, q2, -q2 + pe])/;(
								MatchQ[(Times@@Union[{aa, PD[Momentum[q1, dim] - Momentum[pe, dim],m],	bb}]),
								PD[Momentum[q1, dim], _] PD[Momentum[q1, dim] - Momentum[pe, dim], m] PD[Momentum[q1, dim] -
								Momentum[q2, dim], _] PD[Momentum[q2, dim] - Momentum[pe, dim], _]] ||
								MatchQ[(Times@@Union[{aa, PD[Momentum[q1, dim] - Momentum[pe, dim],m], bb}]),
								PD[Momentum[q1, dim], _] PD[Momentum[q1, dim] -	Momentum[pe, dim], m] PD[Momentum[q2, dim] -
								Momentum[q1, dim], _] PD[Momentum[q2, dim] - Momentum[pe, dim], _]])),
							(* f41 *)
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] - Momentum[pe_ /; pe =!= q1, dim___], m_], bb___PD]]:>
								((tran[fa FeynAmpDenominator[aa, PD[Momentum[q2, dim] - Momentum[pe, dim],m], bb], q2, -q2 + pe, q1, -q1 + pe])/;(
								MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] - Momentum[pe, dim],m], bb}]),
								PD[Momentum[q2, dim], _] PD[Momentum[q2, dim] -
								Momentum[pe, dim], m] PD[Momentum[q2, dim] -
								Momentum[q1, dim], _] PD[Momentum[q1, dim] -
								Momentum[pe, dim], _]] ||
								MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] - Momentum[pe, dim],m], bb}]),
								PD[Momentum[q2, dim], _] PD[Momentum[q2, dim] -
								Momentum[pe, dim], m] PD[Momentum[q1, dim] -
								Momentum[q2, dim], _] PD[Momentum[q1, dim] -
								Momentum[pe, dim], _]])),
							(* f43 *)
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] -
							Momentum[pe_ /; pe =!= q1, dim___], m_], bb___PD]]:>
								((tran[fa FeynAmpDenominator[aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],m], bb], {q2, q1, q1, q2}])/;(MatchQ[(Times@@Union[{aa,
								PD[Momentum[q2, dim] - Momentum[pe, dim],m],bb}]),
								PD[Momentum[q2, dim], _] PD[Momentum[q2, dim] -	Momentum[pe, dim], m] *
								PD[Momentum[q2, dim] - Momentum[q1, dim], _] PD[Momentum[q1, dim], _]] ||
								MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] - Momentum[pe, dim],m],bb}]),
								PD[Momentum[q2, dim], _] PD[Momentum[q2, dim] - Momentum[pe, dim], m] *
								PD[Momentum[q1, dim] - Momentum[q2, dim], 0] PD[Momentum[q1, dim], _]])),
							(* f4331 *)
							qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
							FeynAmpDenominator[aa___PD,	PD[Momentum[q2, dim___] -
							Momentum[pe_ /; pe =!= q1, dim___], 0],	bb___PD]]:>
								((tran[fa as^w FeynAmpDenominator[aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],0], bb], q1, -q1 + q2 ])/;(
								MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] - Momentum[pe, dim],0], bb}]),
								PD[Momentum[q1, dim], 0] PD[Momentum[q2, dim], 0]*
								PD[Momentum[q2, dim] - Momentum[pe, dim], 0] *
								PD[Momentum[q2, dim] - Momentum[q1, dim], 0]])),
							(* f4332 *)
							qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
							FeynAmpDenominator[aa___PD,	PD[Momentum[q1, dim___] -
							Momentum[pe_ /; pe =!= q2, dim___], 0],	bb___PD]]:>
								((tran[fa as^w FeynAmpDenominator[aa, PD[Momentum[q1, dim] -
								Momentum[pe, dim],0], bb], q2, -q2 + q1 ])/;(
								MatchQ[(Times@@Union[{aa, PD[Momentum[q1, dim] - Momentum[pe, dim],0],bb}]),
								PD[Momentum[q2, dim], 0] PD[Momentum[q1, dim], 0]*
								PD[Momentum[q1, dim] - Momentum[pe, dim], 0] *
								PD[Momentum[q1, dim] - Momentum[q2, dim], 0]])),
							(* f4333 *)
							qqq[fa_. q1pww_[(as_ /; (Length[as] === 3 && Head[as]===Plus)), w_]*
							FeynAmpDenominator[aa___PD, PD[Momentum[q1, dim___] -
							Momentum[pe_ /; pe =!= q2, dim___], 0], bb___PD]]:>
								((tran[fa q1pww[as,w] FeynAmpDenominator[aa, PD[Momentum[q1, dim] -
								Momentum[pe, dim],0],bb], q1, -q1 + pe ])/;(MatchQ[(Times@@Union[{aa,
								PD[Momentum[q1, dim] - Momentum[pe, dim],0],bb}]),
								PD[Momentum[q2, dim], 0] PD[Momentum[q1, dim], 0]*
								PD[Momentum[q1, dim] - Momentum[pe, dim], 0] *
								PD[Momentum[q2, dim] - Momentum[pe, dim], 0]])),
							(* f30 *)
							qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
							FeynAmpDenominator[aa___PD,	PD[Momentum[q2, dim___] -
							Momentum[pe_ /; pe =!= q1, dim___], 0], bb___PD]]:>
								((tran[fa as^w FeynAmpDenominator[aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],0], bb], q2, -q2 + q1 + pe])/;
								(MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],0], bb}]), PD[Momentum[q1, dim], 0]*
								PD[Momentum[q2, dim] - Momentum[pe, dim], 0] *
								PD[Momentum[q2, dim] - Momentum[q1, dim], 0]])),
							(* f31 *)
							qqq[fa_. q1pw[as_ /;Length[as] === 3,w_]*
							FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] -
							Momentum[pe_ /; pe =!= q1, dim___], 0], bb___PD]]:>
								((tran[fa q1pw[as,w] FeynAmpDenominator[aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],0], bb], q2, -q2 + q1 + pe])/;
								(MatchQ[(Times@@Union[{aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],0], bb}]),
								PD[Momentum[q1, dim], 0] PD[Momentum[q2, dim] -
								Momentum[pe, dim], 0] PD[Momentum[q2, dim] -
								Momentum[q1, dim], 0]]))
							}/.
							{
							qqq[fa_. Pair[Momentum[q1, dim___], Momentum[q1,dim___]]*
							FeynAmpDenominator[a___, PD[np_. Momentum[pe_, dimp___] +
							nq1_. Momentum[q1,   dim___], 0], b___]] :>
								((tran[fa Pair[Momentum[q1, dim], Momentum[q1, dim]]*
								FeynAmpDenominator[a, PD[np Momentum[pe,dimp] + nq1 Momentum[q1,dim],0],b],
								q1, (-q1/nq1 - np/nq1 pe)]) /; FreeQ[{a,b}, PD[_. Momentum[q1,dim],0]]),

							qqq[fa_. Pair[Momentum[q2, di___], Momentum[q2,dim___]]*
							FeynAmpDenominator[a___, PD[np_. Momentum[pe_,dimp___] +
							nq2_. Momentum[q2, dim___],0], b___]] :>
								((tran[fa Pair[Momentum[q2, di], Momentum[q2,dim]]*
								FeynAmpDenominator[a, PD[np Momentum[pe,dimp] + nq2 Momentum[q2,dim],0],b],
								q2, (-q2/nq2 - np/nq2 pe)])/;
								FreeQ[{a, b}, PD[_. Momentum[q2,dim],0]])
							}/.
							{
							qqq[(fa_.) *
								FeynAmpDenominator[a___, PD[Momentum[q1, dim___] +
								Momentum[q2, dim___] , m_], b___]] :>
								(tran[fa FeynAmpDenominator[a, PD[Momentum[q1, dim] +
								Momentum[q2, dim], m], b],q2, -q2])
							} //.
							{
							qqq[(fa_.) FeynAmpDenominator[a___,	PD[np_. Momentum[pe_,dimp___] +
							nq2_. Momentum[q2, dim___] + nq1_. Momentum[q1, dim___], m_], b___]] :>
								(tran[fa FeynAmpDenominator[a, PD[np Momentum[pe, dimp] +
								nq2 Momentum[q2, dim] + nq1 Momentum[q1, dim], m], b], q1 ,
								(q1/nq1 -np pe/nq1)]) /;FreeQ[fa, q1pw],

							qqq[(fa_.) FeynAmpDenominator[a___, PD[np_. Momentum[pe_,dimp___] +
							nq2_. Momentum[q2, dim___] + nq1_. Momentum[q1, dim___], m_], b___]] :>
								(tran[fa FeynAmpDenominator[a, PD[np Momentum[pe, dimp] + nq2 Momentum[q2, dim] +
								nq1 Momentum[q1, dim], m], b], q2, (q2/nq2 -np pe/nq2)]) /;
								FreeQ[fa, q2pw],

							qqq[(fa_.) q1pw[Pair[Momentum[OPEDelta,di___], Momentum[q1, dim___]] -
							Pair[Momentum[OPEDelta,di___], Momentum[q2, dim___]], pow_] FeynAmpDenominator[a___]] :>
								tran[fa FeynAmpDenominator[a] q1pw[Pair[Momentum[OPEDelta,di], Momentum[q1,dim]] -
								Pair[Momentum[OPEDelta,di],	Momentum[q2, dim]], pow], q1, -q1+q2] /;
								FreeQ[{a}, PD[_. Momentum[q1,___] +	_. Momentum[pe_ /; FreeQ[pe,q2],___],_]],

							qqq[(fa_.) q1pw[Pair[Momentum[OPEDelta,di___], Momentum[q1, dim___]] +
							Pair[Momentum[OPEDelta,di___], Momentum[pe_,dimp___]], pow_] FeynAmpDenominator[a___]] :>
								tran[fa FeynAmpDenominator[a] q1pw[Pair[Momentum[OPEDelta,di], Momentum[q1,dim]] +
								Pair[Momentum[OPEDelta,di], Momentum[pe, dimp]], pow], q1, -q1],

							qqq[(fa_.) q2pw[Pair[Momentum[OPEDelta,di___], Momentum[q2, dim___]] +
							Pair[Momentum[OPEDelta,di___], Momentum[pe_,dimp___]], pow_] *
							FeynAmpDenominator[a___]] :>
							tran[fa FeynAmpDenominator[a] q2pw[Pair[Momentum[OPEDelta,di],
							Momentum[q2,dim]] + Pair[Momentum[OPEDelta,di], Momentum[pe, dimp]], pow],
							q2, -q2]
						} /.
						{
							q1pw :> Power, q2pw :> Power, qqq :> Identity
						}
					);
			];
			(* q1 <--> q2 *)
			If[ !FreeQ[nx, Eps],
				nx = EpsEvaluate[nx]
			];
			If[ FreeQ[nx,(_. + _. Pair[Momentum[OPEDelta,___],
				Momentum[q1,___]]^(hhh_/;Head[hhh] =!= Integer))] && nx =!= {},
				nx = Sort[FDS[{nx, nx /. {q1 :> q2, q2 :> q1}}]][[1]]
			];
			nx
		]
	];

translat[x_, q1_, q2_] :=
	MemSet[translat[x,q1,q2],
	Block[ {nuLlL1, nuLlL2},
		Map[ trach[#, q1, q2]&,
		(FCPrint[1,"in translat" , x, "|", q1, "|" ,q2]; Expand2[x, FeynAmpDenominator] + nuLlL1 + nuLlL2)] /. {nuLlL1:>0, nuLlL2:>0}
	]];

integralClassify[loopHead[x_,y_]]:=
	MemSet[integralClassify[loopHead[x,y]],
	Which[
		FreeQ2[x,{StandardPropagatorDenominator,CartesianPropagatorDenominator,GenericPropagatorDenominator}] &&
			!FreeQ2[x,{PropagatorDenominator}],
		(* FAD *)
		loopHead[x,y],
		FreeQ2[x,{PropagatorDenominator,CartesianPropagatorDenominator,GenericPropagatorDenominator}] &&
			!FreeQ2[x,{StandardPropagatorDenominator}],
		(* SFAD *)
		sLoopHead[x,y],
		FreeQ2[x,{PropagatorDenominator,StandardPropagatorDenominator,GenericPropagatorDenominator}] &&
			!FreeQ2[x,{CartesianPropagatorDenominator}],
		(* CFAD *)
		cLoopHead[x,y],
		True,
		(* mixture of different propagators *)
		mLoopHead[x,y]
	]];


FCPrint[1,"FeynAmpDenominatorSimplify.m loaded."];
End[]
