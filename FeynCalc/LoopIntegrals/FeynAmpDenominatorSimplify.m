(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 February '99 at 18:32 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: simplification *)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSimplify::usage =
"FeynAmpDenominatorSimplify[exp] simplifies each
PropagatorDenominator in a canonical way. \n
FeynAmpDenominatorSimplify[exp, q1] simplifies
all FeynAmpDenominator's in exp in a canonical way,
including some translation of momenta.
FeynAmpDenominatorSimplify[exp, q1, q2] additionally
removes 2-loop integrals with no mass scale.";

FDS::usage =
"FDS is shorthand for FeynAmpDenominatorSimplify.";

$NoShifts::usage =
"$NoShifts is an option of FeynAmpDenominatorSimplify.";

FDS::twoloopsefail =
"FDS detected a fatal error while converting the 2-loop self energy integral `1`
into canonicla form. Evaluation aborted!"

FDS::failmsg = "Error! FDS has encountered a fatal problem and must abort the computation. The problem
reads: `1`";

DetectLoopTopologies::usage=
"DetectLoopTopologies is an option for FDS. If set to True, FDS will try to recognize some
special multiloop topologies and apply appropriate simplifications";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorSimplify`Private`"]

fdsVerbose::usage="";

$NoShifts=False;
FDS = FeynAmpDenominatorSimplify;

(* new 10/95 *)
$Power2 = True;

Options[FeynAmpDenominatorSimplify] = {
	Apart2 -> True,
	FCI -> False,
	FeynAmpDenominatorCombine -> True,
	FCVerbose -> False,
	FC2RHI -> False,
	IntegralTable -> {},
	IncludePair -> False,
	DetectLoopTopologies -> True
};

SetAttributes[FeynAmpDenominatorSimplify, Listable];

(* For the case that there is nothing to do *)
FeynAmpDenominatorSimplify[a_ /; FreeQ[a, PD], ___,OptionsPattern[]] :=
	a /; FreeQ[a, FAD];

(*	Simple wrappers	*)
FeynAmpDenominatorSimplify[exp_,Momentum[q1_, _:4],opts:OptionsPattern[]] :=
	FeynAmpDenominatorSimplify[exp, q1,opts];

FeynAmpDenominatorSimplify[exp_,Momentum[q1_, _:4], Momentum[q2_, _:4],opts:OptionsPattern[]] :=
	FeynAmpDenominatorSimplify[exp, q1, q2,opts];

(*	Without knowing what the loop momenta are, FDS can just try to sort propagators
	and pull out the overall minus signs,	but nothing more	*)
FeynAmpDenominatorSimplify[exp_, OptionsPattern[]] :=
	exp /. PD -> procan /. procan ->
	PD /. FeynAmpDenominator -> feyncan;

(*	Handling general multiloop integrals with more than 2 loop momenta	*)
FeynAmpDenominatorSimplify[exp_, qmore__,
	q1_/;Head[q1] =!= Rule, q2_ /;Head[q2] =!= Rule,opts:OptionsPattern[]] :=
	FeynAmpDenominatorSimplify[exp /. FeynAmpDenominator -> feynsimp[q2], qmore, q1,opts];

(*	FDS for 1-loop integrals	*)
FeynAmpDenominatorSimplify[exp_, q1_/;Head[q1]=!=Momentum, OptionsPattern[]] :=
	Block[ {efdes,aMu,aMu2,aMu3},
		aMu[zq_, zexp_] :=
			If[ Head[zexp] === Plus,
				zexp,
				SelectFree[zexp, zq] aMu2[zq, SelectNotFree[zexp,zq]]
			] /. aMu2 -> aMu3;

		aMu2[zq_, anyf_[a___,Momentum[zq_,___], b___]*
			FeynAmpDenominator[ PD[Momentum[zq_,___],_]..]] :=
			0 /; FreeQ[{a,anyf,b},zq];

		aMu3[_,xx_] :=
			xx;

		efdes[zexp_, zq1_] :=
			If[ Head[zexp] === Plus,
				Map[efdes[#, zq1]&, zexp],
				aMu[zq1,((sumtra[FeynAmpDenominatorCombine[ zexp ] /.{
				FeynAmpDenominator[PD[Momentum[zq1,___]+pe_. ,0]..]:>0/;
				FreeQ[pe,zq1]
											},
				zq1 ]) /. FeynAmpDenominator -> feynsimp[zq1] /.
					FeynAmpDenominator -> feynord[zq1]
				)
				]
			];
		FCPrint[1, "FeynAmpDenominatorSimplify: Entering with ", exp];
		FixedPoint[efdes[#, q1]&, exp, 6]
	];

(*	FDS for 2-loop integrals	*)
FeynAmpDenominatorSimplify[ex_, q1_, q2_/;Head[q2]=!=Rule, opt:OptionsPattern[]] :=
	Block[ {exp, res, null1, null2, fclsOutput, intsRest, intsFDS, intsFDS2, intsFDS3, intsFDS4,
			fds2Loops,fds1LoopQ1,fds1LoopQ2, intsFDSUnique, intsTops, intsTops2,
			intsFDSUniqueFromSols, intsFDSUniqueFromSols2, intsFDSUniqueNested, solsListNested,
			repRuleNested, fds2LoopsNested,
			solsList, repRule
			},

		If [OptionValue[FCVerbose]===False,
			fdsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fdsVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"FDS: Entering 2-loop FDS with", ex, FCDoControl->fdsVerbose];

		If[ !OptionValue[FCI],
			exp = FeynCalcInternal[ex],
			exp = ex
		];

		If [ OptionValue[FeynAmpDenominatorCombine],
			exp = FeynAmpDenominatorCombine[exp]
		];


		(*	Let us first extract all the scalar loop integrals	*)
		fclsOutput = FCLoopSplit[ex,{q1,q2},FCI->False];
		intsRest = fclsOutput[[1]];
		intsFDS = fclsOutput[[2]]+fclsOutput[[3]]+fclsOutput[[4]];

		FCPrint[3, "FDS: Terms to be ignored ", intsRest, FCDoControl->fdsVerbose];
		FCPrint[3, "FDS: Possibly relevant terms ", intsFDS, FCDoControl->fdsVerbose];

		(*	Nothing to do	*)
		If[ intsFDS === 0,
			Return[ex]
		];

		(*	Split FADs to extract only loop momenta dependent pieces *)
		intsFDS = FeynAmpDenominatorSplit[intsFDS];

		(* Let us now isolate all the 2-loop and 1-loop integrals that depend on q1,q2 *)
		intsFDS2 = FCLoopIsolate[intsFDS, {q1,q2}, FCI->True, MultiLoop->True, Head->fds2Loops];
		intsFDS3 = FCLoopIsolate[intsFDS2, {q1}, FCI->True, MultiLoop->True, Head->fds1LoopQ1, ExceptHeads->{fds2Loops}];
		intsFDS4 = FCLoopIsolate[intsFDS3, {q2}, FCI->True, MultiLoop->True, Head->fds1LoopQ2, ExceptHeads->{fds2Loops}];
		(*	Put the FADs back together *)
		intsFDS4 = intsFDS4/. (h:fds2Loops|fds1LoopQ1|fds1LoopQ2)[x__]:> h[FeynAmpDenominatorCombine[x]];

		FCPrint[3, "FDS: Isolated terms ", intsFDS4, FCDoControl->fdsVerbose];

		(* Extract all the unique integrals *)
		intsFDSUnique = Cases[intsFDS4+null1+null2,(fds2Loops|fds1LoopQ1|fds1LoopQ2)[x__],Infinity]//Union;
		FCPrint[3, "FDS: Unique terms ", intsFDSUnique, FCDoControl->fdsVerbose];


		(* Apply standard simplifications *)
		solsList = MapIndexed[
								Which [	Head[#]===fds2Loops,
										fds2Loops[oldFeynAmpDenominatorSimplify[(#/. fds2Loops->Identity),q1,q2,opt]],
										Head[#]===fds1LoopQ1,
										FeynAmpDenominatorSimplify[(#/. fds1LoopQ1->Identity),q1,opt],
										Head[#]===fds1LoopQ2,
										FeynAmpDenominatorSimplify[(#/. fds1LoopQ2->Identity),q2,opt],
										True,
										Message[FDS::failmsg,"Unknown integral type!"];
								]&,intsFDSUnique];



		(*	Tailored simplifications for speical 2-loop topologies	*)
		If[ OptionValue[DetectLoopTopologies],
			FCPrint[3, "FDS: Simplifying special topologies.", FCDoControl->fdsVerbose];


			(* Isolate single loop integrals in every solution *)
			intsFDSUniqueFromSols2 = solsList/.
										{fds2Loops[x__]:> Map[fds2LoopsNested,Expand2[x,FeynAmpDenominator]+null1+null2]}/.
										fds2LoopsNested[null1|null2] -> 0;


			(* Extract unique nested integrals *)
			intsFDSUniqueNested = Cases[intsFDSUniqueFromSols2,fds2LoopsNested[x__],Infinity]//Union;
			FCPrint[3, "FDS: Unique terms ", intsFDSUniqueNested, FCDoControl->fdsVerbose];


			intsTops=getTopology[#,{q1,q2}]&/@(intsFDSUniqueNested/.fds2LoopsNested->Identity);
			FCPrint[3, "FDS: Topologies ", intsTops, FCDoControl->fdsVerbose];

			intsTops2 = checkTopology/@intsTops;
			FCPrint[3, "FDS: Checked topologies ", intsTops2, FCDoControl->fdsVerbose];


			solsListNested = MapIndexed[
					(* Here one could add more topologies if needed *)
					Which[	#1==="2-loop-self-energy",
								(fds2LoopsSE[(First[intsFDSUniqueNested[[#2]]]/.fds2LoopsNested->Identity),q1,q2,(Total[intsTops[[#2]]])[[3]][[1]]]/. FeynAmpDenominator -> feyncan),
							#1==="generic",
								(First[intsFDSUniqueNested[[#2]]]/. fds2LoopsNested->Identity),
							True,
								Message[FDS::failmsg,"Unknown 2-loop topology!"];
								Abort[]

					]&,intsTops2];

			If[Length[intsFDSUniqueNested]=!=Length[solsListNested] || !FreeQ2[solsListNested,{fds2Loops,fds1LoopQ1,fds1LoopQ2,fds2LoopsNested}],
				Message[FDS::failmsg,"FDS can't create the solution list."];
				Abort[]
			];
			repRuleNested = MapIndexed[(Rule[#1, First[solsListNested[[#2]]]]) &, intsFDSUniqueNested];

			solsList = intsFDSUniqueFromSols2/.repRuleNested,
			(* Otherwise just remove the fds2Loops heads *)
			solsList = solsList/.fds2Loops->Identity
		];


		If[Length[solsList]=!=Length[intsFDSUnique] || !FreeQ2[solsList,{fds2Loops,fds1LoopQ1,fds1LoopQ2,fds2LoopsNested}],
			Message[FDS::failmsg,"FDS can't create the solution list."];
			Abort[]
		];
		(* Final replacement rule *)
		repRule = MapIndexed[(Rule[#1, First[solsList[[#2]]]]) &, intsFDSUnique];

		res = intsRest + (intsFDS4/.repRule);

		If[ !FreeQ2[res,{fds2Loops,fds1LoopQ1,fds1LoopQ2}],
				Message[FDS::failmsg,"Some subroutines in FDS didn't work properly."];
				Abort[]
		];

		FCPrint[1,"FDS: Leaving 2-loop FDS with", res, FCDoControl->fdsVerbose];
		res
	];


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
				translat[ zexp /. FeynAmpDenominator -> amucheck[zq1,zq2] /.
									amucheck ->  nopcheck,  zq1, zq2] /.
				FeynAmpDenominator -> feynsimp[zq1] /. FeynAmpDenominator -> feynsimp[zq2] /.
				FeynAmpDenominator -> feynord
			]  /. {
				((_. + _. Pair[Momentum[zq1,___],Momentum[OPEDelta,___]])^(vv_/; Head[vv] =!= Integer)) *
				FeynAmpDenominator[pr1___, PD[_. + Momentum[zq1,_:4], _], pr2___	] :> 0 /; FreeQ[{pr1, pr2}, zq1]
			};

		amucheck[k1_, k2_][PD[aa_. Momentum[k1_,dii___] + bb_. ,0].., b___] :=
			0 /; FreeQ[{b}, k1];

		amucheck[k1_, k2_][b___,PD[aa_. Momentum[k1_,dii___] + bb_. ,0]..] :=
			0 /; FreeQ[{b}, k1];

		amucheck[k1_, k2_][b___,PD[aa_. Momentum[k2_,dii___] + bb_. ,0]..] :=
			0 /; FreeQ[{b}, k2];

		amucheck[k1_, k2_][PD[aa_. Momentum[k2_,dii___] + bb_. ,0].., b___] :=
			0 /; FreeQ[{b}, k2];

		pe[qu1_,qu2_, prop_] :=
			Block[ {pet = SelectFree[Cases2[prop//MomentumExpand,Momentum]/.
			Momentum[a_,___]:>a, {qu1,qu2} ]},
				If[ Length[pet]>0,
					First[pet],
					pet
				]
			];

		basic = {
			FCIntegral[anyf_[a___, Momentum[q1,di_], b___]*
			FeynAmpDenominator[c___,pro:PD[Momentum[q2,di_]-Momentum[q1,di_],_].., d___]] :>
				Calc[(anyf[a,Momentum[q1,di], b] FeynAmpDenominator[c,pro,d]) /.q1-> -q1+q2] /;
				FreeQ[{a,anyf,b,c,d}, q1],

			FCIntegral[anyf_[a___, Momentum[q2,di_], b___]*
			FeynAmpDenominator[c___,pro:PD[Momentum[q2,di_]-Momentum[q1,di_],_].., d___]] :>
				Calc[(anyf[a,Momentum[q2,di], b] FeynAmpDenominator[c,pro,d]) /. q2-> -q2+q1] /;
				FreeQ[{a,anyf,b,c,d}, q2],
			(*A_mu NNEWWW CORRECTED*)
			FCIntegral[anyf_[a___,Momentum[q1,___], b___]*
			FeynAmpDenominator[ c___, PD[Momentum[q1,___], _]..,d___]] :>
			(FCPrint[3,"Amu 1"]; 0) /; FreeQ[{a,anyf,b,c,d},q1],

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

			exp = topi[exp] /. topi -> topi2 /. topi2[a_] :> FCIntegral[a];
			exp = exp /. basic /. FCIntegral -> Identity;
			exp = topi[exp] /. topi -> topi2 /. topi2[a_] :>
					FCIntegral[ChangeDimension[a,4]//FeynCalcExternal];
			exp = exp /. ot /. ot /. pot /. pot /. FCIntegral[b_] :>
					FeynCalcInternal[ChangeDimension[b, D]];
		];

		If[ Head[exp] =!= Plus,
			If[	OptionValue[Options[FDS],{opt},FC2RHI],
				(* This is OPE related stuff with FC2RHI *)
				res = FC2RHI[FixedPoint[fadalll[#, q1, q2]&, Expand2[exp, q1], 7] /. pru, q1, q2,
				IncludePair -> (IncludePair /. {opt} /.	Options[FDS])],
				(* This is the usual routine *)
				res = FixedPoint[fadalll[#, q1, q2]&, Expand2[exp, q1], 7] /. pru
			],
			res = SelectFree[exp, {q1,q2}] +
			If[ (FC2RHI /. {opt} /. Options[FDS]),
				(* This is OPE related stuff with FC2RHI *)
				FC2RHI[FixedPoint[fadalll[#, q1, q2]&, exp-SelectFree[exp,{q1,q2}], 7] /. pru, q1, q2,
				IncludePair -> (IncludePair /. {opt} /.	Options[FDS])],
				(* This is the usual routine *)
				FixedPoint[fadalll[#, q1, q2]&,	exp-SelectFree[exp,{q1,q2}], 7] /. pru
			]
		];

		If[ (Apart2 /. {opt} /. Options[FDS]),
			res = Apart2[res]
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
		PropagatorDenominator[-Momentum[q1,dim_:4], m_]:>PropagatorDenominator[Momentum[q1,dim], m],
		PropagatorDenominator[-Momentum[q2,dim_:4], m_]:>PropagatorDenominator[Momentum[q2,dim], m],
		PropagatorDenominator[Momentum[p, dim1_:4]-Momentum[q1, dim2_:4], m_]:>PropagatorDenominator[Momentum[q1, dim2]-Momentum[p, dim1],m],
		PropagatorDenominator[Momentum[p, dim1_:4]-Momentum[q2, dim2_:4], m_]:>PropagatorDenominator[Momentum[q2, dim2]-Momentum[p, dim1],m],
		PropagatorDenominator[Momentum[q2, dim1_:4]-Momentum[q1, dim2_:4], m_]:>PropagatorDenominator[Momentum[q1, dim2]-Momentum[q2, dim1],m]
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


procan[a_, m_] :=
	Block[ {tt , one},
		tt = Factor2[one MomentumExpand[a]];
		If[ NumericalFactor[tt] === -1,
			PD[-tt/.one->1, m],
			PD[tt/.one->1, m]
		]
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
					FeynCalcInternal -> False];
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
					FeynCalcInternal -> False]
				];
			];
			re
		]
	];

(*
nufaQ[xx_Times] := NumericalFactor[xx] < 0;
nufaQ[xx_Plus] := NumericalFactor[xx[[1]]] < 0;
*)
nufaQ[_Momentum] :=
	True;
nufaQ[xx_Times] :=
	NumericalFactor[xx] > 0;
nufaQ[xx_Plus] :=
	NumericalFactor[xx[[1]]] > 0;

ftr[xx_Plus,q1_] :=
	Map[ftr[#,q1]&, xx];

(* This is just a simple shift for 3-point functions
	1/[(q1^2-m1^2)((q1-p)^2-m2^2)((q1-p)^2-m3^2)] ->
	1/[((p-q1)^2-m1^2)(q1^2-m2^2)(q1^2-m3^2)] *)
ftr[a_. FeynAmpDenominator[
		PD[Momentum[q1_,di_:4], m1_],
		PD[Momentum[q1_,di_:4] - Momentum[pe_,di_:4], m2_],
		PD[Momentum[q1_,di_:4] - Momentum[pe_,di_:4], m3_]], q1_] :=
	Expand2[EpsEvaluate[ExpandScalarProduct[(a FeynAmpDenominator[
			PD[Momentum[q1,di] - Momentum[pe,di],m2],
			PD[Momentum[q1,di] - Momentum[pe,di],m3],
			PD[Momentum[q1,di],m1]]) /. q1 -> (-q1 + pe),
			FeynCalcInternal -> False]], q1] && !$NoShifts;
		(* /; {m2,m3} =!= Sort[{m2,m3}];*) (*should not be necessary, changed Oct. 2003 *)

qtr[xx_Plus,q1_] :=
	Map[qtr[#,q1]&, xx];

mtr[xx_Plus,q1_] :=
	Map[mtr[#,q1]&, xx];

(*	Scaleless 1-loop integral of type 1/(q+p)^2*)
mtr[a_. FeynAmpDenominator[PD[pe_. + Momentum[q1_,___], 0]], q1_] :=
	0 /; FreeQ[{a,pe}, q1];

(* Same, but now with OPEDelta^2 (the only scale is OPEDelta^2)	*)
mtr[a_. (Power2[_. + _. Pair[Momentum[q1_,___], Momentum[OPEDelta,___]], (vv_/; Head[vv] =!= Integer)]) *
	FeynAmpDenominator[PD[pe_. + Momentum[q1_, _:4], 0]],	q1_] :=
	0 /; FreeQ[{a, pe}, q1](* && !FreeQ[w, q1]*);

(* Same, but now with OPEDelta^2 (the only scale is OPEDelta^2)	*)
mtr[a_. ((_. + _. Pair[Momentum[q1_,___], Momentum[OPEDelta,___]])^(vv_/; Head[vv] =!= Integer))*
	FeynAmpDenominator[PD[pe_. + Momentum[q1_,_:4], 0]], q1_] :=
	0 /; FreeQ[{a,pe}, q1](* && !FreeQ[w,q1]*);

(* 	Shift q->-q-p from 	(...)/[(q+p)^2-m^2)...] to
						(...)/[q^2-m^2)...]  and then
	send this again to FDS	*)
mtr[a_. FeynAmpDenominator[PD[pe_ + Momentum[q1_,di___],m1_], anymore___], q1_] :=
	Expand2[EpsEvaluate[FDS[
	ExpandScalarProduct[(a FeynAmpDenominator[PD[pe + Momentum[q1,di], m1],anymore]) /.
	q1 -> (-q1-(pe/.Momentum[bla_,___]:>bla)),
	FeynCalcInternal -> False]]], q1] /; $Power2 === True && !$NoShifts;

(* 	Shift q->-q-p from (...)/[(q+p)^2-m^2)^n] to (...)/[(q^2-m^2)^n]	*)
mtr[a_. FeynAmpDenominator[b:PD[pe_ + Momentum[q1_,_:4], _]..]] :=
	Expand2[EpsEvaluate[ExpandScalarProduct[(a FeynAmpDenominator[b]) /.
	q1 -> (q1-pe), FeynCalcInternal -> False]],q1]  && !$NoShifts

(* 	needs to work also for more than 1-loop ... *)
(* 	n-point function,	Shift q->-q from
	(...)/[(q^2-m0^2)...(q+p)^2-mi^2)...] to
	(...)/[(q^2-m0^2)...(q-p)^2-mi^2)...]  and then
	send this again to FDS	*)
mtr[a_. FeynAmpDenominator[b:PD[Momentum[q1_,di___], _].., c___PD,
	d:PD[pe_ + Momentum[q1_,di___], _].., z___], q1_] :=
	FDS[EpsEvaluate[ExpandScalarProduct[
	(a FeynAmpDenominator[ b, c, d, z]) /. q1 -> (-q1),
	FeynCalcInternal -> False]]] /; nufaQ[pe]  && !$NoShifts
		(* this would give + signs *)
		(*    ] /; !nufaQ[pe]; *)

(* 	n-point function,	Ordering from	(...)/[(q^2-..^2)...(q+p2)^2-..^2)...(q+p1)^2-..^2)] to
					(...)/[(q^2-..^2)...(q+p1)^2-..^2)...(q+p2)^2-..^2)] and
	then send this again to FDS	*)
mtr[a_. FeynAmpDenominator[	b:PD[Momentum[q1_,di___], _].., d:PD[pe1_ + Momentum[q1_,di___], _]..,
			e___, f:PD[pe2_ + Momentum[q1_,di___], _].., g___], q1_] :=
	FDS[EpsEvaluate[ExpandScalarProduct[(a FeynAmpDenominator[ b,f,e,d,g ]),
	FeynCalcInternal -> False]]] /; !OrderedQ[{pe1^2,pe2^2}];

(* 	2-point function,	Shift q->q-p from 	(...)/[(x*q+y)^2-m^2)(q+p)^2-m^2)] to
											(...)/[q^2-m^2)(x*(q-p)+y)^2-m^2)].	*)
mtr[a_. FeynAmpDenominator[ PD[pa_ + fa_. Momentum[q1_,di___], m1_],
	PD[pe_ + Momentum[q1_,di___], m1_]],q1_] :=
	Expand2[EpsEvaluate[ExpandScalarProduct[(a FeynAmpDenominator[PD[pe + Momentum[q1,di], m1],
	PD[pa + fa Momentum[q1,di],m1]]) /.	q1 -> (q1-pe), FeynCalcInternal -> False]],q1] /;
	nufaQ[pe] && $Power2 === True  && !$NoShifts

(* 	reducible to 2-point functions,
	Shift q->q-p1 from 	(...)/[q^2-(x*m)^2)^n (q+p)^2-m^2)^m] to
						(...)/[q^2-m^2)^m (q-p)^2-(x*m)^2)^n].	*)
mmtr[a_.FeynAmpDenominator[
			b:PD[Momentum[q1_,di___], xi_ m1_]..,
			c:PD[pe_ + Momentum[q1_,di___], m1_]..], q1_] :=
	Expand2[EpsEvaluate[ExpandScalarProduct[(a FeynAmpDenominator[c, b]) /.
	q1 -> (q1-(pe/.Momentum[bla_,___]:>bla)), FeynCalcInternal -> False]],
	q1] && !$NoShifts


(* 	reducible 3-point functions,
	Shift q->-q+p1 from 	(...)/[q^2-m1^2)^n (q-p1)^2-m2^2)^m (q-p1+p2)^2-m3^2)^l] to
						(...)/[q^2-m2^2)^m (-q+p)^2-m1^2)^n (-q+p2)^2-m3^2)^l].
	then send this again to FDS	*)
mmtr[a_.FeynAmpDenominator[
			b:PD[Momentum[q1_,di___], m1_]..,
			c:PD[Momentum[q1_,di___]-Momentum[pe1_,___],_]..,
			d:PD[Momentum[q1_,di___]-Momentum[pe1_,___] + Momentum[_,___], _]..
		], q1_] :=
	FDS[Expand2[EpsEvaluate[ExpandScalarProduct[
	(a FeynAmpDenominator[ c, b, d ]) /. q1 -> (-q1+pe1),
	FCI -> False]], q1]]/; m1=!=0  && !$NoShifts;

(* 	reducible  3-point functions,
	Shift q->-q+p1 from 	(...)/[q^2-m1^2)^n (q-p1+p2)^2-m2^2)^m (q-p1)^2-m3^2)^l] to
							(...)/[q^2-m3^2)^l (-q+p1)^2-m1^2)^n (-q+p2)^2-m2^2)^m ].
	then send this again to FDS	*)
mmtr[a_.FeynAmpDenominator[
			b:PD[Momentum[q1_,di___], m1_]..,
			d:PD[Momentum[q1_,di___]-Momentum[pe1_,___] + Momentum[_,___],_]..,
			c:PD[Momentum[q1_,di___]-Momentum[pe1_,___], _]..], q1_] :=
	FDS[Expand2[EpsEvaluate[ExpandScalarProduct[
	(a FeynAmpDenominator[ c, b, d ]) /. q1 -> (-q1+pe1), FCI -> False]],
	q1]]/; m1=!=0  && !$NoShifts

(* 	reducible  3-point functions,
	Shift q->q-p2 from 	(...)/[q^2-m1^2)^n (q+p2)^2-m2^2)^m (q-p1+p2)^2-m3^2)^l] to
							(...)/[q^2-m2^2)^m (q-p2)^2-m1^2)^n  (q-p1)^2-m3^2)^l].
	then send this again to FDS	*)
mmtr[a_.FeynAmpDenominator[
			b:PD[Momentum[q1_,di___], m1_]..,
			c:PD[Momentum[q1_,di___]+Momentum[pe2_,___],_]..,
			d:PD[Momentum[q1_,di___]-Momentum[_,___] + Momentum[pe2_,___],_]..], q1_] :=
	FDS[Expand2[EpsEvaluate[ExpandScalarProduct[
	(a FeynAmpDenominator[ c,b,d ]) /. q1 -> (q1-pe2), FCI -> False]], q1 ]]/; m1=!=0  && !$NoShifts;

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
		) /. q1->(-q1+pe-pi),FeynCalcInternal -> False];
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
		) /. q1->(-q1+pe),FeynCalcInternal -> False];
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
			) /. q1->(-q1+pe),FeynCalcInternal -> False];
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

qid[xx_,_] :=
	xx;
sumtra[xx_,q1_] :=
	Block[{tmp},
		If[ !FreeQ[xx, PD[_,m_/;m=!=0]],
			tmp =  mtr[xx,q1] /. mtr -> qid,
			If[	FreeQ[xx, (pow_ /; (pow === Power2 || pow === Power))[_.Pair[_,Momentum[q1,_]]+_. ,
				w_/;(Head[w]=!=Integer)]],
				If[ FreeQ[xx, FeynAmpDenominator[
					PD[Momentum[q1,___],_],
					PD[Momentum[q1,___]- Momentum[_,___],_],
					PD[Momentum[q1,___]- Momentum[_,___],_]]],
					tmp = mtr[xx,q1] /. mtr -> qid,
					tmp = ftr[xx, q1] /.ftr -> qid
				],
				tmp = PowerSimplify[mtr[mtr[Expand2[xx, q1],q1] /. mtr -> qid, q1] /. mtr -> qid]
			]
		];
		mmtr[tmp,q1]/.mmtr->qid
	];


getmomenta[aa__PD] :=
	Variables[{aa}/. PD[w_,0] :> w];

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
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q1, dim___] +
							Momentum[pe_ /; pe =!= q2, dim___], m_], bb___PD]] :>
								tran[fa FeynAmpDenominator[aa, PD[-Momentum[q1, dim] -
								Momentum[pe, dim], m], bb],	q1, -q1, q2, q2],

							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] +
							Momentum[pe_ /; pe =!= q1, dim___], m_], bb___PD]] :>
								tran[fa FeynAmpDenominator[aa, PD[-Momentum[q2, dim] -
								Momentum[pe, dim], m], bb], q1, q1, q2, -q2]
							}/.
							{
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q1, dim___] -
							Momentum[pe_ /; pe =!= q2, dim___], m_], bb___PD]]:>
								((tran[fa FeynAmpDenominator[aa, PD[Momentum[q1, dim] -
								Momentum[pe, dim], m], bb], q1, -q1 + pe, q2, -q2 + pe])/; (
								MatchQ[(Times@@Union[{aa, PD[Momentum[q1, dim] - Momentum[pe, dim],m],	bb}]),
								PD[Momentum[q1, dim], _] PD[Momentum[q1, dim] -
								Momentum[pe, dim], m] PD[Momentum[q1, dim] -
								Momentum[q2, dim], _] PD[Momentum[q2, dim] -
								Momentum[pe, dim], _]] ||
								MatchQ[(Times@@Union[{aa, PD[Momentum[q1, dim] - Momentum[pe, dim],m], bb}]),
								PD[Momentum[q1, dim], _] PD[Momentum[q1, dim] -
								Momentum[pe, dim], m] PD[Momentum[q2, dim] -
								Momentum[q1, dim], _] PD[Momentum[q2, dim] -
								Momentum[pe, dim], _]])),
							(* f41 *)
							qqq[fa_. FeynAmpDenominator[aa___PD, PD[Momentum[q2, dim___] -
							Momentum[pe_ /; pe =!= q1, dim___], m_], bb___PD]]:>
								((tran[fa FeynAmpDenominator[aa, PD[Momentum[q2, dim] -
								Momentum[pe, dim],m], bb], q2, -q2 + pe, q1, -q1 + pe])/;(
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
		(FCPrint[1,"in translat"]; Expand2[x, FeynAmpDenominator] + nuLlL1 + nuLlL2)] /. {nuLlL1:>0, nuLlL2:>0}
	]];


lenso[PD[x_, m1_], PD[y_, m2_]] :=
	Which[
		m1=!=0 && m2=!=0,
			If[ NTerms[x] < NTerms[y],
				True,
				If[ NTerms[x] === NTerms[y],
					OrderedQ[{x,y}],
					False
				],
				False
			],
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
		FCPrint[0,"Problem in FDS!"];
		Abort[]
	]


feynord[a__PD] :=
	MemSet[feynord[a],
		FeynAmpDenominator @@ Sort[{a}, lenso]
	];
feynord[q_][a__] :=
	MemSet[feynord[q][a],
		FeynAmpDenominator @@
		Join[Sort[SelectNotFree[{a}, q], lenso], Sort[SelectFree[{a}, q], lenso]]
	];

feynsimp[q_][a__PD] :=
	MemSet[feynsimp[q][a],
			Apply[FeynAmpDenominator,
				Expand[MomentumExpand[{a}]] //.
				PD[-Momentum[q,di___] + pe_.,m_] :>
					PD[Momentum[q,di] - pe, m]]
			];

FCPrint[1,"FeynAmpDenominatorSimplify.m loaded."];
End[]
