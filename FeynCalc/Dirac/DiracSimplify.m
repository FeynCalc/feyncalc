(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Like DiracTrick, but including non-commutative expansions
				and simplifications of spinor chains						*)

(* ------------------------------------------------------------------------ *)

DiracSimplify::usage =
"DiracSimplify[exp] simplifies products of Dirac matrices in exp and expands
noncommutative products. The simplifications are done by applying Contract,
DiracEquation, DiracTrick, SpinorChainTrick and SirlinSimplify. All
$\\gamma^5$, $\\gamma^6$ and $\\gamma^7$ are moved to the right. The order of the
other Dirac matrices is not changed, unless the option DiracOrder is set to
True.";

DiracSimplify::failmsg =
"Error! DiracSimplify encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

diracChainContract;
spinorSyntaxCorrectQ;

End[]

Begin["`DiracSimplify`Private`"]

dsVerbose::usage="";
optInsideDiracTrace::usage="";
optExpanding::usage="";
optExpandScalarProduct::usage="";
optDiracGammaCombine::usage="";
optDiracSubstitute67::usage="";
optDiracSubstitute5::usage="";
optDiracSigmaExplicit::usage="";
optDiracTraceEvaluate::usage="";
optDiracEquation::usage="";
optDiracOrder::usage="";
optFactoring::usage="";
optEpsContract::usage="";
optContract::usage="";
optToDiracGamma67::usage="";
optSpinorChainEvaluate::usage="";
optDiracSpinorNormalization::usage="";
optEpsExpand::usage="";

Options[DiracSimplify] = {
	CartesianIndexNames			-> {},
	Contract					-> True,
	DiracChain					-> True,
	DiracChainJoin				-> True,
	DiracEquation				-> True,
	DiracGammaCombine			-> False,
	DiracOrder					-> False,
	DiracSigmaExplicit			-> True,
	DiracSpinorNormalization	-> "Relativistic",
	DiracSubstitute5			-> False,
	DiracSubstitute67			-> False,
	DiracTrace					-> True,
	DiracTraceEvaluate			-> True,
	EpsContract					-> True,
	EpsExpand					-> True,
	Expand2						-> True,
	ExpandScalarProduct			-> True,
	Expanding					-> True,
	FCCanonicalizeDummyIndices	-> False,
	FCCheckSyntax				-> False,
	FCDiracIsolate				-> True,
	FCE							-> False,
	FCI    						-> False,
	FCVerbose					-> False,
	Factoring					-> False,
	InsideDiracTrace    		-> False,
	LorentzIndexNames			-> {},
	SirlinSimplify				-> False,
	SpinorChainTrick			-> True,
	SpinorChainEvaluate			-> True,
	ToDiracGamma67				-> True
};


DiracSimplify[a_ == b_, opts:OptionsPattern[]] :=
	DiracSimplify[a,opts] == DiracSimplify[b,opts];

DiracSimplify[expr_List, opts:OptionsPattern[]] :=
	DiracSimplify[#, opts]&/@expr;

DiracSimplify[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex,res, time, null1, null2, holdDOT, freePart=0, dsPart, diracObjects,
			diracObjectsEval, repRule, tmp, tmpHead, dsHead, dsHeadAll, diracObjectsAll,
			diracObjectsAllEval, optFCCanonicalizeDummyIndices, timeTotal},

		timeTotal = AbsoluteTime[];

		If [OptionValue[FCVerbose]===False,
			dsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dsVerbose=OptionValue[FCVerbose]
			];
		];

		optContract						= OptionValue[Contract];
		optDiracEquation				= OptionValue[DiracEquation];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optDiracOrder					= OptionValue[DiracOrder];
		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracSubstitute5				= OptionValue[DiracSubstitute5];
		optDiracSubstitute67			= OptionValue[DiracSubstitute67];
		optDiracTraceEvaluate   		= OptionValue[DiracTraceEvaluate];
		optEpsContract					= OptionValue[EpsContract];
		optEpsExpand					= OptionValue[EpsExpand];
		optExpandScalarProduct			= OptionValue[ExpandScalarProduct];
		optExpanding  					= OptionValue[Expanding];
		optInsideDiracTrace				= OptionValue[InsideDiracTrace];
		optToDiracGamma67				= OptionValue[ToDiracGamma67];
		optFCCanonicalizeDummyIndices	= OptionValue[FCCanonicalizeDummyIndices];
		optSpinorChainEvaluate			= OptionValue[SpinorChainEvaluate];
		optDiracSpinorNormalization		= OptionValue[DiracSpinorNormalization];

		If[ OptionValue[LorentzIndexNames]=!={} || OptionValue[CartesianIndexNames]=!={},
			optFCCanonicalizeDummyIndices = True
		];

		If[ OptionValue[Factoring] === Automatic,
			optFactoring =
				Function[x, If[ LeafCount[x] <  5000,
								Factor[x],
								x
							]
				],
			optFactoring = OptionValue[Factoring]
		];

		FCPrint[1, "DiracSimplify: Entering.", FCDoControl->dsVerbose];
		FCPrint[3, "DiracSimplify: Entering with ", expr, FCDoControl->dsVerbose];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCCheckSyntax],
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Checking the syntax", FCDoControl->dsVerbose];
			FCCheckSyntax[ex,FCI->True];
			FCPrint[1, "DiracSimplify: Checking the syntax done", FCDoControl->dsVerbose];
			FCPrint[1,"DiracSimplify: Done checking the syntax, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose]
		];

		If[ FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];

		(* Here we separately simplify each chain of Dirac matrices	*)
		If[	OptionValue[FCDiracIsolate],
			(*	This is the standard mode for calling DiracSimplify	*)
			FCPrint[1,"DiracSimplify: Normal mode.", FCDoControl->dsVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Extracting Dirac objects.", FCDoControl->dsVerbose];

			(* 	First of all we need to extract all Dirac structures in the input. *)
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->OptionValue[DiracGammaCombine],
				DiracSigmaExplicit->OptionValue[DiracSigmaExplicit], LorentzIndex->True, CartesianIndex->True,
				ToDiracGamma67->optToDiracGamma67, DiracChain->OptionValue[DiracChain], Split->dsHeadAll];

			If[	!FreeQ[ex,DiracTrace] && !OptionValue[DiracTrace],
				ex = ex /. dsHead[zz_]/; !FreeQ[zz,DiracTrace] :> zz
			];

			{freePart,dsPart} = FCSplit[ex,{dsHeadAll}];

			FCPrint[3,"DiracSimplify: dsPart: ",dsPart , FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: freePart: ",freePart , FCDoControl->dsVerbose];
			FCPrint[1, "DiracSimplify: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
			diracObjectsAll = Cases[dsPart+null1+null2, dsHeadAll[_], Infinity]//Sort//DeleteDuplicates;
			diracObjectsEval = diracObjects;

			FCPrint[3,"DiracSimplify: diracObjects: ", diracObjects , FCDoControl->dsVerbose];

			If[ OptionValue[Contract],
				time=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: Doing index contractions.", FCDoControl->dsVerbose];
				diracObjectsEval = diracChainContract[diracObjectsEval];
				FCPrint[1, "DiracSimplify: Index contractions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracObjectsEval after index contractions: ", diracObjectsEval , FCDoControl->dsVerbose];

			];

			If[ optDiracTraceEvaluate && !FreeQ[diracObjectsEval,DiracTrace],
				time=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: Calculating Dirac traces.", FCDoControl->dsVerbose];
				diracObjectsEval = diracObjectsEval /. DiracTrace[zz_, opts:OptionsPattern[]] :> DiracTrace[zz,
					DiracTraceEvaluate->True, Expand-> optExpandScalarProduct, opts];
				FCPrint[1, "DiracSimplify: Done calculating Dirac traces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracObjectsEval after calcuating Dirac traces: ", diracObjectsEval , FCDoControl->dsVerbose]
			];

			If[ OptionValue[DiracChainJoin] && !FreeQ[diracObjectsEval,DiracChain],
				time=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: Contracting Dirac indices.", FCDoControl->dsVerbose];
				diracObjectsEval = diracObjectsEval /. dsHead[x_]/;!FreeQ[x,DiracChain] :>
					DiracChainJoin[x, FCI->True, FCDiracIsolate->False];
				FCPrint[1, "DiracSimplify: Done contracting Dirac indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3,"DiracSimplify: diracObjectsEval after contracting Dirac indices: ", diracObjects , FCDoControl->dsVerbose]
			];

			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Applying diracSimplifyEval", FCDoControl->dsVerbose];

			diracObjectsEval = FeynCalc`Package`diracTrickEvalFastFromDiracSimplifyList[(diracObjectsEval/.dsHead->Identity), {optInsideDiracTrace,optDiracOrder}];

			diracObjectsEval = diracSimplifyEval/@diracObjectsEval;

			FCPrint[3,"DiracSimplify: After diracSimplifyEval: ", diracObjectsEval, FCDoControl->dsVerbose];
			FCPrint[1,"DiracSimplify: diracSimplifyEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			If[ !FreeQ2[diracObjectsEval,{FeynCalc`Package`diracTrickEvalFastFromDiracSimplifyList,diracSimplifyEval,holdDOT}],
				Message[DiracSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "DiracSimplify: Inserting Dirac objects back into products.", FCDoControl->dsVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[diracObjects,diracObjectsEval]];
			FCPrint[3,"DiracSimplify: repRule: ", repRule, FCDoControl->dsVerbose];
			diracObjectsAllEval = diracObjectsAll /. dsHeadAll->Identity /. Dispatch[repRule];
			FCPrint[1, "DiracSimplify: Done inserting Dirac objects back into products, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose],

			(*
				This is a fast mode for input that is already isolated, e.g. for calling DiracSimplify/@exprList
				from internal functions
			*)
			FCPrint[1,"DiracSimplify: Fast mode.", FCDoControl->dsVerbose];

			tmp = FeynCalc`Package`diracTrickEvalFastFromDiracSimplifySingle[ex, {tmpHead,optInsideDiracTrace,optDiracOrder}];

			(* It might happen that after diracTrickEvalFast there are no Dirac matrices left.*)

			FCPrint[3,"DiracSimplify: After diracTrickEvalFast: ", tmp , FCDoControl->dsVerbose];

			If[ OptionValue[ToDiracGamma67],
				res = res /. tmpHead[zzz_]/;!FreeQ[{zzz}, DiracGamma[5]] :> tmpHead[ToDiracGamma67[zzz,FCI->True]]
			];

			If[ !FreeQ2[tmp,{DiracHeadsList,tmpHead}],
				tmp = tmp /. tmpHead -> diracSimplifyEval
			];

			If[ !FreeQ2[tmp,{FeynCalc`Package`diracTrickEvalFastFromDiracSimplifySingle,diracSimplifyEval,holdDOT}],
				Message[DiracSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			diracObjectsAllEval = tmp;
			diracObjectsAll = dsHeadAll[tmp];
			dsPart = diracObjectsAll

		];
		FCPrint[3, "DiracSimplify: Intermediate result: ", diracObjectsAllEval, FCDoControl->dsVerbose];

		(*
			Here we need to take care of simplifications that can be done only when the full
			Dirac structure in each term is taken into account, i.e. not just single chains but
			the products of all chains. This is handled by SpinorChainTrick
		*)

		If[	OptionValue[SpinorChainTrick],
			FCPrint[1, "DiracSimplify: Applying SpinorChainTrick.", FCDoControl->dsVerbose];
			time=AbsoluteTime[];
			diracObjectsAllEval = SpinorChainTrick[diracObjectsAllEval, FCI->True,FCDiracIsolate->List, DiracOrder -> optDiracOrder,
				FCCanonicalizeDummyIndices->optFCCanonicalizeDummyIndices, DiracEquation-> optDiracEquation,
				Contract -> optContract, LorentzIndexNames->OptionValue[LorentzIndexNames],
				CartesianIndexNames->OptionValue[CartesianIndexNames], SirlinSimplify-> OptionValue[SirlinSimplify]];
			FCPrint[1,"DiracSimplify: Done applying SpinorChainTrick, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: After SpinorChainTrick: ", diracObjectsAllEval, FCDoControl->dsVerbose];
		];


		FCPrint[1, "DiracSimplify: Creating the final replacement rule.", FCDoControl->dsVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjectsAll,diracObjectsAllEval]];
		tmp =  (dsPart /. Dispatch[repRule]);
		tmp = tmp/.dsHeadAll->Identity;

		res = freePart + tmp;
		FCPrint[3,"DiracSimplify: repRule: ", repRule, FCDoControl->dsVerbose];
		FCPrint[1, "DiracSimplify: Final replacement rule done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

		If[	optExpanding && OptionValue[Expand2],
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Expanding the result.", FCDoControl->dsVerbose];
			res = Expand2[res];
			FCPrint[1,"DiracSimplify: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: After expanding: ", res, FCDoControl->dsVerbose]
		];


		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"DiracSimplify: Leaving.", FCDoControl->dsVerbose];
		FCPrint[1,"DiracSimplify: Total timing: ", N[AbsoluteTime[] - timeTotal, 4], FCDoControl->dsVerbose];
		FCPrint[3,"DiracSimplify: Leaving with ", res, FCDoControl->dsVerbose];
		res
	];

(*TODO Figure out some way to treat spinors as well! *)
diracSimplifyEval[DiracChain[expr_,i_,j_]]:=
	DiracChain[diracSimplifyEval[expr],i,j]/; !optExpanding

diracSimplifyEval[DiracChain[expr_,i_,j_]]:=
	DiracChainExpand[DiracChain[diracSimplifyEval[expr],i,j], FCI->True]/; optExpanding

diracSimplifyEval[expr_]:=
	Block[{tmp=expr, time, time2, res},

		(*	General algorithm of diracSimplifyEval:

			1)	Apply DiracTrick to the unexpanded expression
			2)	Expand the expression using DotSimplify and apply DiracTrick again
			3)	If there are uncontracted indices, contract them
			4)	If needed, expand scalar products
			5) 	If needed, replace chiral projectors by their explicit values and apply DotSimplify
				afterwards

			6) 	If the expression contains spinors
				6.1) If needed, apply Dirac equation
				6.2) If needed, apply Sirlin's relations

			7)	If needed, order the remaining Dirac matrices canonically
			8)	If needed, factor the result

		*)



		FCPrint[2, "DiracSimplify: diracSimplifyEval: Entering", FCDoControl->dsVerbose];
		FCPrint[3, "DiracSimplify: diracSimplifyEval: Entering with: ", tmp, FCDoControl->dsVerbose];



		(* First application of DiracTrick, no expansions so far *)
		If[ !FreeQ[tmp, DiracGamma],
			time=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying DiracTrick.", FCDoControl->dsVerbose];
			tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace-> optInsideDiracTrace, FCDiracIsolate->False, ToDiracGamma67->optToDiracGamma67];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: DiracTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After DiracTrick: ", tmp, FCDoControl->dsVerbose]
		];

		(*
			This is mainly relevant for products of traces when using NDR, so that despite of
			DiracTraceEvaluate->True we may end up with unevaluated traces here, like with
			DiracSimplify[DiracTrace[GAD[al, be, mu, nu, 5]] DiracTrace[GAD[al, be, mu, nu]]]
		*)
		If[ !FreeQ[tmp, DiracTrace] && optDiracTraceEvaluate,
			tmp = tmp /. DiracTrace[z_, o:OptionsPattern[]] :> DiracTrace[z,
					DiracTraceEvaluate->True, Expand-> optExpandScalarProduct, o];
		];

		(*	Expansion of Dirac slashes	*)
		If[	!optDiracGammaCombine && !FreeQ[tmp, DiracGamma],
			tmp = DiracGammaExpand[tmp,FCI->True];
		];


		If[	optExpanding && !FreeQ[tmp, DiracGamma],
				time2=AbsoluteTime[];
				FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying Dotsimplify.", FCDoControl->dsVerbose];
				tmp = DotSimplify[tmp, FCI->True, Expanding -> True, FCJoinDOTs->False];
				FCPrint[2,"DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
				FCPrint[3,"DiracSimplify: diracSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->dsVerbose];
		];

		If[ !FreeQ[tmp, DiracGamma],

			If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
				time2=AbsoluteTime[];
				FCPrint[2, "DiracSimplify: diracSimplifyEval: Doing index contractions.", FCDoControl->dsVerbose];
				tmp = diracChainContract[tmp];
				FCPrint[2, "DiracSimplify: diracSimplifyEval: Index contractions done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracSimplifyEval: After index contractions: ", tmp, FCDoControl->dsVerbose];

				time2=AbsoluteTime[];
				FCPrint[2, "DiracSimplify: diracSimplifyEval: Applying EpsContract.", FCDoControl->dsVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]//EpsEvaluate[#,FCI->True, EpsExpand->optEpsExpand]&
				];
				FCPrint[2, "DiracSimplify: diracSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracSimplifyEval: After EpsContract: ", tmp, FCDoControl->dsVerbose];


			];

			time2=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying DiracTrick.", FCDoControl->dsVerbose];
			tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace-> optInsideDiracTrace, FCJoinDOTs->False, ToDiracGamma67->optToDiracGamma67];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: DiracTrick done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After DiracTrick: ", tmp, FCDoControl->dsVerbose];
		];

		(* Doing index contractions *)
		If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
			time2=AbsoluteTime[];
			FCPrint[2, "DiracSimplify: diracSimplifyEval: Doing index contractions.", FCDoControl->dsVerbose];
			tmp = diracChainContract[tmp];
			FCPrint[2, "DiracSimplify: diracSimplifyEval: Index contractions done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: diracSimplifyEval: After index contractions: ", tmp, FCDoControl->dsVerbose];

			time2=AbsoluteTime[];
			FCPrint[2, "DiracSimplify: diracSimplifyEval: Applying EpsContract.", FCDoControl->dsVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]//EpsEvaluate[#,FCI->True, EpsExpand->optEpsExpand]&
				];
			FCPrint[2, "DiracSimplify: diracSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: diracSimplifyEval: After EpsContract: ", tmp, FCDoControl->dsVerbose];
		];

		(* 	Expansion of the scalar products.	*)
		If[ optExpandScalarProduct && !FreeQ[tmp,Momentum],
			time2=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Expanding scalar products", FCDoControl->dsVerbose];
			tmp = ExpandScalarProduct[tmp,FCI->False];
			FCPrint[2,"DiracSimplify:diracSimplifyEvale: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose]
		];

		(*	Substituting the explicit values of Dirac sigma	*)
		If [ optDiracSigmaExplicit && !FreeQ[tmp,DiracSigma],
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Substituting the explicit values of the Dirac sigma.", FCDoControl->dsVerbose];
			tmp = DiracSigmaExplicit[tmp,FCI->True]
		];



		(*	Substituting the explicit values of the chiral projectors	*)
		If[ optDiracSubstitute67 && !FreeQ2[tmp,{DiracGamma[6],DiracGamma[7]}],
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Substituting the explicit values of the chiral projectors.", FCDoControl->dsVerbose];
			tmp = DiracSubstitute67[tmp, FCI->True]
		];

		(*	Rewriting DiracGamma[5] in terms of the chiral projectors 	*)
		If[ optDiracSubstitute5 && !FreeQ[tmp,DiracGamma[5]],
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Rewriting DiracGamma[5] in terms of the chiral projectors.", FCDoControl->dsVerbose];
			tmp = DiracSubstitute5[tmp, FCI->True]
		];


		If[	optExpanding && (!optDiracGammaCombine || optDiracSubstitute67),
			time2=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying Dotsimplify.", FCDoControl->dsVerbose];
			tmp = DotSimplify[tmp, FCI->True, Expanding -> True];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->dsVerbose];
		];


		(*	Dirac equation	*)
		If[	!FreeQ[tmp,Spinor] && optDiracEquation,
			time2=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying DiracEquation.", FCDoControl->dsVerbose];
			tmp = DiracEquation[tmp, FCI->True];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: DiracEquation done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After DiracEquation: ", tmp, FCDoControl->dsVerbose];
		];

		(*	Spinor inner product	*)
		If[	!FreeQ[tmp,Spinor] && optSpinorChainEvaluate,
			time2=AbsoluteTime[];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: Applying SpinorChainEvaluate.", FCDoControl->dsVerbose];
			tmp = SpinorChainEvaluate[tmp, FCI->True, DiracSpinorNormalization->optDiracSpinorNormalization];
			FCPrint[2,"DiracSimplify: diracSimplifyEval: SpinorChainEvaluate done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After SpinorChainEvaluate: ", tmp, FCDoControl->dsVerbose];
		];

		(* Factoring	*)
		If[ optFactoring=!=False,
				time2=AbsoluteTime[];
				FCPrint[2,"DiracSimplify: diracSimplifyEval: Factoring the result.", FCDoControl->dsVerbose];
				tmp = optFactoring[tmp];
				FCPrint[2,"DiracSimplify:diracSimplifyEvale: Done factoring, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose]
		];

		res = tmp;

		FCPrint[3,"DiracSimplify: diracSimplifyEval: Leaving with: ", res, FCDoControl->dsVerbose];

		res


	]/;Head[expr]=!=DiracChain;

diracChainContract[ex_]:=
	ex/; FreeQ2[ex,{Pair,CartesianPair,PairContract,CartesianPairContract}];

diracChainContract[ex_]:=
	(
	Expand2[ex,{Pair,CartesianPair,PairContract,CartesianPairContract}]/. CartesianPair -> CartesianPairContract /. Pair -> PairContract /.
	CartesianPairContract -> CartesianPair /. PairContract -> Pair
	)/; !FreeQ2[ex, {Pair,CartesianPair,PairContract,CartesianPairContract}];

spinorSyntaxCorrectQ[ex_]:=
	Block[{spinors, correct, check, null1, null2},
		correct=True;
		spinors = Cases2[ex+null1+null2,Spinor];
		spinors = MomentumExpand[spinors];
		If[	spinors=!={},
			check = MatchQ[#,Spinor[s_. Momentum[p_/;Head[p]=!=Plus,dim_:4],__]/;(MemberQ[{1,-1},s])]&/@spinors;
			If[	Union[check]=!={True},
				correct=False
			]
		];
		correct
	];

FCPrint[1,"DiracSimplify.m loaded."];
End[]
