(* ::Package:: *)



(* :Title: Anti5															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Anti5 anticommutes g^5 to the right or the left			    *)

(* ------------------------------------------------------------------------ *)

Anti5::usage =
"Anti5[exp] anticommutes all $\\gamma^5$ in exp to the right. Anti5[exp, n]
anticommutes all $\\gamma^5$ $n$-times to the right. Anti5[exp, -n]
anticommutes all $\\gamma^5$ $n$-times to the left.";

Anti5::failmsg =
"Error! Anti5 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`Anti5`Private`"]

a5Verbose::usage="";
anti5MoveLeft::usage="";
anti5MoveRight::usage="";
max::usage="";


commonGamma5Properties 				= FeynCalc`DiracTrick`Private`commonGamma5Properties;
chiralTrickAnticommuting4DimRight	= FeynCalc`DiracTrick`Private`chiralTrickAnticommuting4DimRight;
chiralTrickAnticommuting4DimLeft	= FeynCalc`DiracTrick`Private`chiralTrickAnticommuting4DimLeft;
chiralTrickAnticommutingDDimRight	= FeynCalc`DiracTrick`Private`chiralTrickAnticommutingDDimRight;
chiralTrickAnticommutingDDimLeft	= FeynCalc`DiracTrick`Private`chiralTrickAnticommutingDDimLeft;
gamma5MoveBMHVRight 				= FeynCalc`DiracTrick`Private`gamma5MoveBMHVRight;
gamma5MoveBMHVLeft 					= FeynCalc`DiracTrick`Private`gamma5MoveBMHVLeft;
holdDOT 							= commonGamma5Properties;

Options[Anti5] = {
	FCDiracIsolate	-> True,
	FCE				-> False,
	FCI				-> False,
	FCVerbose		-> False
};

Anti5[a_ == b_, rest___] :=
	Anti5[a,rest] == Anti5[b,rest];

Anti5[expr_List, rest___]:=
	Anti5[#, rest]&/@expr;

Anti5[expr_/; !MemberQ[{List,Equal},expr], opts:OptionsPattern[]] :=
	Anti5[expr, 1, opts];

Anti5[expr_/; !MemberQ[{List,Equal},expr], n_/; !OptionQ[n] && MatchQ[n, Infinity | -Infinity | _Integer], OptionsPattern[]] :=
	Block[ {new = 0,ex,terms,rest,res, eps, freePart, dsPart, diracObjects,
			diracObjectsEval, null1, null2, dsHead, time, repRule},

		If [OptionValue[FCVerbose]===False,
			a5Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				a5Verbose=OptionValue[FCVerbose]
			];
		];

		max = n;

		FCPrint[1, "Anti5: Entering Anti5", FCDoControl->a5Verbose];
		FCPrint[3, "Anti5: Entering with, ", expr , FCDoControl->a5Verbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex, {DiracGamma[5],DiracGamma[6],DiracGamma[7]}],
			Return[ex]
		];


		If[	OptionValue[FCDiracIsolate],

			(* This is the normal mode which works well both for large and small expressions *)
			FCPrint[1, "Anti5: Normal mode.", FCDoControl->a5Verbose];
			time=AbsoluteTime[];
			FCPrint[1, "Anti5: Extracting Dirac objects.", FCDoControl->a5Verbose];
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DiracGammaCombine->True, LorentzIndex->False, DiracChain->True];
			ex = ex /. h_dsHead/; FreeQ2[h,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] :> Identity@@h;

			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//DeleteDuplicates//Sort;
			FCPrint[1, "Anti5: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->a5Verbose],

			(* This is the fast mode for single expressions *)
			FCPrint[1, "Anti5: Fast mode.", FCDoControl->a5Verbose];
			freePart = 0;
			dsPart = dsHead[ex];
			diracObjects = {dsPart}
		];
		FCPrint[3,"Anti5: dsPart: ",dsPart , FCDoControl->a5Verbose];
		FCPrint[3,"Anti5: freePart: ",freePart , FCDoControl->a5Verbose];
		FCPrint[3,"Anti5: diracObjects: ",diracObjects , FCDoControl->a5Verbose];

		FCPrint[1, "Anti5: Moving g^5.", FCDoControl->a5Verbose];
		time=AbsoluteTime[];

		diracObjectsEval = (diracObjects /. DOT->holdDOT /. dsHead->Identity);
		Switch[n,

			_Integer?Positive,
				diracObjectsEval = Map[anti5MoveRight[#,0]&,diracObjectsEval],
			Infinity,
				diracObjectsEval = Map[anti5MoveRight[#,Infinity]&,diracObjectsEval],
			_Integer?Negative,
				diracObjectsEval = Map[anti5MoveLeft[#,0]&,diracObjectsEval],
			-Infinity,
				diracObjectsEval = Map[anti5MoveLeft[#,-Infinity]&,diracObjectsEval],
			_,
			Message[Anti5::failmsg,"Failed to identify the requested number of iterations."];
			Abort[]
		];

		FCPrint[1, "Anti5: Done moving g^5, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->a5Verbose];

		FCPrint[3, "Anti5: diracObjectsEval: ", diracObjectsEval , FCDoControl->a5Verbose];

		time=AbsoluteTime[];
		FCPrint[1, "Anti5: Inserting Dirac objects back.", FCDoControl->a5Verbose];

		diracObjectsEval = diracObjectsEval /. {
			holdDOT->DOT
		} /. (anti5MoveLeft|anti5MoveRight)[z_,_]-> z;
		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"Anti5: repRule: ", repRule, FCDoControl->a5Verbose];
		res = freePart + ( dsPart/. Dispatch[repRule]);
		FCPrint[1, "Anti5: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->a5Verbose];
		FCPrint[3,"Anti5: Intermediate result: ", res, FCDoControl->a5Verbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];


		FCPrint[1, "Anti5: Leaving.", FCDoControl->a5Verbose];
		FCPrint[3, "Anti5: Leaving with ", res, FCDoControl->a5Verbose];

		res
	];


(*	------------------------------------------------------------------------------	*)
(*	------------------------------------------------------------------------------	*)
(*	--------------					Right movers			----------------------	*)
(*	------------------------------------------------------------------------------	*)
(*	------------------------------------------------------------------------------	*)

anti5MoveRight[0, _]:=
	0;

anti5MoveRight[DiracChain[ex_, i_, j_], c_]:=
	DiracChain[anti5MoveRight[ex, c],i,j];

anti5MoveRight[c1_. a_holdDOT + c2_. b_holdDOT + c3_:0, i_]:=
	anti5MoveRight[c1 a,i] + anti5MoveRight[c2 b,i] + anti5MoveRight[c3,i];

(*	4-dimensions, any scheme, move to the right	*)
anti5MoveRight[r_. ch:holdDOT[___, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d__], Infinity] :=
	r anti5MoveRight[FixedPoint[(# /. chiralTrickAnticommuting4DimRight -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommuting4DimRight)&,
							chiralTrickAnticommuting4DimRight@@ch]/. chiralTrickAnticommuting4DimRight->holdDOT, Infinity]/; FreeQ[r,holdDOT] &&
							MatchQ[FCGetDimensions[{d}, ChangeDimension->True],{4}];

(*	-> Limited number of rearrangements	*)
anti5MoveRight[r_. holdDOT[b___, ch: (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), c_, d___], n_Integer?NonNegative] :=
	r anti5MoveRight[FixedPoint[(# /. chiralTrickAnticommuting4DimRight -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommuting4DimRight)&,
							chiralTrickAnticommuting4DimRight@@Join[{b}/. DiracGamma->diga,{ch,c}, {d} /. DiracGamma->diga]]/.
							chiralTrickAnticommuting4DimRight->holdDOT /.diga->DiracGamma, n+1]/; FreeQ[r,holdDOT] && (n+1<=max) &&
							MatchQ[FCGetDimensions[{c}, ChangeDimension->True],{4}];

(*	------------------------------------------------------------------------------	*)

(*	D-dimensions, NDR scheme, move to the right	*)
anti5MoveRight[r_. ch:holdDOT[___, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d__], Infinity] :=
	r anti5MoveRight[FixedPoint[(# /. chiralTrickAnticommutingDDimRight -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommutingDDimRight)&,
	chiralTrickAnticommutingDDimRight@@ch]/. chiralTrickAnticommutingDDimRight->holdDOT, Infinity]/;
	MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] &&
	(MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{_Symbol}]);

(*			-> Limited number of rearrangements	*)
anti5MoveRight[r_. holdDOT[b___, ch: (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), c_, d___], n_Integer?NonNegative] :=
	r anti5MoveRight[FixedPoint[(# /. chiralTrickAnticommutingDDimRight -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommutingDDimRight)&,
	chiralTrickAnticommutingDDimRight@@Join[{b}/. DiracGamma->diga,{ch,c}, {d} /. DiracGamma->diga]]/.
	chiralTrickAnticommutingDDimRight->holdDOT /.diga->DiracGamma, n+1]/;
	MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] && (n+1<=max) &&
	MatchQ[FCGetDimensions[{c}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{_Symbol}];

(*	------------------------------------------------------------------------------	*)

(*	BMHV scheme, D or D-4 dimensions, move to the right	*)
anti5MoveRight[r_. ch:holdDOT[___, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d__], Infinity] :=
	r anti5MoveRight[FixedPoint[(# /. gamma5MoveBMHVRight -> commonGamma5Properties /. commonGamma5Properties -> gamma5MoveBMHVRight)&,
	gamma5MoveBMHVRight@@ch]/. gamma5MoveBMHVRight->holdDOT, Infinity]/;
	MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] &&
	(!MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{4}]);

(*			-> Limited number of rearrangements	*)
anti5MoveRight[r_. holdDOT[b___, ch: (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), c_, d___], n_Integer?NonNegative] :=
	r anti5MoveRight[FixedPoint[(# /. gamma5MoveBMHVRight -> commonGamma5Properties /. commonGamma5Properties -> gamma5MoveBMHVRight)&,
	gamma5MoveBMHVRight@@Join[{b}/. DiracGamma->diga,{ch,c}, {d} /. DiracGamma->diga]]/.
	gamma5MoveBMHVRight->holdDOT /.diga->DiracGamma, n+1]/;
	MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] && (n+1<=max) &&
	(!MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{4}]);

(*	------------------------------------------------------------------------------	*)
(*	------------------------------------------------------------------------------	*)
(*	--------------					Left movers				----------------------	*)
(*	------------------------------------------------------------------------------	*)
(*	------------------------------------------------------------------------------	*)


anti5MoveLeft[0, _]:=
	0;

anti5MoveLeft[DiracChain[ex_, i_, j_], c_]:=
	DiracChain[anti5MoveLeft[ex, c],i,j];

anti5MoveLeft[c1_. a_holdDOT + c2_. b_holdDOT + c3_:0, i_]:=
	anti5MoveLeft[c1 a,i] + anti5MoveLeft[c2 b,i] + anti5MoveLeft[c3,i];

(*	4-dimensions, any scheme, move to the left	*)
anti5MoveLeft[r_. ch:holdDOT[d__, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), ___], -Infinity] :=
	r anti5MoveLeft[FixedPoint[(# /. chiralTrickAnticommuting4DimLeft -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommuting4DimLeft)&,
							chiralTrickAnticommuting4DimLeft@@ch]/. chiralTrickAnticommuting4DimLeft->holdDOT, -Infinity]/; FreeQ[r,holdDOT] &&
	(MatchQ[FCGetDimensions[{d}, ChangeDimension->True],{4}]);

(*			-> Limited number of rearrangements	*)
anti5MoveLeft[r_. holdDOT[b___, c_, ch:(DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d___], n_Integer?NonPositive] :=
	r anti5MoveLeft[FixedPoint[(# /. chiralTrickAnticommuting4DimLeft -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommuting4DimLeft)&,
							chiralTrickAnticommuting4DimLeft@@Join[{b}/. DiracGamma->diga,{c,ch}, {d} /. DiracGamma->diga]]/.
							chiralTrickAnticommuting4DimLeft->holdDOT /.diga->DiracGamma, n-1]/; FreeQ[r,holdDOT] && (max<=n-1) && MatchQ[FCGetDimensions[{c}, ChangeDimension->True],{4}];

(*	------------------------------------------------------------------------------	*)
(*	D-dimensions, NDR scheme, move to the left	*)
anti5MoveLeft[r_. ch:holdDOT[d__, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), ___], -Infinity] :=
	r anti5MoveLeft[FixedPoint[(# /. chiralTrickAnticommutingDDimLeft -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommutingDDimLeft)&,
							chiralTrickAnticommutingDDimLeft@@ch]/. chiralTrickAnticommutingDDimLeft->holdDOT, -Infinity]/;
	MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] &&
	(MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{_Symbol}]);

(*			-> Limited number of rearrangements	*)
anti5MoveLeft[r_. holdDOT[b___, c_, ch:(DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d___], n_Integer?NonPositive] :=
	r anti5MoveLeft[FixedPoint[(# /. chiralTrickAnticommutingDDimLeft -> commonGamma5Properties /. commonGamma5Properties -> chiralTrickAnticommutingDDimLeft)&,
							chiralTrickAnticommutingDDimLeft@@Join[{b}/. DiracGamma->diga,{c,ch}, {d} /. DiracGamma->diga]]/.
							chiralTrickAnticommutingDDimLeft->holdDOT /.diga->DiracGamma, n-1]/;
		MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] && (max<=n-1) &&
		MatchQ[FCGetDimensions[{c}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{_Symbol}];

(*	------------------------------------------------------------------------------	*)

(*	BMHV scheme, D oder D-4 dimensions, move to the left	*)
anti5MoveLeft[r_. ch:holdDOT[d__, (DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), ___], -Infinity] :=
	r anti5MoveLeft[FixedPoint[(# /. gamma5MoveBMHVLeft -> commonGamma5Properties /. commonGamma5Properties -> gamma5MoveBMHVLeft)&,
							gamma5MoveBMHVLeft@@ch]/. gamma5MoveBMHVLeft->holdDOT, -Infinity]/;
	MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] &&
	!MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{4}];


(*			-> Limited number of rearrangements	*)
anti5MoveLeft[r_. holdDOT[b___, c_, ch:(DiracGamma[5|6|7] | (_. DiracGamma[6] + _. DiracGamma[7])), d___], n_Integer?NonPositive] :=
	r anti5MoveLeft[FixedPoint[(# /. gamma5MoveBMHVLeft -> commonGamma5Properties /. commonGamma5Properties -> gamma5MoveBMHVLeft)&,
							gamma5MoveBMHVLeft@@Join[{b}/. DiracGamma->diga,{c,ch}, {d} /. DiracGamma->diga]]/.
							gamma5MoveBMHVLeft->holdDOT /.diga->DiracGamma, n-1]/;
		MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme] && FreeQ[r,holdDOT] && (max<=n-1) &&
		!MatchQ[FCGetDimensions[{d}, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True],{4}];

FCPrint[1,"Anti5.m loaded."];
End[]
