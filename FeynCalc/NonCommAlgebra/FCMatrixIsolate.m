(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCMatrixIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates matrix objects										*)

(* ------------------------------------------------------------------------ *)

FCMatrixIsolate::usage =
"FCMatrixIsolate[exp] wraps the occurring Dirac, Pauli and color objects into
heads specified by the user.";

FCMatrixIsolate::failmsg =
"Error! FCMatrixIsolate has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCMatrixIsolate`Private`"]
relevantPart::usage="";

diracHead::usage="";
pauliHead::usage="";
sunHead::usage="";

optsDirac::usage="";
optsPauli::usage="";
optsSUN::usage="";

fcmiVerbose::usage="";

Options[FCMatrixIsolate] = {
	Collecting		-> True,
	FCColorIsolate 	-> {FCGV["ColorObject"], {FCI->True, Expanding->False}},
	FCDiracIsolate 	-> {FCGV["DiracObject"], {FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False}},
	FCE				-> False,
	FCI				-> False,
	FCPauliIsolate	-> {FCGV["PauliObject"], {FCI->True, PauliChain->True, Expanding->False, FCJoinDOTs->False}},
	FCVerbose 		-> False,
	Factoring		-> Factor,
	Head			-> Identity,
	Ordering 		-> {FCDiracIsolate,FCColorIsolate,FCPauliIsolate},
	TimeConstrained	-> 3
};

FCMatrixIsolate[expr_List, opts:OptionsPattern[]]:=
	FCMatrixIsolate[#, opts]&/@expr

FCMatrixIsolate[expr_/;Head[expr]=!=List, OptionsPattern[]] :=
	Block[{	res, ex, optHead, optOrdering, time,
			optFCDiracIsolate, optFCPauliIsolate, optFCColorIsolate
		},

		optHead				= OptionValue[Head];
		optOrdering 		= OptionValue[Ordering];
		optFCDiracIsolate	= OptionValue[FCDiracIsolate];
		optFCPauliIsolate	= OptionValue[FCPauliIsolate];
		optFCColorIsolate	= OptionValue[FCColorIsolate];

		If [OptionValue[FCVerbose]===False,
			fcmiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcmiVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!MatchQ[optOrdering,{__}] || !FCSubsetQ[{FCDiracIsolate,FCColorIsolate,FCPauliIsolate},Union[optOrdering]],
			Message[FCMatrixIsolate::failmsg, "The value of the Ordering option is incorrect."];
			Abort[]
		];

		If[	Head[optFCDiracIsolate]=!=List || Head[optFCPauliIsolate]=!=List || Head[optFCColorIsolate]=!=List,
			Message[FCMatrixIsolate::failmsg, "The option values of the isolating function must be given as lists."];
			Abort[]
		];

		If[OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1,"FCMatrixIsolate: Entering.", FCDoControl->fcmiVerbose];
		FCPrint[3,"FCMatrixIsolate: Entering with: ", expr, FCDoControl->fcmiVerbose];


		diracHead	= optFCDiracIsolate[[1]];
		pauliHead	= optFCPauliIsolate[[1]];
		sunHead		= optFCColorIsolate[[1]];


		If[	Length[optFCDiracIsolate]===1,
			optsDirac = FilterRules[Options[FCMatrixIsolate], FCDiracIsolate][[1]][[2]][[2]],
			optsDirac	= FilterRules[optFCDiracIsolate[[2]], Except[Head]]
		];

		If[	Length[optFCPauliIsolate]===1,
			optsPauli = FilterRules[Options[FCMatrixIsolate], FCPauliIsolate][[1]][[2]][[2]],
			optsPauli	= FilterRules[optFCPauliIsolate[[2]], Except[Head]]
		];

		If[	Length[optFCColorIsolate]===1,
			optsSUN = FilterRules[Options[FCMatrixIsolate], FCColorIsolate][[1]][[2]][[2]],
			optsSUN	= FilterRules[optFCColorIsolate[[2]], Except[Head]]
		];

		FCPrint[1,"FCMatrixIsolate: Ordering of isolations: ", optOrdering,  FCDoControl->fcmiVerbose];
		optOrdering = optOrdering /. {FCDiracIsolate -> isolateDirac, FCPauliIsolate -> isolatePauli, FCColorIsolate -> isolateSUN};

		ex = relevantPart[ex];

		time=AbsoluteTime[];
		FCPrint[1, "FCMatrixIsolate: Applying isolating functions.", FCDoControl->fcmiVerbose];
		ex = Fold[#2[#1] &, ex, optOrdering];
		FCPrint[1, "FCMatrixIsolate: Done applying isolating functions, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcmiVerbose];
		FCPrint[3, "FCMatrixIsolate: After applying isolating functions: ", ex, FCDoControl->fcmiVerbose];

		res = ex /. relevantPart-> OptionValue[Head];

		If[	OptionValue[Collecting],
			ex = Collect2[ex,{diracHead,pauliHead,sunHead},Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

isolateDirac[x_]:=
	isolateMatrices[x, {FCDiracIsolate, FeynCalc`Package`DiracHeadsList, {diracHead,relevantPart}, optsDirac}];

isolatePauli[x_]:=
	isolateMatrices[x, {FCPauliIsolate, FeynCalc`Package`PauliHeadsList, {pauliHead,relevantPart}, optsPauli}];

isolateSUN[x_]:=
	isolateMatrices[x, {FCColorIsolate, FeynCalc`Package`SUNHeadsList, {sunHead,relevantPart}, optsSUN}];

isolateMatrices[x_, {fu_, checkHeads_List, isoHeads_, opts_}]:=
	Block[{obj, objEval, repRule, time},
		obj = Cases2[x, relevantPart];
		FCPrint[1, "FCMatrixIsolate: isolateMatrices: Entering ", fu, FCDoControl->fcmiVerbose];
		FCPrint[3, "FCMatrixIsolate: isolateMatrices: Entering with", x, FCDoControl->fcmiVerbose];
		If[	FreeQ2[obj, checkHeads],
			Return[x]
		];

		time= AbsoluteTime[];
		FCPrint[1, "FCMatrixIsolate: isolateMatrices: Applying ", fu, FCDoControl->fcmiVerbose];
		objEval = fu[#, Join[opts, {Head->isoHeads}]]&/@ (obj/.relevantPart->Identity);
		FCPrint[1, "FCMatrixIsolate: isolateMatrices: Done applying "<>ToString[fu,InputForm]<>", timing: ",
			N[AbsoluteTime[] - time, 4], FCDoControl->fcmiVerbose];
		repRule = Thread[Rule[obj,objEval]];

		(x /. Dispatch[repRule])

];

FCPrint[1,"FCMatrixIsolate.m loaded."];
End[]
