(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCRenameDummyIndices												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Rename dummy Lorentz, Cartesian, SU(N) and Dirac indices 		*)

(* ------------------------------------------------------------------------ *)

FCRenameDummyIndices::usage =
"FCRenameDummyIndices[expr] identifies all dummy Lorentz and $SU(N)$ indices
and changes their names pairwise to random symbols. This can be useful if you
have an expression that contains dummy indices and want to compute the square
of it. For example, the square of GA[a, l, a] equals $16$. However, if you
forget to rename the dummy indices and compute GA[a, l, a, a, l, a] instead of
GA[a, l, a, b, l, b], you will get $64$.";

FCRenameDummyIndices::failmsg =
"Error! FCRenameDummyIndices has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCRenameDummyIndices`Private`"]
rdiVerbose::usage="";

Options[FCRenameDummyIndices] = {
	DotSimplify	-> True,
	Expanding	-> True,
	FCE 		-> False,
	FCI			-> False,
	Head		-> {
		LorentzIndex,
		CartesianIndex,
		SUNIndex,
		SUNFIndex,
		DiracIndex
	},
	FCVerbose	-> False

};

FCRenameDummyIndices[expr_List, opts:OptionsPattern[]] :=
	Map[FCRenameDummyIndices[#,opts] &, expr];

FCRenameDummyIndices[expr_, OptionsPattern[]] :=
	Block[{	indexList = {}, replacementList, exprFCI, ex, res, heads,
			patt, optExpanding, time},

		If [OptionValue[FCVerbose]===False,
			rdiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				rdiVerbose=OptionValue[FCVerbose]
			];
		];

		heads = OptionValue[Head];
		optExpanding = OptionValue[Expanding];

		If[ !OptionValue[FCI],
			exprFCI = FCI[expr],
			exprFCI = expr
		];

		FCPrint[1,"FCRenameDummyIndices: Entering.", FCDoControl->rdiVerbose];
		FCPrint[3,"FCRenameDummyIndices: Entering with ", exprFCI, FCDoControl->rdiVerbose];

		If[	DummyIndexFreeQ[exprFCI, heads],
			FCPrint[1,"FCRenameDummyIndices: No relevant dummy indices found, leaving.", FCDoControl->rdiVerbose];
			Return[exprFCI]
		];

		ex = exprFCI;

		If[ OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "FCRenameDummyIndices: Applying DotSimplify.", FCDoControl->rdiVerbose];
			exprFCI = DotSimplify[exprFCI, FCI->True, Expanding->optExpanding];
			FCPrint[1, "FCRenameDummyIndices: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->rdiVerbose];
			FCPrint[3, "FCRenameDummyIndices: After FCColorIsolate: ", exprFCI, FCDoControl->rdiVerbose];

		];

		If[ optExpanding,
			time=AbsoluteTime[];
			FCPrint[1, "FCRenameDummyIndices: Applying Expand.", FCDoControl->rdiVerbose];
			exprFCI = Expand[exprFCI];
			FCPrint[1, "FCRenameDummyIndices: Expand done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->rdiVerbose];
			FCPrint[3, "FCRenameDummyIndices: After Expand: ", exprFCI, FCDoControl->rdiVerbose];
		];


		If[ Head[heads]=!=List || heads==={},
			Message[FCRenameDummyIndices::failmsg, "You did not specify which index heads should be renamed."];
			Abort[]
		];

		If[	Length[heads]>1,
			patt = Alternatives@@heads,
			patt = Identity@@heads
		];


		If[ Head[exprFCI]===Plus,
			indexList =
				Map[Tally, Map[Cases[#, (patt)[ind_, ___] :> ind, Infinity]&,Apply[List, exprFCI]]]// Flatten[#, 1] & // Union,
			indexList =
				Cases[exprFCI, (patt)[ind_,___] :> ind, Infinity] // Tally;
		];

		FCPrint[1,"FCRenameDummyIndices: List of indices to be randomized: ", StandardForm[indexList], FCDoControl->rdiVerbose];

		If[ Select[indexList, ((#[[2]]) > 2) &]=!={},
			Message[FCRenameDummyIndices::failmsg, "The input expression violates Einstein summation. Some dummy indices appear more than twice."];
			Abort[]
		];

		replacementList = Map[Rule[(h: patt)[#[[1]],di___], h[$AL[Unique[]],di]] &, Cases[indexList, {_, 2}]];

		FCPrint[2,"FCRenameDummyIndices: List of replacement rules: ", replacementList];

		res = ex //. replacementList;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCRenameDummyIndices: Leaving.", FCDoControl->rdiVerbose];
		FCPrint[3,"FCRenameDummyIndices: Leaving with ", res, FCDoControl->rdiVerbose];

		res
	]

FCPrint[1,"FCRenameDummyIndices.m loaded."];
End[]
