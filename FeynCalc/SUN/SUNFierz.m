(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFierz     													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Fierz identity of two color deltas							*)

(* ------------------------------------------------------------------------ *)

SUNFierz::usage =
"SUNFierz[exp, {i, j, k, l}] applies Fierz identity to the product of two
Kronecker deltas the in fundamental representation (SUNFDelta) carrying
indices i, j, k and l present in exp.";

SUNFierz::failmsg =
"Error! SUNFierz has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`SUNFierz`Private`"]

sunSiVerbose::usage="";
dummyInd::usage="";
optSUNTraceEvaluate::usage="";

Options[SUNFierz] = {
	FCI 				-> False,
	FCE 				-> False,
	FCVerbose 			-> False,
	SUNIndexNames		-> {}
};

SUNFierz[expr_, {i1_,i2_,i3_,i4_}/;!OptionQ[{i1,i2,i3,i4}], OptionsPattern[]] :=
	Block[{	ex, time, sunsiIso, listColoredObjects, res, listColoredObjectsEval, finalRepRule},

		If [OptionValue[FCVerbose]===False,
			sunSiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				sunSiVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "SUNFierz: Entering.", FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNFierz: Entering with ", ex, FCDoControl->sunSiVerbose];

		If[	FreeQ[ex,SUNFDelta],
			(*  Nothing to do *)
			Return[ex]
		];

		(* Isolate everything except for the color structures that we are interested in*)
		time=AbsoluteTime[];
		FCPrint[1, "SUNFierz: Collecting terms w.r.t. colored objects.", FCDoControl->sunSiVerbose];
		ex = FCColorIsolate[ex, FCI->True,Isolate->True, IsolateFast->True, IsolateNames->sunsiIso, Head->sunObj,
			ClearHeads->{sunObj}, ExceptHeads->{SUNN,CA,CF}, "ExpandNestedDOTs" -> True, FCTraceExpand->True];
		FCPrint[1,"SUNFierz: collecting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNFierz: After collecting terms w.r.t. colored objects: ",ex, FCDoControl->sunSiVerbose];

		listColoredObjects = Cases2[ex, sunObj];

		listColoredObjectsEval = listColoredObjects /. sunObj->Identity;

		FCPrint[3, "SUNFierz: Unique colored objects: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		listColoredObjectsEval = Expand2[#,{SUNFDelta}]&/@listColoredObjectsEval;

		listColoredObjectsEval = (fierzRewrite[#,{i1,i2,i3,i4}]&/@listColoredObjectsEval) /. fierzRewrite[x_,_List] -> x;

		FCPrint[3, "SUNFierz: After fierzRewrite: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		finalRepRule = Thread[Rule[listColoredObjects,listColoredObjectsEval]];

		ex = ex /. Dispatch[finalRepRule];

		If[	OptionValue[SUNIndexNames]=!={},
			ex = FCCanonicalizeDummyIndices[ex, FCI -> True, Head ->{SUNIndex}, SUNIndexNames->OptionValue[SUNIndexNames]];
		];

		(*Remove isolations*)
		ex = FRH[ex, IsolateNames->sunsiIso];


		res = ex;

		If [OptionValue[FeynCalcExternal],
			res = FCE[res]
		];

		res

	];


fierzRewrite[rest_. SUNFDelta[SUNFIndex[i_],SUNFIndex[j_]] SUNFDelta[SUNFIndex[k_],SUNFIndex[l_]],{i_,j_,k_,l_}] :=
	(
	dummyInd=SUNIndex[FCGV[ToString[Unique["sun"]]]];
	1/SUNN fierzRewrite[rest SUNFDelta[SUNFIndex[i],SUNFIndex[l]] SUNFDelta[SUNFIndex[k],SUNFIndex[j]],{i,j,k,l}]+
	2 fierzRewrite[rest SUNTF[{dummyInd},SUNFIndex[i],SUNFIndex[l]] SUNTF[{dummyInd},SUNFIndex[k],SUNFIndex[j]],{i,j,k,l}]
	);


FCPrint[1,"SUNFierz.m loaded."];
End[]
