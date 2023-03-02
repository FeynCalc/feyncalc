(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopTopologyNameToSymbol										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Converts topo ids to symbols								*)

(* ------------------------------------------------------------------------ *)

FCLoopTopologyNameToSymbol::usage =
"FCLoopTopologyNameToSymbol[exp] converts topology names in FCTopologys and
GLIs that are strings to expressions. This can be useful when exporting
expressions generated with Mathematica to other software tools.

Using the option Except one can exclude certain names from the conversion
process.";

FCLoopTopologyNameToSymbol::failmsg =
"Error! FCLoopTopologyNameToSymbol has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopTopologyNameToSymbol`Private`"]

fcltntsVerbose::usage = "";

Options[FCLoopTopologyNameToSymbol] = {
	FCVerbose->False,
	Except -> {}
};


FCLoopTopologyNameToSymbol[expr_, OptionsPattern[]] :=
	Block[{gliTopoList, gliTopoListEval, optExcept, res, repRule},

		If[	OptionValue[FCVerbose] === False,
			fcltntsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcltntsVerbose = OptionValue[FCVerbose]];
		];

		optExcept = OptionValue[Except];


		If[	FreeQ2[expr,{FCTopology,GLI}],
			FCPrint[1,"FCLoopTopologyNameToSymbol: Nothing to do, leaving", FCDoControl->fcltntsVerbose];
			Return[expr]
		];

		gliTopoList = Cases2[expr,{GLI,FCTopology}];

		FCPrint[3,"FCLoopTopologyNameToSymbol: Raw list:", gliTopoList, FCDoControl->fcltntsVerbose];

		If[	optExcept=!={},

			If[!MatchQ[optExcept,{__String}],
				Message[FCLoopTopologyNameToSymbol::failmsg,"The value of the option Except must be a list of strings or an empty list"];
				Abort[]
			];

			gliTopoList = Select[gliTopoList,StringFreeQ[#[[1]],optExcept]&];
			FCPrint[3,"FCLoopTopologyNameToSymbol: Filtered list:", gliTopoList, FCDoControl->fcltntsVerbose];
		];

		gliTopoListEval = gliTopoList/. {
			GLI[id_String, rest___]:> GLI[ToExpression[id],rest],
			FCTopology[id_String, rest___]:> GLI[ToExpression[id],rest]
		};

		repRule = Thread[Rule[gliTopoList,gliTopoListEval]];

		res = expr /. Dispatch[repRule];

		res
	]

FCPrint[1,"FCLoopTopologyNameToSymbol.m loaded."];
End[]
