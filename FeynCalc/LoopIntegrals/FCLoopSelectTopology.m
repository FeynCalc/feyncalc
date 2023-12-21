(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSelectTopology												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Selects the topology for the given GLI						*)

(* ------------------------------------------------------------------------ *)

FCLoopSelectTopology::usage =
"FCLoopSelectTopology[int, topos] selects the topology that matches the GLI int
from a list of topologies topos.

The first argument can be also a list, in which case the function will return
a list of matching topologies.";

FCLoopSelectTopology::failmsg =
"Error! FCLoopSelectTopology has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopSelectTopology`Private`"]

fclsVerbose::usage = "";

Options[FCLoopSelectTopology] = {
	FCE		-> False,
	Check	-> True
};

FCLoopSelectTopology[ex_/;Head[ex]=!=List, topos:{__FCTopology}, opts:OptionsPattern[]] :=
	First[FCLoopSelectTopology[{ex}, topos, opts]];

FCLoopSelectTopology[glisRaw_List, topos:{__FCTopology}, OptionsPattern[]] :=
	Block[{res, glis, gliTopos,null},

		If[	OptionValue[Check],
			If[	!FCLoopValidTopologyQ[topos],
				Message[FCLoopSelectTopology::failmsg, "The supplied list of topologie is incorrect."];
				Abort[]
			];
		];

		If[	TrueQ[!MatchQ[glisRaw, {__GLI}]],
			glis = Cases2[glisRaw+null,GLI],
			glis = glisRaw
		];

		gliTopos=Union[First/@glis];

		res = Sort[Select[topos,MemberQ[gliTopos,First[#]]&]];

		If[	res==={},
			Message[FCLoopSelectTopology::failmsg,"There are no topologies that appear in the given list of GLIs"];
			Abort[]
		];
		(*
		If[MatchQ[glisRaw, {_GLI}],
			res=First[res]
		];*)

		If[	OptionValue[FCE],
			res =FCE[res]
		];

		res
	]
(*
FCLoopSelectTopology[GLI[id_,_List], topos:{__FCTopology}, OptionsPattern[]] :=
	Block[{res},

		If[	OptionValue[Check],
			If[	!FCLoopValidTopologyQ[topos],
				Message[FCLoopSelectTopology::failmsg, "The supplied list of topologie is incorrect."];
				Abort[]
			];
		];

		res = Select[topos,(#[[1]]===id)&];
		If[	res==={},
			Message[FCLoopSelectTopology::failmsg,"There are no topologies with the id " <> ToString[id]];
			Abort[]
		];

		res = First[res];

		If[	OptionValue[FCE],
			res =FCE[res]
		];

		res
	]
*)
FCPrint[1,"FCLoopSelectTopology.m loaded."];
End[]
