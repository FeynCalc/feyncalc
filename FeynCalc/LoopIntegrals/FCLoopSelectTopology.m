(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSelectTopology												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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
	FCE->False
};

FCLoopSelectTopology[glis_List,topos:{__FCTopology},opts:OptionsPattern[]]:=
	Union[Flatten[FCLoopSelectTopology[#,topos,opts]&/@glis]]/;
	MatchQ[glis,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}];


FCLoopSelectTopology[glis_, topos:{__FCTopology},opts:OptionsPattern[]]:=
	Union[Flatten[FCLoopSelectTopology[#,topos,opts]&/@Cases2[glis,GLI]]]/;
	!MatchQ[glis, _List | _GLI] &&
	MatchQ[glis,(Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])];

FCLoopSelectTopology[GLI[id_,_List], topos:{__FCTopology}, OptionsPattern[]] :=
	Block[{res},

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopSelectTopology::failmsg, "The supplied list of topologie is incorrect."];
			Abort[]
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

FCPrint[1,"FCLoopSelectTopology.m loaded."];
End[]
