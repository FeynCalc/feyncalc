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
	FCE							-> False,
	Check						-> True,
	"OneToOneCorrespondence"	-> False
};

FCLoopSelectTopology[glisRaw_, topo_FCTopology, opts:OptionsPattern[]] :=
	FCLoopSelectTopology[glisRaw, {topo}, opts];

FCLoopSelectTopology[ex_/;Head[ex]=!=List, topos:{__FCTopology}, opts:OptionsPattern[]] :=
	First[FCLoopSelectTopology[{ex}, topos, opts]];

FCLoopSelectTopology[glisRaw_List, topos:{__FCTopology}, OptionsPattern[]] :=
	Block[{res, glis, gliTopos, null, optOneToOne, aux},

		If[	OptionValue[Check],
			If[	!FCLoopValidTopologyQ[topos],
				Message[FCLoopSelectTopology::failmsg, "The supplied list of topologie is incorrect."];
				Abort[]
			];
		];

		optOneToOne = OptionValue["OneToOneCorrespondence"];

		If[	TrueQ[!MatchQ[glisRaw, {__GLI}]],

			If[	TrueQ[MatchQ[glisRaw,{(_GLI | Power[_GLI, _] | 0 | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}]],
				(*List of GLIs involving products*)
				glis = Cases2[#+null,GLI]&/@glisRaw;
				gliTopos=glis /. GLI[id_,__] :> id,

				(*Amplitude*)
				glis = Cases2[glisRaw+null,GLI];
				gliTopos=List/@First/@glis
			],
			(*List of GLIs without any products*)
			glis = glisRaw;
			gliTopos=List/@First/@glis;
		];

		If[	!optOneToOne,
			gliTopos = Union[Flatten[gliTopos]]
		];

		If[	!optOneToOne,
			res = Sort[Select[topos,MemberQ[gliTopos,First[#]]&]],
			aux = Map[Rule[#[[1]], #] &, topos];
			res = gliTopos /. Dispatch[aux]
		];

		If[	res==={},
			Message[FCLoopSelectTopology::failmsg,"There are no topologies that appear in the given list of GLIs"];
			Abort[]
		];

		If[	OptionValue[FCE],
			res =FCE[res]
		];

		res
	];

FCPrint[1,"FCLoopSelectTopology.m loaded."];
End[]
