(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCReorderList													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Reorder a list according to the given ordering				*)

(* ------------------------------------------------------------------------ *)

FCReorderList::usage =
"FCReorderList[li, ord] reorders the list li according to the given ordering
ord.";

FCReorderList::failmsg =
"Error! FCReorderList has encountered a fatal problem and must abort the computation. The problem reads: `1`";

Begin["`Package`"];

End[]

Begin["`FCReorderList`Private`"];

rangeFu[i_Integer?Positive] :=
	Range[i, i];

rangeFu[{i_Integer?Positive}] :=
	Range[i, i];

rangeFu[{i_Integer?Positive, j_Integer?Positive}] :=
	Range[i, j];

takeFu[l_List, {i_, ___, j_}] :=
	Take[l, {i, j}];

takeFu[l_List, {i_}] :=
	Take[l, {i, i}];

(* This does the same as CANONICA's SectorBoundariesFromDE *)

FCReorderList[li_List, ru_List] :=
	Block[{len, ordering, raw, res},

		len = Length[li];
		ordering = rangeFu /@ ru;

		If[	!MatchQ[ordering, {{_Integer ..} ..}],
			Message[FCReorderList::failmsg, "The given ordering is incorrect."];
			Abort[]
		];

		If[	Sort[Flatten[ordering]] =!= Range[len],
			Message[FCReorderList::failmsg, "The given ordering does not contain all list elements."];
			Abort[]
		];

		res = takeFu[li, #] & /@ ordering;
		res = Join @@ res;

		If[	Sort[res] =!= Sort[li],
			Message[FCReorderList::failmsg, "The reordered list contains a different number of elements than the original one."];
			Abort[]
		];

		res
	]

End[]
