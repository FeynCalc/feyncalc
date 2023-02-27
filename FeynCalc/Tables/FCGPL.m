(* ::Package:: *)



(* :Title: FCGPL                                                       		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Symbols for GPLs											*)

(* ------------------------------------------------------------------------ *)


FCGPL::usage =
"FCGPL[{inds_}, var] represents a Goncharov polylogarithm (GPL) with indices
`inds` and argument `var`.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCGPL`Private`"]


FCGPL /:
	MakeBoxes[FCGPL[{inds__}, var_], TraditionalForm] :=
		RowBox[{"G", "(", Sequence @@ Riffle[(ToBoxes /@ {inds}), ","],	"; ", ToBoxes[var], ")"}];

FCGPL[{words__}, 0]:=
	0/; !MatchQ[{words},{0..}];

FCPrint[1,"FCGPL.m loaded"];
End[]
