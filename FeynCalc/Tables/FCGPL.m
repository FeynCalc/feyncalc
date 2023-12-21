(* ::Package:: *)



(* :Title: FCGPL                                                       		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Symbols for GPLs											*)

(* ------------------------------------------------------------------------ *)


FCGPL::usage =
"FCGPL[{inds_}, var] represents a Goncharov polylogarithm (GPL) with indices
inds and argument var.

As of know this symbol merely acts a placeholder and does not implement any
mathematical properties of GPLs.";

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
