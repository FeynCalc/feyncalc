(* ::Package:: *)



(* :Title: FCHPL                                                       		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Symbols for HPLs											*)

(* ------------------------------------------------------------------------ *)


FCHPL::usage =
"FCHPL[{inds_}, var] represents a harmonic polylogarithm (HPL) with indices
inds and argument var.

As of know this symbol merely acts a placeholder and does not implement any
mathematical properties of HPLs.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCHPL`Private`"]


FCHPL /:
	MakeBoxes[FCHPL[{inds__}, var_], TraditionalForm]:=
		RowBox[{"H", "(", Sequence @@ Riffle[(ToBoxes /@ {inds}), ","],	"; ", ToBoxes[var], ")"}];

FCHPL[{words__}, 0]:=
	0/; !MatchQ[{words},{0..}];

FCPrint[1,"FCHPL.m loaded"];
End[]
