(* ::Package:: *)



(* :Title: FCHPL                                                       		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Symbols for HPLs											*)

(* ------------------------------------------------------------------------ *)


FCHPL::usage =
"FCHPL[{inds_}, var] represents a harmonic polylogarithm (HPL) with indices
`inds` and argument `var`.";

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
