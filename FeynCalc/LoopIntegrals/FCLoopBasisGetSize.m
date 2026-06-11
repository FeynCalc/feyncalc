(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisGetSize												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Gets basis size												*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisGetSize::usage =
"FCLoopBasisGetSize[n1, n2] returns the number of linearly independent
propagators for a topology that contains n1 loop momenta and n2 external
momenta.";

Begin["`Package`"]
End[]

Begin["`FCLoopBasisGetSize`Private`"]


FCLoopBasisGetSize[lmoms_Integer?Positive,emoms_Integer?NonNegative,extra_Integer:0]:=
	lmoms*(lmoms + 1)/2 + lmoms*emoms + extra;

FCPrint[1,"FCLoopBasisGetSize.m loaded."];
End[]
