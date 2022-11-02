(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DummyIndexFreeQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Checks if the expression contains dummy indices		*)

(* ------------------------------------------------------------------------ *)



DummyIndexFreeQ::usage =
"DummyIndexFreeQ[exp, {head1, head2, ...}] returns True if the expression
contains dummy indices with heads head1, head2, ... and False otherwise.

As always in FeynCalc, Einstein summation convention is implicitly assumed.

The function is optimized for large expressions, i.e. it is not so good as a
criterion in e.g. Select.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DummyIndexFreeQ`Private`"]

DummyIndexFreeQ[expr_, heads_List] :=
	True/; FreeQ2[expr,heads];

DummyIndexFreeQ[expr_, heads_List] :=
	(FCGetDummyIndices[expr,heads]==={})/; !FreeQ2[expr,heads];

FCPrint[1,"DummyIndexFreeQ.m loaded."];
End[]
