(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetDummyIndices												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts dummy indices										*)

(* ------------------------------------------------------------------------ *)

FCGetDummyIndices::usage =
"FCGetDummyIndices[exp, {head1, head2, ...}] returns the list of dummy indices
from heads head1, head2, ...

As always in FeynCalc, Einstein summation convention is implicitly assumed.";

Begin["`Package`"]
End[]

Begin["`FCGetDummyIndices`Private`"]

Options[FCGetDummyIndices] =
	Options[FCGetFreeIndices];

FCGetDummyIndices[expr_, heads_List, opts:OptionsPattern[]]:=
	FCGetFreeIndices[expr, heads, Inverse->True, opts];

FCPrint[1,"FCGetDummyIndices.m loaded."];
End[]
