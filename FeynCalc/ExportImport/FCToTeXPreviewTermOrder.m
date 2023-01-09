(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCToTeXPreviewTermOrder												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Overrides Mathematica's term ordering						*)

(* ------------------------------------------------------------------------ *)

FCToTeXPreviewTermOrder::usage =
"FCToTeXPreviewTermOrder[exp] displays the output of FCToTeXReorder using the
built-in Plus and Times but preserving the original ordering.

Use ReleaseHold or FRH to allow Mathematica return to its original ordering.

Notice that the output of FCToTeXPreviewTermOrder is not suitable for
algebraic manipulations but should be understood as an intermediate expression
form created to serve as an input for TeXForm";

FCToTeXPreviewTermOrder::failmsg = "Error! FCToTeXPreviewTermOrder has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`FCToTeXPreviewTermOrder`"]
End[]

Begin["`FCToTeXPreviewTermOrder`Private`"]


Options[FCToTeXPreviewTermOrder] = {};

FCToTeXPreviewTermOrder[ex_, OptionsPattern[]] :=
	Block[{	Plus, Times, li, res},

		SetAttributes[Times, Flat];
		res = ex //. {
			{a_, b_, Times} /; FreeQ[a, List] && FreeQ[b, List] :> Times[a, b],
			{a_, b__, Plus} /; FreeQ[a, List] && FreeQ[li[b], List] :> HoldForm[Plus[a, b]]
		};
		res
	]

FCPrint[1,"FCToTeXPreviewTermOrder.m loaded."];
End[]
