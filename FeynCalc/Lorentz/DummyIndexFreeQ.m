(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DummyIndexFreeQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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
	Block[{fullLiList, freeLiList, ex, times, i, sel, sel2},


		FCPrint[1, "DummyIndexFreeQ: Entering."];
		FCPrint[3, "DummyIndexFreeQ: Entering with: ", expr];

		If[ Head[expr]===Times,
			ex = SelectNotFree[expr, heads],
			ex = expr
		];

		ex = ExpandAll2[ex];

		If[Head[ex]===Plus,
			ex=First[ex]
		];

		If[	!FreeQ[ex, Power],
			ex = ex /. Power[a_, b_Integer?Positive] /; !FreeQ2[a, heads] :>
				Apply[times, Table[a, {i, b}]];
		];

		FCPrint[3, "DummyIndexFreeQ: Final ex: ", ex];

		sel = Blank/@heads;
		If[	Length[heads]===1,
			sel = Identity@@(Blank/@heads);
			sel2 = Identity@@heads,

			sel = Alternatives@@(Blank/@heads);
			sel2 = Alternatives@@heads
		];

		FCPrint[1, "DummyIndexFreeQ: sel: ", sel];

		freeLiList = Cases[ex, sel, Infinity]//
			ReplaceAll[#, (sel2)[z_, ___] :> z] & // Tally // Cases[#, {z_, 1} :> z] &;

		FCPrint[3, "DummyIndexFreeQ: freeLiList: ", freeLiList];

		fullLiList = Cases[expr, sel, Infinity] // DeleteDuplicates // Sort;
		FCPrint[3, "DummyIndexFreeQ: fullLiList: ", fullLiList];

		fullLiList = fullLiList // ReplaceAll[#, (sel2)[z_, ___] :> z] & // DeleteDuplicates // Sort;
		FCPrint[3, "DummyIndexFreeQ: fullLiList: ", fullLiList];

		(Complement[fullLiList, freeLiList] === {})
	]/; !FreeQ2[expr,heads];

FCPrint[1,"DummyIndexFreeQ.m loaded."];
End[]
