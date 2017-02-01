(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DummyLorentzIndexFreeQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Checks if the expression contains dummy Lorentz indices		*)

(* ------------------------------------------------------------------------ *)



DummyLorentzIndexFreeQ::usage =
"DummyLorentzIndexFreeQ[expr] returns True if the expression \
contains dummy Lorentz indices and False otherwise. As always in FeynCalc, \
Einstein summation convention is implicitly assumed. The function is optimized
for large expressions, i.e. it is not so good as a criterion in e.g. Select";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DummyLorentzIndexFreeQ`Private`"]

DummyLorentzIndexFreeQ[expr_] :=
	True/; FreeQ[expr,LorentzIndex];

DummyLorentzIndexFreeQ[expr_] :=
	Block[{fullLiList, freeLiList, ex, times, i},


		FCPrint[1, "DummyLorentzIndexFreeQ: Entering."];
		FCPrint[3, "DummyLorentzIndexFreeQ: Entering with: ", expr];

		If[ Head[expr]===Times,
			ex = SelectNotFree[expr, LorentzIndex],
			ex = expr
		];

		ex = ExpandAll2[ex];

		If[Head[ex]===Plus,
			ex=First[ex]
		];

		If[	!FreeQ[ex, Power],
			ex = ex /. Power[a_, b_Integer?Positive] /; !FreeQ[a, LorentzIndex] :>
				Apply[times, Table[a, {i, b}]];
		];

		FCPrint[3, "DummyLorentzIndexFreeQ: Final ex: ", ex];

		freeLiList = Cases[ex, _LorentzIndex, Infinity]//
			ReplaceAll[#, LorentzIndex[z_, ___] :> z] & // Tally // Cases[#, {z_, 1} :> z] &;

		fullLiList = Cases[expr, _LorentzIndex, Infinity] // DeleteDuplicates // Sort;
		fullLiList = fullLiList // ReplaceAll[#, LorentzIndex[z_, ___] :> z] & // DeleteDuplicates // Sort;
		(Complement[fullLiList, freeLiList] === {})
	]/; !FreeQ[expr,LorentzIndex];

FCPrint[1,"DummyLorentzIndexFreeQ.m loaded."];
End[]
