(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCClausen											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Clausen's identity												*)

(* ------------------------------------------------------------------------ *)


FCClausen::usage =
"FCClausen[x,y] gives the Clausen function with arguments x and y.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCClausen`Private`"]

SetAttributes[FCClausen, NumericFunction];

Options[FCClausen] = {
	Explicit ->	False
};

FCClausen /: N[FCClausen[n_, x_, OptionsPattern[]], pr___] :=
	N[1/2 I (PolyLog[n, Exp[-I x]] - PolyLog[n, Exp[I x]]), pr];

FCClausen[n_,x_, OptionsPattern[]]:=
	1/2 I (PolyLog[n, Exp[-I x]] - PolyLog[n, Exp[I x]])/; OptionValue[Explicit];

FCClausen /:
	MakeBoxes[FCClausen[n_,x_, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox["Cl",TBox[n]],"(", TBox[x],")"}];

FCPrint[1,"FCClausen.m loaded."];
End[]
