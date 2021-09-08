(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantFieldDerivative											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gauge covariant field derivative								*)

(* ------------------------------------------------------------------------ *)

CovariantFieldDerivative::usage =
"CovariantFieldDerivative[f[x], x, {li1, li2, ...}] is a covariant derivative
of f[x] with respect to space-time variables x and with Lorentz indices li1,
li2, .... CovariantFieldDerivative has only typesetting definitions by
default. The user is must supply his/her own definition of the actual
function.";

CDr::usage =
"CDr is the shorthand notation for CovariantFieldDerivative.";

Begin["`Package`"]
End[]

(* ------------------------------------------------------------------------ *)

Begin["`CovariantFieldDerivative`Private`"]

CDr = CovariantFieldDerivative;

Options[CovariantFieldDerivative] = {};

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, lis___FCPartialD, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, _, {lis___}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, {lis___}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, _, lis___LorentzIndex, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, lis___FCPartialD], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];

CovariantFieldDerivative /:
	MakeBoxes[CovariantFieldDerivative[a_, lis___List], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[StyleForm["\[ScriptCapitalD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
		MakeBoxes[TraditionalForm[a]], ")"}];


FCPrint[1,"CovariantFieldDerivative.m loaded."];
End[]
