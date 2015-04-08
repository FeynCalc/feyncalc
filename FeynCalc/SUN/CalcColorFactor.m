(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CalcColorFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2nd of November 2003 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CalcColorFactor *)

(* ------------------------------------------------------------------------ *)


CalcColorFactor::usage =
"CalcColorFactor[expr] calculates the color factor of expr.
CalcColorFactor is useful for application on FeynArts produced amplitudes.
CalcColorFactor is just a macro function for
CalcColorFactor[x_] := If[FreeQ2[FeynCalcInternal[x], SUNIndex], x,
SUNSimplify[SUNSimplify[(If[ !FreeQ[#1, DiracGamma], DiracTrick[#1], #1] & )[
SUNSimplify[x]], Explicit -> False], Explicit -> True]].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CalcColorFactor`Private`"]

SetAttributes[CalcColorFactor, Listable];

Options[CalcColorFactor] = {SUNNToCACF -> True};

SetAttributes[CalcColorFactor, Listable];

CalcColorFactor[x_Plus, opts:OptionsPattern[]] := CalcColorFactor[#, opts]& /@ x;

CalcColorFactor[x_, OptionsPattern[]] :=
	Module[{tmp = FeynCalcInternal[x]},
		If[FreeQ[tmp, SUNIndex],
			tmp,
			If[Head[tmp]=!=Times,
				fac = 1,
				fac = Select[tmp, FreeQ[#, SUNIndex]&]
			];
			fac SUNSimplify[SUNSimplify[(If[ !FreeQ[#1,
			DiracGamma], DiracTrick[#1], #1] & )[SUNSimplify[tmp/fac]],
			Explicit -> False], Explicit -> True]
		]
	];

FCPrint[1,"CalcColorFactor.m loaded"];
End[]
