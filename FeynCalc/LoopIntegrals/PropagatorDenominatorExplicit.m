(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PropagatorDenominatorExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PropagatorDenominatorExplicit *)

(* ------------------------------------------------------------------------ *)


PropagatorDenominatorExplicit::usage =
"PropagatorDenominatorExplicit[exp] changes each occurence of
PropagatorDenominator[a,b]
in exp into 1/(ScalarProduct[a,a]-b^2) and replaces FeynAmpDenominator
by Times.";

PDEHead::usage =
"PDEHead is an option of PropagatorDenominatorExplicit. It allows
to wrap propagators into a specified head after applying
PropagatorDenominatorExpliciti. The default value is Identity.";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PropagatorDenominatorExplicit`Private`"]

Options[PropagatorDenominatorExplicit] = {
	PDEHead->Identity
}

PropagatorDenominatorExplicit[x_, OptionsPattern[]] :=
	If[ FreeQ[x, PropagatorDenominator],
		x,
		x //. {
				PropagatorDenominator[a_  /; !FreeQ[a, Momentum] ,b_] :>
				(1/Expand[ExpandScalarProduct[Pair[a, a]] - b^2]),
				PropagatorDenominator[a_  /; FreeQ[a, Momentum] ,b_] :>
				(1/Expand[ExpandScalarProduct[Pair[Momentum[a], Momentum[a]]] -
						b^2]),
				FeynAmpDenominator[c___] :> OptionValue[PDEHead][Times[c]]
				}
	];

FCPrint[1,"PropagatorDenominatorExplicit.m loaded."];
End[]
