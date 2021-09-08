(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopEikonalPropagatorFreeQ										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Returns False if the integral contains eikonal propagators	*)

(* ------------------------------------------------------------------------ *)

FCLoopEikonalPropagatorFreeQ::usage =
"FCLoopEikonalPropagatorFreeQ[exp] checks if the integral is free of eikonal
propagators $\\frac{1}{p \\cdot q+x}$. If the option First is set to False,
propagators that have both a quadratic and linear piece, e.g. $\\frac{1}{p^2 +
p \\cdot q+x}$ will also count as eikonal propagators. The option Momentum can
be used to check for the presence of eikonal propagators only with respect to
particular momenta. The check is performed only for
StandardPropagatorDenominator and CartesianPropagatorDenominator.";

FCLoopEikonalPropagatorFreeQ::failmsg =
"FCLoopEikonalPropagatorFreeQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopEikonalPropagatorFreeQ`Private`"]

Options[FCLoopEikonalPropagatorFreeQ] = {
	First 		-> True,
	Momentum	-> All
};


FCLoopEikonalPropagatorFreeQ[sps_. fad_FeynAmpDenominator, OptionsPattern[]]:=
	Block[{fadList,res, eikProps, optMomentum, optFirst},

			optMomentum = OptionValue[Momentum];
			optFirst	= OptionValue[First];

			fadList = Sort[List@@fad];
			If[	optMomentum=!=All && !MatchQ[optMomentum,{__}],
				Message[FCLoopEikonalPropagatorFreeQ::failmsg,"Incorrect value of the option Momentum"];
				Abort[]
			];

			If[	optMomentum===All,
				eikProps = Cases[fadList, (StandardPropagatorDenominator|CartesianPropagatorDenominator)[w_/;(w===0 && optFirst) || !optFirst,
					x_/;x=!=0,_,_]:> True,Infinity],
				eikProps = Cases[fadList, (StandardPropagatorDenominator|CartesianPropagatorDenominator)[w_/;(w===0 && optFirst) || !optFirst,
					x_/;x=!=0 && !FreeQ2[{x},optMomentum],_,_]:> True,Infinity]
			];

			eikProps==={}
		]/;FreeQ[sps,FeynAmpDenominator]

FCPrint[1,"FCLoopEikonalPropagatorFreeQ.m loaded."];
End[]
