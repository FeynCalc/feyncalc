(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GhostPropagator													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary: Ghost propagator												*)

(* ------------------------------------------------------------------------ *)

GHP::usage =
"GHP is equivalent to GhostPropagator.";

GhostPropagator::usage =
"GhostPropagator[p, a, b] gives the ghost propagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GhostPropagator`Private`"]

GHP = GhostPropagator;

Options[GhostPropagator] = {
	Dimension -> D,
	Explicit -> False
};

GhostPropagator[pi_, OptionsPattern[]] :=
	Block[ {p, glp},
		p = Momentum[pi, OptionValue[Dimension]];
		glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]];
		QCDFeynmanRuleConvention[GhostPropagator] glp
	] /; OptionValue[Explicit];

GhostPropagator[pi_, ai_, bi_, OptionsPattern[]] :=
	Block[ {p, a, b, glp},
		p = Momentum[pi, OptionValue[Dimension]];
		a = SUNIndex[ai];
		b = SUNIndex[bi];
		glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]] SUNDelta[a, b];
		glp
	] /; OptionValue[Explicit];

GhostPropagator /:
	MakeBoxes[GhostPropagator[p_,a_,b_], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]", TBox[a,b]],"(", TBox[p], ")"}];

GhostPropagator /:
	MakeBoxes[GhostPropagator[p_], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]", "u"], "(", TBox[p], ")" }];

FCPrint[1,"GhostPropagator.m loaded"];
End[]
