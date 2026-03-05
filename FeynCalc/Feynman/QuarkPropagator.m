(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkPropagator													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Quark propagator												*)

(* ------------------------------------------------------------------------ *)

QP::usage =
"QP is an alias for QuarkPropagator.

QP[p] is the massless quark propagator.

QP[{p,m}] gives the  quark propagator with mass m.";

QuarkPropagator::usage =
"QuarkPropagator[p] is the massless quark propagator.

QuarkPropagator[{p, m}] gives the quark propagator with mass $m$.

QP can be used as an abbreviation of QuarkPropagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`QuarkPropagator`Private`"]

DeclareNonCommutative[QuarkPropagator];

Options[QuarkPropagator] = {
	Dimension -> D,
	Explicit -> False
};

QP = QuarkPropagator;
Abbreviation[QuarkPropagator] = HoldForm[QP];

QuarkPropagator[pi:Except[_?OptionQ], opt:OptionsPattern[]] :=
	QuarkPropagator[{pi,0}, opt]/; Head[pi]=!=List;

QuarkPropagator[{pi_, m_},  OptionsPattern[]] :=
	Block[ {dim, re, ope, pol, cou, cop, loo},
		dim    = OptionValue[Dimension];
		re = I (DiracGamma[Momentum[pi, dim], dim]+m) FeynAmpDenominator[PropagatorDenominator[MomentumExpand[Momentum[pi,dim]], m]];
		re
	]/; OptionValue[Explicit];


QuarkPropagator /:
	MakeBoxes[QuarkPropagator[{p_,_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubscriptBox["\[CapitalPi]","q"],"(", TBox[p], ")"}];

FCPrint[1,"QuarkPropagator.m loaded"];
End[]
