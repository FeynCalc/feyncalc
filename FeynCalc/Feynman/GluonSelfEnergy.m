(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonSelfEnergy													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gluon self-energy												*)

(* ------------------------------------------------------------------------ *)

GluonSelfEnergy::usage =
"GluonSelfEnergy[{\[Mu], a}, {\[Nu], b}] yields the 1-loop gluon self-energy.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GluonSelfEnergy`Private`"]

Options[GluonSelfEnergy] = {
	Dimension -> D,
	CouplingConstant -> SMP["g_s"],
	FinalSubstitutions -> {Log[ScaleMu^2 _] :> 0, EulerGamma :> Log[4 Pi]},
	Gauge -> 1,
	Momentum -> FCGV["p"]
};

GluonSelfEnergy[{mu_,a_},{nu_,b_}, OptionsPattern[]] :=
	Block[ {alpha, coup, pe, pe2, zdeta, fin , dim},
		coup = OptionValue[CouplingConstant];
		alpha = OptionValue[Gauge];
		pe = OptionValue[Momentum];
		dim  = OptionValue[Dimension];
		fin  = OptionValue[FinalSubstitutions];
		pe2  = Pair[Momentum[pe], Momentum[pe]];
		zdeta = 2/Epsilon + EulerGamma - Log[4 Pi];
		(* gluon + ghost - loop *)
		(I coup^2 CA/2 SUNDelta[SUNIndex[a], SUNIndex[b]] (
		Pair[Momentum[pe, dim], LorentzIndex[mu, dim]]  Pair[Momentum[pe, dim], LorentzIndex[nu, dim]] -
		Pair[LorentzIndex[mu, dim], LorentzIndex[nu, dim]] Pair[Momentum[pe, dim], Momentum[pe, dim]]) *
		( zdeta (10/3 + (1-alpha)) - 62/9 -    10/3 Log[ScaleMu^2/pe2] +(1-alpha) ( 2 - Log[ScaleMu^2/pe2]) -
		1/2 (1-alpha)^2    ) +    I coup^2 Tf SUNDelta[SUNIndex[a], SUNIndex[b]] (Pair[Momentum[pe, dim], LorentzIndex[mu, dim]] *
		Pair[Momentum[pe, dim], LorentzIndex[nu, dim]] - Pair[LorentzIndex[mu, dim], LorentzIndex[nu, dim]] *
		Pair[Momentum[pe, dim], Momentum[pe, dim]]) ( -4/3 zdeta + 20/9 + 4/3 Log[ScaleMu^2/pe2])) /. fin
	];

FCPrint[1,"GluonSelfEnergy.m loaded"];
End[]
