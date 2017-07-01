(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FieldStrength													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary: Field strength tensor											*)

(* ------------------------------------------------------------------------ *)

FieldStrength::usage =
"FieldStrength[mu,nu,a] is the field strength tensor \
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu^a + \
g f^{abc} A_mu^b A_nu^c. \

FieldStrength[mu,nu] is the field strength tensor \
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu. \
The name of the field (A) and the coupling constant (g) \
can be set through the options or by additional arguments: \
FieldStrength[mu,nu,a, A, g] or, specifying the dummy \
color indices: FieldStrength[mu,nu,a, {A,b,c}, g].";

IndexPosition::usage =
"IndexPosition is an option for FieldStrength.";

Begin["`Package`"]
End[]

Begin["`FieldStrength`Private`"]

DeclareNonCommutative[FieldStrength];

Options[FieldStrength] = {
	CouplingConstant -> SMP["g_s"],
	Explicit -> False,
	IndexPosition -> {0,0},
	Symbol -> "F",
	QuantumField -> GaugeField
};

FieldStrength[mu___, OPEDelta, nu___] :=
	FieldStrength[mu, Momentum[OPEDelta], nu];

FieldStrength[mu_, nu_, a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule, OptionsPattern[]] :=
	(QuantumField[FCPartialD[LorentzIndex[mu]],aA, LorentzIndex[nu], SUNIndex[a]] -
	QuantumField[FCPartialD[LorentzIndex[nu]],aA,    LorentzIndex[mu], SUNIndex[a]] +
	g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
	QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]]) /;
	FreeQ2[{mu,nu}, {Momentum, OPEDelta}] && OptionValue[Explicit];

FieldStrength[mu_, Momentum[OPEDelta], a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule, OptionsPattern[]] :=
	(QuantumField[FCPartialD[LorentzIndex[mu]], aA, Momentum[OPEDelta], SUNIndex[a]] -
	QuantumField[FCPartialD[Momentum[OPEDelta]],aA, LorentzIndex[mu], SUNIndex[a]] +
	g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
	QuantumField[aA, Momentum[OPEDelta], SUNIndex[c]]]) /;
	FreeQ2[{mu}, {Momentum, OPEDelta}] && OptionValue[Explicit];


FieldStrength[Momentum[OPEDelta], nu_, a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule, OptionsPattern[]] :=
	(QuantumField[FCPartialD[Momentum[OPEDelta]], aA, LorentzIndex[nu], SUNIndex[a]] -
	QuantumField[FCPartialD[LorentzIndex[nu]],aA, Momentum[OPEDelta], SUNIndex[a]] +
	g SUNF[a, b, c] DOT[QuantumField[aA, Momentum[OPEDelta], SUNIndex[b]],
	QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]]) /;
	FreeQ2[{nu}, {Momentum, OPEDelta}] && OptionValue[Explicit];


FieldStrength[mu_, nu_, OptionsPattern[]] :=
	(QuantumField[FCPartialD[mu], OptionValue[QuantumField],
	LorentzIndex[nu]] -    QuantumField[FCPartialD[nu], OptionValue[QuantumField],
	LorentzIndex[mu]]) /; OptionValue[Explicit];

FieldStrength[mu_, nu_, a_, opts:OptionsPattern[]] :=
	Block[ {g,b,c},
		b = Unique["b"];
		c = Unique["c"];
		FieldStrength[mu, nu, a, {OptionValue[QuantumField], b, c},
		OptionValue[CouplingConstant], opts]
	] /; OptionValue[Explicit];

MakeBoxes[FieldStrength[mu_, nu_, a:Except[_?OptionQ]..., opts:OptionsPattern[]], TraditionalForm] :=
	Catch[
		If[ OptionValue[FieldStrength,{opts},IndexPosition] === {0,0},
			Throw[SubsuperscriptBox[OptionValue[FieldStrength,{opts},Symbol], TBox[mu,nu],TBox[a]]]
		];
		If[ OptionValue[FieldStrength,{opts},IndexPosition] === {1,1},
			Throw[SubsuperscriptBox[OptionValue[FieldStrength,{opts},Symbol],"\[Null]", TBox[a,mu,nu]]]
		];
		If[ OptionValue[FieldStrength,{opts},IndexPosition] === {0,1},
			Throw[SubsuperscriptBox[OptionValue[FieldStrength,{opts},Symbol],    TBox[mu], TBox[a,nu]]]
		];
		If[ OptionValue[FieldStrength,{opts},IndexPosition] === {1,0},
			Throw[SubsuperscriptBox[OptionValue[FieldStrength,{opts},Symbol], TBox[nu], TBox[a,mu]]]
		];
	];

FCPrint[1,"FieldStrength.m loaded."];
End[]
