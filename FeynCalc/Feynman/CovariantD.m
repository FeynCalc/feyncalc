(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Covariant derivative											*)

(* ------------------------------------------------------------------------ *)

CovariantD::usage =
"CovariantD[mu] is a generic covariant derivative with Lorentz index $\\mu$.

CovariantD[x, mu] is a generic covariant derivative with respect to $x^{\\mu
}$.

CovariantD[mu, a, b] is a covariant derivative for a bosonic field that acts
on QuantumField[f, {}, {a, b}], where f is some field name and a and b are two
$SU(N)$ indices in the adjoint representation.";



(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`CovariantD`Private`"]

DeclareNonCommutative[CovariantD];

Options[CovariantD] = {
	CouplingConstant	-> SMP["g_s"],
	Explicit			-> False,
	FCPartialD 			-> RightPartialD,
	QuantumField 		-> GaugeField
};

CovariantD /:
	MakeBoxes[CovariantD[mud_], TraditionalForm] :=
		RowBox[{SubscriptBox["D",TBox[mud]]}];

CovariantD /:
	MakeBoxes[CovariantD[mud_, a_, b_], TraditionalForm] :=
		SubsuperscriptBox["D", TBox[mud], TBox[a, b]]/; Head[mud] =!= List;

CovariantD /:
	MakeBoxes[CovariantD[x_, LorentzIndex[mu__]], TraditionalForm] :=
		RowBox[{"\[PartialD]", "/", "D", SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]}];


CovariantD[al_, OptionsPattern[]] :=
	Block[ {aA, g, cC, res, partial,qf},

		aA 		= OptionValue[QuantumField];
		g 		= OptionValue[CouplingConstant];
		partial = OptionValue[FCPartialD];


		cC = Unique["c"];

		If[ TrueQ[Head[al]=== Momentum],
			qf = QuantumField[aA, Momentum[al], SUNIndex[cC]],
			qf = QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		];

		partial[al] - g I DOT[SUNT[SUNIndex[cC]], qf]
		]/; OptionValue[Explicit];

CovariantD[al_, a_, b_/;!OptionQ[{b}], OptionsPattern[]] :=
	Block[{aA, g, cC, partial, qf},

		aA 		= OptionValue[QuantumField];
		g 		= OptionValue[CouplingConstant];
		partial = OptionValue[FCPartialD];

		cC = Unique["c"];

		If[ TrueQ[Head[al]=== Momentum],
			qf = QuantumField[aA, Momentum[al], SUNIndex[cC]],
			qf = QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		];

		SUNDelta[a, b] partial[al] - g SUNF[a,b,cC]  qf
	]/; OptionValue[Explicit] && Head[a]=!=SUNFIndex;



CovariantD[al_, a_SUNFIndex, b_SUNFIndex, OptionsPattern[]] :=
	Block[ {aA, cC, du, partial, g, qf},

		aA 		= OptionValue[QuantumField];
		g 		= OptionValue[CouplingConstant];
		partial = OptionValue[FCPartialD];

		cC = Unique["c"];

		If[ TrueQ[Head[al]=== Momentum],
			qf = QuantumField[aA, Momentum[al], SUNIndex[cC]],
			qf = QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		];

		SUNFDelta[a, b] partial[al] - I g SUNTF[cC,a,b] qf
	] /; OptionValue[Explicit];


FCPrint[1,"CovariantD.m loaded."];
End[]
