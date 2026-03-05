(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Covariant derivative *)

(* ------------------------------------------------------------------------ *)

CovariantD::usage =
"CovariantD[mu] is a generic covariant derivative with Lorentz index $\\mu$.

CovariantD[x, mu] is a generic covariant derivative with respect to $x^{\\mu
}$.

CovariantD[mu, a, b] is a covariant derivative for a bosonic field that acts
on QuantumField[f, {}, {a, b}], where f is some field name and a and b are two
$SU(N)$ indices in the adjoint representation.

CovariantD[OPEDelta, a, b] is a short form for CovariantD[mu, a, b]
FV[OPEDelta, mu].

CovariantD[{OPEDelta, a, b}, {n}] yields the product of n operators, where n
is an integer.

CovariantD[OPEDelta, a, b, {m, n}] gives the expanded form of
CovariantD[OPEDelta, a, b]^m up to order $g^n$ for the gluon, where $n$ is an
integer and $g$ the coupling constant indicated by the setting of the option
CouplingConstant.

CovariantD[OPEDelta, {m, n}] gives the expanded form of CovariantD[OPEDelta]^m
up to order $g^n$ of the fermionic field. To obtain the explicit expression
for a particular covariant derivative, the option Explicit must be set to
True.";

DummyIndex::usage =
"DummyIndex is an option of CovariantD specifying an index to use as dummy
summation index. If set to Automatic, unique indices are generated.";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`CovariantD`Private`"]

DeclareNonCommutative[CovariantD];

Options[CovariantD] = { CouplingConstant -> SMP["g_s"],
						DummyIndex -> Automatic,
						Explicit -> False,
						FCPartialD -> RightPartialD,
						QuantumField -> GaugeField
						};

isunt[a_] :=
	FeynCalcInternal[I SUNT[a]];
subsit[un_][in_] :=
	If[ $Notebooks,
		Subscript[un,in],
		un[in]
	];

Unique2[x_] :=
	If[ $Notebooks === True,
		subsit[Unique[x]],
		Unique[x]
	];

$dummycount = 1;

CovariantD[al_, ru___Rule ] :=
	Block[ {aA, g, cC, du, partial},
		partial = FCPartialD /. {ru} /. Options[CovariantD];
		aA = QuantumField     /. {ru}  /. Options[CovariantD];
		g = CouplingConstant /. {ru}  /. Options[CovariantD];
		du = DummyIndex /. {ru}  /. Options[CovariantD];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		(
			partial[al] - g I (DOT[SUNT[SUNIndex[cC]] ,
		If[ (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]]                 )
		)] /;(Explicit /. {ru} /. Options[CovariantD]);

CovariantD[al_, a_, b_, ru___Rule ] :=
	Block[ {aA, cC, du, partial},
		aA = QuantumField     /. {ru}  /. Options[CovariantD];
		g = CouplingConstant /. {ru}  /. Options[CovariantD];
		partial = FCPartialD /. {ru} /. Options[CovariantD];
		du = DummyIndex /. {ru}  /. Options[CovariantD];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		SUNDelta[a, b] partial[al] -
		g SUNF[a,b,cC] *
		If[ (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]
	] /;
	(Explicit /. {ru} /. Options[CovariantD]) && Head[a]=!=SUNFIndex;



CovariantD[al_, a_SUNFIndex, b_SUNFIndex, OptionsPattern[]] :=
	Block[ {aA, cC, du, partial,g},
		aA = OptionValue[QuantumField];
		g = OptionValue[CouplingConstant];
		partial = OptionValue[FCPartialD];
		du = OptionValue[DummyIndex];
		If[ du === Automatic,
			cC = Unique["c"],
			cC = du
		];
		SUNFDelta[a, b] partial[al] -
		I g SUNTF[cC,a,b] *
		If[ (Head[al]=== Momentum),
			QuantumField[aA, Momentum[al], SUNIndex[cC]],
			QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
		]
	] /; OptionValue[Explicit];

CovariantD /:
	MakeBoxes[CovariantD[mud_], TraditionalForm] :=
		RowBox[{SubscriptBox["D",TBox[mud]]}];

CovariantD /:
	MakeBoxes[CovariantD[mud_, a_, b_], TraditionalForm] :=
		SubsuperscriptBox["D", TBox[mud], TBox[a, b]]/; Head[mud] =!= List;

CovariantD /:
	MakeBoxes[CovariantD[x_, LorentzIndex[mu__]], TraditionalForm] :=
		RowBox[{"\[PartialD]", "/", "D", SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]}];

FCPrint[1,"CovariantD.m loaded."];
End[]
