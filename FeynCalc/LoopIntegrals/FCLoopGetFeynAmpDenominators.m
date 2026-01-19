(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGetFeynAmpDenominators							*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Adds auxiliary mass to massless propagators				*)

(* ------------------------------------------------------------------------ *)

FCLoopGetFeynAmpDenominators::usage =
"FCLoopGetFeynAmpDenominators[expr, {k1,k2,...}, head] extracts all single
propagator denominators present in the expression that depend on the loop
momenta k1, k2, ....

The function returns a list of two elements.  The first one contains the
original expression with selected denominators wrapped with head. The second
one is the list of relevant denominators

Setting the option \"Massless\" to Truewill select only massless denominators.

The option Momentum specifies the dependency on external momenta. When set to
a list of momenta, relevant propagators will be selected irrespective of being
massless or massive.";

FCLoopGetFeynAmpDenominators::failmsg =
"FCLoopGetFeynAmpDenominators has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopGetFeynAmpDenominators`Private`"]

Options[FCLoopGetFeynAmpDenominators] = {
	Collecting 					-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor, 5000},
	FeynAmpDenominatorSplit		-> True,
	FinalSubstitutions			-> {},
	MomentumExpand				-> True,
	ToSFAD						-> True,
	Momentum					-> All,
	"Massless"					-> False
};


FCLoopGetFeynAmpDenominators[expr_, lmoms_List, propHead_Symbol, OptionsPattern[]]:=
	Block[{	exp, dens, optFinalSubstitutions, optVerbose,
			optMomentum, optMassless, densMassless={}, densExtMoms={}},

		optFinalSubstitutions 			= OptionValue[FinalSubstitutions];

		optMomentum						= OptionValue[Momentum];
		optMassless						= OptionValue["Massless"];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			exp = expr,
			{exp,optFinalSubstitutions} = FCI[{expr,optFinalSubstitutions}]
		];

		FCPrint[1, "FCLoopGetFeynAmpDenominators: Entering.", FCDoControl->optVerbose];
		FCPrint[3, "FCLoopGetFeynAmpDenominators: Entering with: ", exp, FCDoControl->optVerbose];
		FCPrint[3, "FCLoopGetFeynAmpDenominators: Loop momenta are ", lmoms, FCDoControl->optVerbose];

		If[ OptionValue[ToSFAD],
			exp = ToSFAD[exp];
			FCPrint[3, "FCLoopGetFeynAmpDenominators: After ToSFAD: ", exp, FCDoControl->optVerbose]
		];


		If[	OptionValue[FeynAmpDenominatorSplit],
			exp = FeynAmpDenominatorSplit[exp, FCI->True];
			FCPrint[3, "FCLoopGetFeynAmpDenominators: After FeynAmpDenominatorSplit: ", exp, FCDoControl->optVerbose]
		];

		dens = Cases2[exp, FeynAmpDenominator];

		dens = SelectNotFree[dens,lmoms];


		If[	optMassless,
			(*Select only massless ones*)
			densMassless = Join[
				Cases[dens,f:FeynAmpDenominator[GenericPropagatorDenominator[moms_, __]]/; PossibleZeroQ[moms/. Pair[_]->0]:>f,Infinity],
				Cases[dens,f:FeynAmpDenominator[StandardPropagatorDenominator[_, _, 0, _]]:>f,Infinity]
			],
			densMassless = {}
		];

		If[ optMomentum=!=All && Head[optMomentum]===List,
			FCPrint[1, "FCLoopGetFeynAmpDenominators: Only keeping denominators that depend on ", optMomentum, FCDoControl->optVerbose];
			densExtMoms = SelectNotFree[dens,optMomentum],
			densExtMoms = {}
		];

		If[	optMassless || optMomentum=!=All,
			dens = Union[densMassless,densExtMoms]
		];

		exp = exp /. Dispatch[Thread[Rule[dens,propHead/@dens]]];

		dens = propHead/@dens;

		If[	OptionValue[FCE],
			{exp,dens} = {FCE[exp],FCE[dens]}
		];

		{exp,dens}
	];

FCPrint[1,"FCLoopGetFeynAmpDenominators.m loaded."];
End[]
