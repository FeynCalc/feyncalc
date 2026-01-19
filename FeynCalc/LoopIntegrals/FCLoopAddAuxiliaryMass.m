(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopAddAuxiliaryMass							*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Adds auxiliary mass to massless propagators				*)

(* ------------------------------------------------------------------------ *)

FCLoopAddAuxiliaryMass::usage =
"FCLoopAddAuxiliaryMass[expr, {k1, k2, ...},-m^2,n] adds auxiliary mass term
$m^2$ to the propagators in the list that depend on loop momenta k1, k2, ....
For $n=0$ the mass is added directly.

For $n>0$ the function applies the exact identity from
[arXiv:hep-ph/9711266](https://arxiv.org/abs/hep-ph/9711266), known as 
infrared rearrangement, $n$ times. The option Last allows to add a flag to the
last term in the expression as a check that it does not contribute to the
physical results.";

FCLoopAddAuxiliaryMass::failmsg =
"FCLoopAddAuxiliaryMass has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopAddAuxiliaryMass`Private`"]

Options[FCLoopAddAuxiliaryMass] = {
	Collecting 					-> True,
	Dimension					-> D,
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor, 5000},
	FeynAmpDenominatorCombine 	-> True,
	FeynAmpDenominatorSplit		-> True,
	FinalSubstitutions			-> {},
	MomentumExpand				-> True,
	Head						-> Identity,
	ToSFAD						-> True,
	Last						-> 1,
	"MassHead"					-> Identity
};


FCLoopAddAuxiliaryMass[expr_List, lmoms_List, m2_,n_Integer?NonNegative, OptionsPattern[]]:=
	Block[{	exp, dens, res, densEval, flag, optTimes, optMomentumExpand,
			optHead, optFinalSubstitutions, optVerbose,optMassHead,
			masslessFAD, optDimension, optFeynAmpDenominatorCombine,
			optLast,rest},

		optFinalSubstitutions 			= OptionValue[FinalSubstitutions];
		optDimension		  			= OptionValue[Dimension];
		optFeynAmpDenominatorCombine	= OptionValue[FeynAmpDenominatorCombine];
		optHead							= OptionValue[Head];
		optMomentumExpand				= OptionValue[MomentumExpand];
		optMassHead						= OptionValue["MassHead"];

		optLast = OptionValue[Last];


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

		FCPrint[1, "FCLoopAddAuxiliaryMass: Entering.", FCDoControl->optVerbose];
		FCPrint[3, "FCLoopAddAuxiliaryMass: Entering with: ", exp, FCDoControl->optVerbose];
		FCPrint[3, "FCLoopAddAuxiliaryMass: Loop momenta are ", lmoms, FCDoControl->optVerbose];

		If[	!MatchQ[exp/.optHead->Identity,{FeynAmpDenominator[_]..}],
			Message[FCLoopAddAuxiliaryMass::failmsg,"The input expression must be a list of single propagator denominators."];
			Abort[]
		];

		(*Select only massless ones*)
		dens = Join[
			Cases[exp,f:FeynAmpDenominator[GenericPropagatorDenominator[moms_, __]]/; PossibleZeroQ[moms/. Pair[_]->0]:>f,Infinity],
			Cases[exp,f:FeynAmpDenominator[StandardPropagatorDenominator[_, _, 0, _]]:>f,Infinity],
			Cases[exp,f:FeynAmpDenominator[PropagatorDenominator[_, 0]]:>f,Infinity]
		];

		dens = SelectNotFree[dens,lmoms];

		rest = Complement[exp/.optHead->Identity,dens];


		densEval = dens;

		If[	optMomentumExpand,
			densEval = MomentumExpand[densEval]
		];

		If[	OptionValue[ToSFAD],
			densEval = ToSFAD[densEval,FCI->True]
		];

		FCPrint[3, "FCLoopAddAuxiliaryMass: Relevant propagators: ", densEval, FCDoControl->optVerbose];

		densEval = densEval /. FeynAmpDenominator -> masslessFAD;

		If[	n>0,
			densEval = Map[Function[{x},Nest[denRewrite[#,masslessFAD,lmoms,m2,optDimension,flag,optMassHead]&,
				x,n]], densEval]/.masslessFAD->FeynAmpDenominator,

			densEval = densEval /.masslessFAD[StandardPropagatorDenominator[moms_,0,0,r_]]->FeynAmpDenominator[StandardPropagatorDenominator[moms,0,m2,r]]
		];

		FCPrint[3, "FCLoopAddAuxiliaryMass: After evaluation: ", densEval, FCDoControl->optVerbose];

		If[	!FreeQ[densEval,denRewrite],
			Message[FCLoopAddAuxiliaryMass::failmsg,"Something went wrong when adding auxiliary mass."]
		];

		If[	optFeynAmpDenominatorCombine,
			densEval= FeynAmpDenominatorCombine[densEval,FCI->True]
		];



		res = Join[Thread[Rule[optHead/@dens,densEval]],Thread[Rule[optHead/@rest,rest]]] /. flag -> optLast /. Dispatch[optFinalSubstitutions];

		(*TODO Check*)

		FCPrint[1, "FCLoopAddAuxiliaryMass: Leaving.", FCDoControl->optVerbose];

		FCPrint[3, "FCLoopAddAuxiliaryMass: Leaving with ", res, FCDoControl->optVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

denRewrite[a_+b_,args__]:=
	denRewrite[a,args]+denRewrite[b,args];

denRewrite[x_, masslessFAD_,__]:=
	x/;FreeQ[x,masslessFAD];

(*Todo GFAD missing*)
denRewrite[coeffRaw_. masslessFAD[StandardPropagatorDenominator[moms_,0,0,rest_]],masslessFAD_,lmoms_List,m2_,dim_,flag_,head_]:=
	Block[{	loopmoms, extmoms, coeff},
		extmoms=SelectFree2[moms,lmoms];
		loopmoms = Expand2[moms-extmoms,lmoms];
		coeff = coeffRaw /. flag->1;

		If[!PossibleZeroQ[moms-extmoms-lmoms],
			Message[FCLoopAddAuxiliaryMass::failmsg,"Something went wrong when separating external and loop momenta"];
			Abort[]
		];

		Collect2[coeff*(FeynAmpDenominator[StandardPropagatorDenominator[loopmoms,0,m2,rest]] +
			(- Pair[Momentum[extmoms,dim],Momentum[extmoms,dim]]-2 Pair[Momentum[loopmoms,dim],Momentum[extmoms,dim]]+head[m2])*
			FeynAmpDenominator[StandardPropagatorDenominator[loopmoms,0,m2,rest]]*
			masslessFAD[StandardPropagatorDenominator[moms,0,0,rest]]),masslessFAD]/. x_masslessFAD-> flag x


	]/; FreeQ[c,masslessFAD];

FCPrint[1,"FCLoopAddAuxiliaryMass.m loaded."];
End[]
