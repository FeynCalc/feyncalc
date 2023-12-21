(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToLightConeComponents												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: Converts selected Lorentz tensors into Cartesian tensors.		*)

(* ------------------------------------------------------------------------ *)


ToLightConeComponents::usage=
"ToLightConeComponents[expr, n, nb] rewrites all Dirac matrices, scalar
products, 4-vectors and metric tensors in terms of their component along the
lightcone directions n and nb

Using the option NotMomentum one can specify that quantities containing the
listed 4-momenta should be left untouched.";

ToLightConeComponents::fail=
"Error! ToLightConeComponents has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToLightConeComponents`Private`"]

vecN::usage="";
vecNB::usage="";
hold::usage="";
tlcVerbose::usage="";

Options[ToLightConeComponents] = {
	DiracGammaExpand	-> True,
	DotSimplify 		-> True,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose			-> False,
	FV 					-> True,
	GA 					-> True,
	GS 					-> True,
	MT					-> True,
	NotMomentum			-> {},
	PairContract		-> True,
	Polarization		-> True,
	SP					-> True
};

ToLightConeComponents[expr_, opts:OptionsPattern[]]:=
	ToLightConeComponents[expr, $FCDefaultLightconeVectorN,
		$FCDefaultLightconeVectorNB, opts];


ToLightConeComponents[expr_, n_, nb_, OptionsPattern[]]:=
	Block[{ex, heads, tmp, res, uniqList,null1,null2, uniqListEval,
		repRule, optNotMomentum, selector, pattern, time, holdDOT},

		heads = {};
		optNotMomentum = OptionValue[NotMomentum];

		If [OptionValue[FCVerbose]===False,
			tlcVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				tlcVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[SP] || OptionValue[FV],
			heads = Join[heads,{Pair}]
		];

		If[	OptionValue[GA] || OptionValue[GS],
			heads = Join[heads,{DiracGamma}]
		];

		vecN = n;
		vecNB = nb;


		FCPrint[1, "ToLightConeComponents: Entering.", FCDoControl->tlcVerbose];
		FCPrint[3, "ToLightConeComponents: Entering with ", expr, FCDoControl->tlcVerbose];


		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		uniqList = Cases[ex+null1+null2,Alternatives@@(Blank/@heads),Infinity]//DeleteDuplicates//Sort;

		FCPrint[2, "ToLightConeComponents: Raw list of unique expressions ", uniqList, FCDoControl->tlcVerbose];

		If[	optNotMomentum=!={},
			selector = Join[(Momentum[#,pattern]&/@optNotMomentum),(Momentum[Polarization[#,pattern],pattern]&/@optNotMomentum)] /. pattern->BlankNullSequence[];
			uniqList = SelectFree[uniqList,selector]
		];

		uniqListEval = uniqList;

		FCPrint[2, "ToLightConeComponents: Final list of unique expressions ", uniqListEval, FCDoControl->tlcVerbose];


		If[	OptionValue[ExpandScalarProduct],
			uniqListEval = ExpandScalarProduct[#,FCI->True]&/@uniqListEval;
		];

		If[	OptionValue[DiracGammaExpand],
			uniqListEval = DiracGammaExpand[#,FCI->True]&/@uniqListEval
		];

		If[	OptionValue[SP],
			uniqListEval = uniqListEval /. Pair -> spToLC /. spToLC -> Pair
		];

		If[	OptionValue[FV],
			uniqListEval = uniqListEval /. Pair -> fvToLC /. fvToLC -> Pair
		];

		If[	OptionValue[MT],
			uniqListEval = uniqListEval /. Pair -> mtToLC /. mtToLC -> Pair
		];

		If[	OptionValue[GA],
			uniqListEval = uniqListEval /. DiracGamma -> gaToLC /. gaToLC -> DiracGamma
		];

		If[	OptionValue[GS],
			uniqListEval = uniqListEval /. DiracGamma -> gsToLC /. gsToLC -> DiracGamma
		];

		FCPrint[3, "ToLightConeComponents: List of expanded expressions ", uniqListEval, FCDoControl->tlcVerbose];

		repRule = Thread[Rule[uniqList, uniqListEval]];

		res = ex /. Dispatch[repRule] /. hold->Identity;

		If[	OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "ToLightConeComponents: Applying DotSimplify.", FCDoControl->tlcVerbose];
			res = DotSimplify[res,FCI->True];
			FCPrint[1,"ToLightConeComponents: Done applying DotSimplify, timing; ", N[AbsoluteTime[] - time, 4], FCDoControl->tlcVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "ToLightConeComponents: Applying additional Dirac algebra-related simplifications.", FCDoControl->tlcVerbose];
			res = res/. DOT->holdDOT /. Dispatch[ {
				holdDOT[___, DiracGamma[Momentum[nb,dim___],dim___],	DiracGamma[Momentum[nb,dim___],dim___], ___] -> 0,
				holdDOT[___, DiracGamma[Momentum[n,dim___],dim___], DiracGamma[Momentum[n,dim___],dim___], ___] -> 0,
				Pair[Momentum[n,___], _LorentzIndex]^2 -> 0,
				Pair[Momentum[nb,___], _LorentzIndex]^2 -> 0,
				holdDOT[___,0,___] -> 0
			}];
			FCPrint[1,"ToLightConeComponents: Done applying additional simplifications, timing; ", N[AbsoluteTime[] - time, 4], FCDoControl->tlcVerbose];


			If[	OptionValue[PairContract],
				time=AbsoluteTime[];
				FCPrint[1, "ToLightConeComponents: Applying PairContract.", FCDoControl->tlcVerbose];
				res = res /. Pair->PairContract /. PairContract->Pair;
				FCPrint[1, "ToLightConeComponents: Done applying PairContract, timing; ", N[AbsoluteTime[] - time, 4], FCDoControl->tlcVerbose];
			];

			res = res /. holdDOT[___,0,___] -> 0 /. holdDOT -> DOT;


		];




		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

mtToLC[LorentzIndex[l1_,dim_:4], LorentzIndex[l2_,dim_:4]]:=
	(
	1/2 hold[Pair][LorentzIndex[l1,dim],Momentum[vecN,dim]] hold[Pair][LorentzIndex[l2,dim],Momentum[vecNB,dim]] +
	1/2 hold[Pair][LorentzIndex[l2,dim],Momentum[vecN,dim]] hold[Pair][LorentzIndex[l1,dim],Momentum[vecNB,dim]] +
	Pair[LightConePerpendicularComponent[LorentzIndex[l1,dim],Momentum[vecN,dim],Momentum[vecNB,dim]],
		LightConePerpendicularComponent[LorentzIndex[l2,dim],Momentum[vecN,dim],Momentum[vecNB,dim]]]
	);

spToLC[Momentum[a_,dim_:4], Momentum[b_,dim_:4]]:=
	(
	1/2 hold[Pair][Momentum[a,dim],Momentum[vecN,dim]] hold[Pair][Momentum[b,dim],Momentum[vecNB,dim]] +
	1/2 hold[Pair][Momentum[a,dim],Momentum[vecNB,dim]] hold[Pair][Momentum[b,dim],Momentum[vecN,dim]] +
	Pair[LightConePerpendicularComponent[Momentum[a,dim],Momentum[vecN,dim],Momentum[vecNB,dim]],
		LightConePerpendicularComponent[Momentum[b,dim],Momentum[vecN,dim],Momentum[vecNB,dim]]]
	);

fvToLC[l_LorentzIndex, m_Momentum]:=
	fvToLC[m,l];

fvToLC[Momentum[a_,dim_:4], LorentzIndex[l_,dim_:4]]:=
	(
	1/2 hold[Pair][Momentum[a,dim],Momentum[vecN,dim]] hold[Pair][LorentzIndex[l,dim],Momentum[vecNB,dim]] +
	1/2 hold[Pair][Momentum[a,dim],Momentum[vecNB,dim]] hold[Pair][LorentzIndex[l,dim],Momentum[vecN,dim]] +
	Pair[LightConePerpendicularComponent[Momentum[a,dim],Momentum[vecN,dim],Momentum[vecNB,dim]],
		LightConePerpendicularComponent[LorentzIndex[l,dim],Momentum[vecN,dim],Momentum[vecNB,dim]]]
	);

gaToLC[LorentzIndex[l_, dim_:4], dim_:4]:=
	(
	1/2 hold[DiracGamma][Momentum[vecN,  dim], dim] hold[Pair][LorentzIndex[l,dim],Momentum[vecNB,dim]] +
	1/2 hold[DiracGamma][Momentum[vecNB, dim], dim] hold[Pair][LorentzIndex[l,dim],Momentum[vecN,dim]] +
	DiracGamma[LightConePerpendicularComponent[LorentzIndex[l,dim],Momentum[vecN,dim],Momentum[vecNB,dim]],dim]
	);

gsToLC[Momentum[l_, dim_:4], dim_:4]:=
	(
	1/2 hold[DiracGamma][Momentum[vecN,  dim], dim] hold[Pair][Momentum[l,dim],Momentum[vecNB,dim]] +
	1/2 hold[DiracGamma][Momentum[vecNB, dim], dim] hold[Pair][Momentum[l,dim],Momentum[vecN,dim]] +
	DiracGamma[LightConePerpendicularComponent[Momentum[l,dim],Momentum[vecN,dim],Momentum[vecNB,dim]],dim]
	);

FCPrint[1,"ToLightConeComponents.m loaded."];
End[]
