(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSingularityStructure										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Provides information on the UV/IR poles						*)

(* ------------------------------------------------------------------------ *)

FCLoopSingularityStructure::usage =
"FCLoopSingularityStructure[int, {q1, q2, ...}] returns a list of expressions
{pref,U,F,gbF} that are useful to analyze the singular behavior of the loop
integral int.

- pref is the $\\varepsilon$-dependent prefactor of the Feynman parameter
integral that can reveal an overall UV-singularity
- U and F denote the first and second Symanzik polynomials respectively
- gbF is the Groebner basis of ${F, \\partial F / \\partial x_i}$ with respect
to the Feynman parameters

The idea to search for solutions of Landau equations for the $F$-polynomial
using Groebner bases was adopted from
[1810.06270](https://arxiv.org/abs/1810.06270) and
[2003.02451](https://arxiv.org/abs/2003.02451) by B. Ananthanarayan, Abhishek
Pal, S. Ramanan Ratan Sarkar and Abhijit B. Das.";

FCLoopSingularityStructure::failmsg =
"Error! FCLoopSingularityStructure has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopSingularityStructure`Private`"]

fclsVerbose::usage = "";

Options[FCLoopSingularityStructure] = {
	FCE					-> False,
	FCI					-> False,
	FCVerbose 			-> False,
	FCReplaceD 			-> {D->4-2 Epsilon},
	FinalSubstitutions	-> {},
	Names				-> FCGV["x"]
};


FCLoopSingularityStructure[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopSingularityStructure[expr, {FCGV["dummy"]}, opts];

FCLoopSingularityStructure[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopSingularityStructure[expr, {FCGV["dummy"]}, opts];

FCLoopSingularityStructure[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	fPolys, pows, mat, vars, optFCReplaceD,
			tensorRank, res, time, lmoms, optFinalSubstitutions,
			ex, notList = False, tmp, optNames, landauEqs},

		If[	OptionValue[FCVerbose] === False,
			fclsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclsVerbose = OptionValue[FCVerbose]];
		];

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optNames 				= OptionValue[Names];
		optFCReplaceD			= OptionValue[FCReplaceD];

		FCPrint[1, "FCLoopSingularityStructure: Entering.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopSingularityStructure: Entering with: ", expr, FCDoControl -> fclsVerbose];

		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopSingularityStructure: Calling FCFeynmanPrepare.", FCDoControl -> fclsVerbose];

		If[	lmomsRaw==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, _GLI | _FCTopology],
				notList = True;
				tmp =	FCFeynmanParametrize[ex, lmoms, FCI -> True, Names -> optNames,
					FinalSubstitutions-> optFinalSubstitutions, FCFeynmanPrepare->True, FCReplaceD->optFCReplaceD];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {__GLI} | {__FCTopology}],
				tmp = FCFeynmanParametrize[ex, lmoms, FCI -> True, Names -> optNames,
					FinalSubstitutions-> optFinalSubstitutions, FCFeynmanPrepare->True, FCReplaceD->optFCReplaceD],
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				tmp = FCFeynmanParametrize[#, lmoms, FCI -> True, Names -> optNames,
					FinalSubstitutions-> optFinalSubstitutions, FCFeynmanPrepare->True, FCReplaceD->optFCReplaceD]/@ex,
			True,
				Message[FCLoopToPakForm::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopSingularityStructure: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];

		tmp = Map[{(*pref*) #[[2]], (*U*) #[[4]][[1]], (*F*) #[[4]][[2]], (*FPs*) Cases2[#[[4]][[2]], optNames]} &, tmp];
		res = Map[{#[[1]],#[[2]],#[[3]],getLandauEqs[#[[3]],#[[4]]]}&, tmp];

		FCPrint[3, "FCLoopSingularityStructure: Leaving.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopSingularityStructure: Leaving with: ", res, FCDoControl -> fclsVerbose];

		If[	notList,
			res = First[res]
		];

		If[OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

getLandauEqs[fPoly_,vars_List]:=
	GroebnerBasis[Join[{fPoly},D[fPoly,{vars}]],vars]

FCPrint[1,"FCLoopSingularityStructure.m loaded."];
End[]
