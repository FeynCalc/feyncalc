(* ::Package:: *)



(* :Title: PaVeLimitTo4                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts UV divergent parts of Passarino-Veltman
				coefficient functions 										*)

(* ------------------------------------------------------------------------ *)


PaVeLimitTo4::usage =
"PaVeLimitTo4[expr]  simplifies products of Passarino-Veltman functions and
$D$-dependent prefactors by evaluating the prefactors at $D=4$ and adding an
extra term from the product of $(D-4)$ and the UV pole of the
Passarino-Veltman function.

This is possible because the UV poles of arbitrary Passarino-Veltman functions
can be determined via PaVeUVPart. The result is valid up to 0th order in
Epsilon, i.e. it is sufficient for 1-loop calculations.

Warning! This simplification always ignores possible IR poles of
Passarino-Veltman functions. Therefore, it can be used only if all IR poles
are regulated without using dimensional regularization (e.g. by assigning
gluons or photons a fake mass) or if it is known in advance that the given
expression is free of IR singularities.

The application of PaVeLimitTo4 is equivalent to using the old OneLoop routine
with the flags $LimitTo4 and $LimitTo4IRUnsafe set to True.";

PaVeLimitTo4::failmsg =
"Error! PaVeLimitTo4 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PaVeLimitTo4`Private`"]

dimVar::usage="";
deriv::usage="";
polePref::usage="";

Options[PaVeLimitTo4] = {
	Collecting		-> True,
	Dimension 		-> D,
	FCE 			-> False,
	FCI				-> False,
	FCVerbose 		-> False,
	Factoring		-> {Factor2, 5000},
	TimeConstrained -> 3,
	Together 		-> True
};

PaVeLimitTo4[expr_,  OptionsPattern[]] :=
	Block[{	ex,repList,res, dummy, rest, pvl4Verbose,
			dim, time, loopInts,intsUnique,
			intsUniqueEval, paVeInt, repRule},

		dim	= OptionValue[Dimension];

		If [OptionValue[FCVerbose]===False,
			pvl4Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				pvl4Verbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PaVeLimitTo4: Entering.",  FCDoControl->pvl4Verbose];
		FCPrint[3, "PaVeLimitTo4: Entering with: ",  expr, FCDoControl->pvl4Verbose];

		If[ FreeQ2[expr,FeynCalc`Package`PaVeHeadsList],
			Return[expr]
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "PaVeLimitTo4: Applying FCLoopExtract.", FCDoControl->pvl4Verbose];
		{rest,loopInts,intsUnique} = FCLoopExtract[ex,{dummy},paVeInt,PaVe->True, PaVeIntegralHeads->Join[FeynCalc`Package`PaVeHeadsList, {dim}], FCI->True];
		FCPrint[3, "PaVeLimitTo4: List of the unique integrals: ",  intsUnique, FCDoControl->pvl4Verbose];

		intsUniqueEval = ToPaVe2/@((intsUnique/.paVeInt->Identity));
		FCPrint[3, "PaVeLimitTo4: List of the unique integrals after ToPaVe2: ",  intsUniqueEval, FCDoControl->pvl4Verbose];

		intsUniqueEval = FCProductSplit[#, {PaVe}] & /@ intsUniqueEval;
		FCPrint[3, "PaVeLimitTo4: List of the unique integrals after FCProductSplit: ",  intsUniqueEval, FCDoControl->pvl4Verbose];

		If[	!MatchQ[intsUniqueEval, {{_, _PaVe} ..}],
			Message[PaVeLimitTo4::failmsg,"The extracted loop integral is not a proper PaVe function."];
			Abort[]
		];

		(*
			If the prefactor or the PaVe-function depend on scalar products or similar quantitites,
			they must be converted to 4 dimensions.
		*)

		intsUniqueEval = Map[ChangeDimension[#, 4, FCI -> True] &, intsUniqueEval] /. dim-> dimVar;
		FCPrint[3, "PaVeLimitTo4: List of the unique integrals after ChangeDimension: ",  intsUniqueEval, FCDoControl->pvl4Verbose];

		If[	!MatchQ[Internal`RationalFunctionQ[#[[1]], dimVar] & /@ intsUniqueEval, {True ..}],
			Message[PaVeLimitTo4::failmsg,"The D-dependent prefactor must be a rational function of D."];
			Abort[]
		];

		If[	!MatchQ[FreeQ[#[[2]], dimVar] & /@ intsUniqueEval, {True ..}],
			Message[PaVeLimitTo4::failmsg,"The arguments of PaVe-functions may not depend on D."];
			Abort[]
		];

		If[	!Quiet[Internal`ExceptionFreeQ[intsUniqueEval /. dimVar -> 4]],
			Message[PaVeLimitTo4::failmsg, "Cannot handle PaVe functions multiplied by 1/Epsilon poles."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "PaVeLimitTo4: Taking the limit D->4", FCDoControl->pvl4Verbose];
		intsUniqueEval = limitTo4Fun /@ intsUniqueEval;
		FCPrint[1, "PaVeLimitTo4: Limit D->4 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvl4Verbose];
		FCPrint[3, "PaVeLimitTo4: Preliminary results for the unique integrals: ",  intsUniqueEval, FCDoControl->pvl4Verbose];

		If[	!FreeQ2[intsUniqueEval,{limitTo4Fun, dimVar}],
			Message[PaVeLimitTo4::failmsg, "Something went wrong when taking the limit D->4."];
			Abort[]
		];

		repRule = Thread[Rule[intsUnique,intsUniqueEval]];
		FCPrint[3, "PaVeLimitTo4: Replacement rule ",  repRule, FCDoControl->pvl4Verbose];

		res = rest + loopInts /. Dispatch[repRule];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "PaVeLimitTo4: Applying Collect2.", FCDoControl->pvl4Verbose];
			res = Collect2[res, FeynCalc`Package`PaVeHeadsList, Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained] ];
			FCPrint[1, "PaVeLimitTo4: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvl4Verbose];
			FCPrint[3, "PaVeLimitTo4: After Collect2: ", res, FCDoControl->pvl4Verbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "PaVeLimitTo4: Leaving.", FCDoControl->pvl4Verbose];
		FCPrint[3, "PaVeLimitTo4: Leaving with: ", res, FCDoControl->pvl4Verbose];

		res

	];


limitTo4Fun[{pref_, int_PaVe}] :=
	(pref /. dimVar -> 4) * int +
	((D[pref, dimVar] /. dimVar -> 4) *((dimVar - 4) PaVeUVPart[int,Dimension -> dimVar,
		FCI -> True, FCLoopExtract -> False]));

FCPrint[1,"PaVeLimitTo4.m loaded."];
End[]
