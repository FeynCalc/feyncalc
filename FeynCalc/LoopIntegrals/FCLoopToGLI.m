(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopToGLI														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Converts FADs to {GLI,FCTopology}							*)

(* ------------------------------------------------------------------------ *)

FCLoopToGLI::usage=
"FCLoopToGLI[int, lmoms] converts the integral int depending on the loop
momenta lmoms to the GLI-notation. The function returns a GLI-integral and a
minimal FCTopology containing only the propagators from the original integral.";

FCLoopToGLI::failmsg =
"Error! FCLoopToGLI encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopToGLI`Private`"]


Options[FCLoopToGLI] = {
	FCE 				-> False,
	FCI 				-> False,
	FCParallelize		-> False,
	FCVerbose 			-> False,
	FinalSubstitutions	-> {},
	Names				-> "loopint",
	Unique				-> True
}

FCLoopToGLI[expr_List, lmomsRaw_List/; FreeQ[lmomsRaw, OptionQ], opts:OptionsPattern[]]:=
	Block[{	res, optVerbose, lmoms, aux, time, optNames, suffix="", names, list,
			optFinalSubstitutions},


		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		optNames = OptionValue[Names];
		optFinalSubstitutions = OptionValue[FinalSubstitutions];

		If[	TrueQ[FreeQ[list@@lmomsRaw,List]],
			lmoms  =ConstantArray[lmomsRaw,Length[expr]],
			lmoms = lmomsRaw
		];

		If[	TrueQ[FreeQ[list@@optFinalSubstitutions,List]],
			optFinalSubstitutions  =ConstantArray[optFinalSubstitutions,Length[expr]],
			If[	Length[optFinalSubstitutions]=!=Length[expr],
				Message[FCLoopToGLI::failmsg,"The number of elements in the FinalSubstitutions option must match the number of expressions to process."];
				Abort[]
			];
		];


		If[	OptionValue[Unique],
			suffix = ToString[Unique[]]<>"$"
		];


		Switch[
			optNames,
			_String,
				names=Table[optNames<>suffix<>ToString[i],{i,1,Length[expr]}],
			_Symbol,
				names=Table[ToExpression[optNames<>suffix<>ToString[i]],{i,1,Length[expr]}],
			_Function,
				names=Table[optNames[suffix<>ToString[i]],{i,1,Length[expr]}],
			_,
			Message[FCLoopToGLI::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
			FCPrint[1,"FCLoopToGLI: Applying FCLoopToGLI in parallel." , FCDoControl->optVerbose];
			aux = Transpose[{expr,lmoms,names,optFinalSubstitutions}];
			res = Transpose@ParallelMap[FCLoopToGLI[#[[1]],#[[2]],Unique->False,Names->#[[3]],
				FinalSubstitutions->#[[4]],
				FilterRules[{opts},  Except[FCParallelize|FCVerbose|Unique|Names|FinalSubstitutions]]]&,aux, DistributedContexts -> None,
				Method->"ItemsPerEvaluation" -> Ceiling[N[Length[aux]/$KernelCount]/10]];
			FCPrint[1, "FCLoopToGLI: FCLoopToGLI done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			,

			FCPrint[1,"FCLoopToGLI: Applying FCLoopToGLI." , FCDoControl->optVerbose];
			res = Transpose@MapThread[FCLoopToGLI[#1, #2, Unique->False,Names->#3, FinalSubstitutions->#4,
				FilterRules[{opts},  Except[FCParallelize|FCVerbose|Unique|Names|FinalSubstitutions]]]&,
				{expr,lmoms,names,optFinalSubstitutions}];
			FCPrint[1, "FCLoopToGLI: FCLoopToGLI done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];
		res
	];



FCLoopToGLI[expr_/;Head[expr]=!=List, lmoms_List/; FreeQ[lmoms, OptionQ], OptionsPattern[]]:=
	Block[{	exp, tmp, res, rest, optVerbose, time, props, powers, optFinalSubstitutions,
			momenta, emoms, optNames, topoID, topo, gli, optUnique, suffix = "", lmomsRel},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optNames				= OptionValue[Names];
		optUnique				= OptionValue[Unique];

		If[	Length[lmoms]<1,
			Message[FCLoopToGLI::failmsg,"The list of the loop momenta cannot be empty."];
			Abort[]
		];

		FCPrint[1,"FCLoopToGLI: Entering.", FCDoControl->optVerbose];
		FCPrint[3,"FCLoopToGLI: Entering with ", exp, FCDoControl->optVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopToGLI: Applying FCLoopIntegralToPropagators.", FCDoControl->optVerbose];

		{tmp, rest} = FCLoopIntegralToPropagators[expr, lmoms, Tally -> True, FCI->OptionValue[FCI],Rest->True];

		{props,powers} = Transpose[tmp];

		FCPrint[1, "FCLoopToGLI: FCLoopIntegralToPropagators done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		FCPrint[3, "FCLoopToGLI: After FCLoopIntegralToPropagators: ", tmp, FCDoControl->optVerbose];

		momenta = Union[Cases[MomentumExpand[props],Momentum[m_,___]:>m,Infinity]];
		lmomsRel = SelectNotFree[lmoms,momenta];
		emoms = SelectFree[momenta,lmomsRel];

		If[	optUnique,
			suffix = ToString[Unique[]]
		];

		Switch[
			optNames,
			_String,
				topoID=optNames<>suffix,
			_Symbol,
				topoID=ToExpression[optNames<>suffix],
			_Function,
				topoID=optNames[suffix],
			_,
			Message[FCLoopToGLI::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		topo = FCTopology[topoID,props,lmomsRel,emoms,optFinalSubstitutions,{}];
		gli = rest GLI[topoID,powers];

		res = {gli,topo};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopToGLI: Leaving.", FCDoControl->optVerbose];
		FCPrint[3,"FCLoopToGLI: Leaving with ", res, FCDoControl->optVerbose];

		res
	];

FCPrint[1,"FCLoopToGLI.m loaded."];
End[]
