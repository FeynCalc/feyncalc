(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisOverdeterminedQ										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Checks if the propagator basis is overdetermined			*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisOverdeterminedQ::usage =
"FCLoopBasisOverdeterminedQ[int, {q1, q2, ...}] checks whether the loop
integral or topology int contains linearly dependent propagators.

The input can also consist of an FCTopology object or a list thereof.";

FCLoopBasisOverdeterminedQ::failmsg =
"Error! FCLoopBasisOverdeterminedQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopBasisOverdeterminedQ`Private`"]

Options[FCLoopBasisOverdeterminedQ] = {
	FCI						-> False,
	FCVerbose 				-> False,
	InitialSubstitutions	-> {},
	SetDimensions			-> {3, 4, D, D-1}
};

FCLoopBasisOverdeterminedQ[topos:{__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopBasisOverdeterminedQ[#, opts]&/@topos;

FCLoopBasisOverdeterminedQ[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo, optInitialSubstitutions},


		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopBasisOverdeterminedQ::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		optInitialSubstitutions = topo[[5]];

		FCLoopBasisOverdeterminedQ[Times@@topo[[2]], topo[[3]], Join[{FCI->True,
			InitialSubstitutions->optInitialSubstitutions},FilterRules[{opts}, Except[FCI|InitialSubstitutions]]]]

	];


FCLoopBasisOverdeterminedQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	ex, vecs, ca, res, fclbVerbose, dims, lmomSP,
			check, hRule, vecs12New, nlCoeffs, optInitialSubstitutions},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		optInitialSubstitutions = OptionValue[InitialSubstitutions];

		If[	FreeQ2[expr,lmoms],
			Message[FCLoopBasisOverdeterminedQ::failmsg, "The input expression does not depend on the given loop momenta."];
			Abort[]
		];

		If[	!OptionValue[FCI],
			{ex,optInitialSubstitutions} = FCI[{expr,optInitialSubstitutions}],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasisOverdeterminedQ::fail,ToString[ex,InputForm]];
			Abort[]
		];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Entering with: ", ex, FCDoControl->fclbVerbose];
		FCPrint[3,"FCLoopBasisOverdeterminedQ: Loop momenta: ", lmoms, FCDoControl->fclbVerbose];

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->OptionValue[SetDimensions], Rest->False];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		vecs = vecs/.optInitialSubstitutions;

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Vectors after applying substitutions: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)

		(* This is to make cases like Sqrt[CSPD[k]] work, otherwise CoefficientArrays would fail here.*)

		nlCoeffs=Select[vecs[[2]], !MemberQ[{Pair, CartesianPair, TemporalPair}, Head[#]] &];
		hRule = Map[Rule[#, Unique["caVar"]] &, nlCoeffs];
		vecs12New = {vecs[[1]] //. hRule, Join[Complement[vecs[[2]],nlCoeffs],Last/@hRule]};
		ca = Normal[CoefficientArrays@@vecs12New];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		(* ... and check if some of those vectors are linearly dependent *)
		res = (NullSpace[Transpose[Last[ca]]] =!= {});

		res
	];


FCPrint[1,"FCLoopBasisOverdeterminedQ.m loaded."];
End[]
