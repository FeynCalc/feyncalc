(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFindLoopMomentumShifts											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  	Finds loop momentum shifts									*)

(* ------------------------------------------------------------------------ *)

FCFindLoopMomentumShifts::usage =
"FCFindLoopMomentumShifts[source, target, {p1, p2, ...}] finds loop momentum shifts \
that bring loop integrals or topologies in the list source to the form specified \
in target. The integrals/topologies in intFrom and intTo are assumed to be equivalent \
and their denominators must be properly ordered via FCToPakForm. Furthermore, target \
must be provided as a list of FeynAmpDenominator objects, while intFrom is a list of \
such lists.";

FCFindLoopMomentumShifts::failmsg =
"Error! FCFindLoopMomentumShifts has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFindLoopMomentumShifts`Private`"]

fcflsVerbose::usage = "";

Options[FCFindLoopMomentumShifts] = {
	FCI 						-> False,
	FCVerbose 					-> False
};

FCFindLoopMomentumShifts[fromRaw_List, toRaw_List, lmoms_List, OptionsPattern[]] :=
	Block[{	from,to,pakFormInts, res, time, x, fromPak, toPak, shifts},

		If[	OptionValue[FCVerbose] === False,
			fcflsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcflsVerbose = OptionValue[FCVerbose]];
		];

		If[OptionValue[FCI],
			{from, to} = {fromRaw, toRaw},
			{from, to} = FCI[{fromRaw, toRaw}]
		];

		FCPrint[1, "FCFindLoopMomentumShifts: Entering.", FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCFindLoopMomentumShifts: Entering with: ", from, FCDoControl -> fcflsVerbose];

		If[!MatchQ[to,{__FeynAmpDenominator}|FCTopology[_,{__FeynAmpDenominator},__]],
			Message[FCFindLoopMomentumShifts::failmsg,""];
		];

		shifts = findShifts[#,to,lmoms]&/@from;


		FCPrint[1, "FCFindLoopMomentumShifts: Leaving.", FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCFindLoopMomentumShifts: Leaving with: ", shifts, FCDoControl -> fcflsVerbose];

		shifts
	];


findShifts[from:{__FeynAmpDenominator},to:{__FeynAmpDenominator}, lmoms_List]:=
	Block[{lhs, rhs, eq,mark, vars, sol},
		lhs = MomentumCombine[from,FCI->True];
		rhs = MomentumCombine[to,FCI->True];
		{lhs, rhs} = {lhs, rhs} /. {
			FeynAmpDenominator[PropagatorDenominator[Momentum[mom_, _], _]] :> mom,
			FeynAmpDenominator[StandardPropagatorDenominator[Momentum[mom_, _], 0, _, {1, _}]] :> mom,
			FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[mom_, _], 0, _, {1, _}]] :> mom,
			FeynAmpDenominator[StandardPropagatorDenominator[0, x_, _, {1, _}]]/; x=!=0 :> Unevaluated[Sequence[]],
			FeynAmpDenominator[CartesianPropagatorDenominator[0, x_, _, {1, _}]]/; x=!=0 :> Unevaluated[Sequence[]]

		};

		FCPrint[3, "FCFindLoopMomentumShifts: Preliminary lhs: ", lhs, FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCFindLoopMomentumShifts: Preliminart rhs: ", rhs, FCDoControl -> fcflsVerbose];

		If[	!FreeQ[{lhs,rhs},FeynAmpDenominator],
			Message[FCFindLoopMomentumShifts::failmsg,"Failed to set up a proper system of equations."];
			Abort[]
		];

		vars = mark/@(lmoms);
		lhs = lhs /. Thread[Rule[lmoms,vars]];

		FCPrint[3, "FCFindLoopMomentumShifts: Final lhs: ", lhs, FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCFindLoopMomentumShifts: Final rhs: ", rhs, FCDoControl -> fcflsVerbose];

		eq = Thread[Equal[lhs,rhs]];

		sol = Solve[eq,vars];

		If[	sol==={},
			Message[FCFindLoopMomentumShifts::failmsg,"Failed to find momentum shifts for one of the topologies."];
			Abort[]
		];

		(First[sol] /. mark -> Identity)

	]










FCPrint[1,"FCFindLoopMomentumShifts.m loaded."];
End[]
