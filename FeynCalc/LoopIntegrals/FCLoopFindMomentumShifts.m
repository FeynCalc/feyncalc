(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindMomentumShifts											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Finds loop momentum shifts									*)

(* ------------------------------------------------------------------------ *)

FCLoopFindMomentumShifts::usage =
"FCLoopFindMomentumShifts[source, target, {p1, p2, ...}] finds loop momentum shifts \
that bring loop integrals or topologies in the list source to the form specified \
in target. The integrals/topologies in intFrom and intTo are assumed to be equivalent \
and their denominators must be properly ordered via FCToPakForm. Furthermore, target \
must be provided as a list of FeynAmpDenominator objects, while intFrom is a list of \
such lists.";

FCLoopFindMomentumShifts::failmsg =
"Error! FCLoopFindMomentumShifts has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindMomentumShifts`Private`"]

fcflsVerbose::usage = "";

Options[FCLoopFindMomentumShifts] = {
	FCI 						-> False,
	FCVerbose 					-> False
};

FCLoopFindMomentumShifts[fromRaw_List, toRaw_, lmoms_List, OptionsPattern[]] :=
	Block[{from, to, res, time, shifts},

		If[	OptionValue[FCVerbose] === False,
			fcflsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcflsVerbose = OptionValue[FCVerbose]];
		];

		If[OptionValue[FCI],
			{from, to} = {fromRaw, toRaw},
			{from, to} = FCI[{fromRaw, toRaw}]
		];

		If[	!FreeQ[{from, to},FCTopology],
			{from, to} = {from, to} /. FCTopology[_,zz_List,___] :> zz
		];

		FCPrint[1, "FCLoopFindMomentumShifts: Entering.", FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCLoopFindMomentumShifts: List of source topologies: ", from, FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCLoopFindMomentumShifts: Target topology: ", to, FCDoControl -> fcflsVerbose];

		If[	!MatchQ[from,{{__FeynAmpDenominator}..}],
			Message[FCLoopFindMomentumShifts::failmsg,"The list of source topologies is not a list of lists of FeynAmpDenominator objects."];
			Abort[]
		];

		If[	!MatchQ[to,{__FeynAmpDenominator}],
			Message[FCLoopFindMomentumShifts::failmsg,"The target topology is not a list of FeynAmpDenominator objects."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindMomentumShifts: Finding loop momentum shifts.", FCDoControl -> fcflsVerbose];
		shifts = findShifts[#,to,lmoms]&/@from;
		FCPrint[1, "FCLoopFindMomentumShifts: Done finding loop momentum shifts, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcflsVerbose];


		FCPrint[1, "FCLoopFindMomentumShifts: Leaving.", FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCLoopFindMomentumShifts: Leaving with: ", shifts, FCDoControl -> fcflsVerbose];

		shifts
	];


findShifts[from:{__FeynAmpDenominator},to:{__FeynAmpDenominator}, lmoms_List]:=
	Block[{lhs, rhs, eq,mark, vars, sol, res},
		lhs = MomentumCombine[from,FCI->True];
		rhs = MomentumCombine[to,FCI->True];
		{lhs, rhs} = {lhs, rhs} /. {
			FeynAmpDenominator[PropagatorDenominator[Momentum[mom_, _], _]] :> mom,
			FeynAmpDenominator[StandardPropagatorDenominator[Momentum[mom_, _], 0, _, {1, _}]] :> mom,
			FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[mom_, _], 0, _, {1, _}]] :> mom,
			FeynAmpDenominator[StandardPropagatorDenominator[0, x_, _, {1, _}]]/; x=!=0 :> Unevaluated[Sequence[]],
			FeynAmpDenominator[CartesianPropagatorDenominator[0, x_, _, {1, _}]]/; x=!=0 :> Unevaluated[Sequence[]]

		};

		FCPrint[3, "FCLoopFindMomentumShifts: Preliminary lhs: ", lhs, FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCLoopFindMomentumShifts: Preliminary rhs: ", rhs, FCDoControl -> fcflsVerbose];

		If[	!FreeQ[{lhs,rhs},FeynAmpDenominator],
			Message[FCLoopFindMomentumShifts::failmsg,"Failed to set up a proper system of equations."];
			Abort[]
		];

		vars = mark/@(lmoms);
		lhs = lhs /. Thread[Rule[lmoms,vars]];

		FCPrint[3, "FCLoopFindMomentumShifts: Final lhs: ", lhs, FCDoControl -> fcflsVerbose];
		FCPrint[3, "FCLoopFindMomentumShifts: Final rhs: ", rhs, FCDoControl -> fcflsVerbose];



		eq = Thread[Equal[lhs^2,rhs^2]];

		sol = Solve[eq,vars];

		If[	sol==={},
			Message[FCLoopFindMomentumShifts::failmsg,"Failed to find momentum shifts for one of the topologies."];
			Abort[]
		];

		res = (First[sol] /. mark -> Identity);

		If[	MomentumCombine[FDS[from/.res]]=!=MomentumCombine[to],
			Message[FCLoopFindMomentumShifts::failmsg,"The obtained set of shifts is incorrect."];
			Abort[]
		];

		res

	]










FCPrint[1,"FCLoopFindMomentumShifts.m loaded."];
End[]
