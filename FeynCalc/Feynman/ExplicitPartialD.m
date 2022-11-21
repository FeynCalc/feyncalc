(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitPartialD													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Partial derivative												*)

(* ------------------------------------------------------------------------ *)

ExplicitPartialD::usage =
"ExplicitPartialD[exp] inserts the definitions for LeftRightPartialD,
LeftRightPartialD2, LeftRightNablaD, LeftRightNablaD2, LeftNablaD and
RightNablaD";

Begin["`Package`"]
End[]

Begin["`ExplicitPartialD`Private`"]

Options[ExplicitPartialD] = {
};

ExplicitPartialD[expr_, OptionsPattern[]] :=
	Block[	{i, j , ex, res},

		ex = expr;

		If[	FreeQ2[ex,{LeftRightPartialD,LeftRightPartialD2,LeftRightNablaD,LeftRightNablaD2,LeftNablaD,RightNablaD}],
			Return[ex]
		];

		res = ex /. {
			LeftRightPartialD[a__]^n_Integer :>
				1/2^n (Sequence @@ Table[(RightPartialD[a] - LeftPartialD[a]), {j, n}]),
			LeftRightPartialD2[a__]^n_Integer :>
				(DOT @@ Table[(RightPartialD[a] + LeftPartialD[a]), {j, n}]),

			LeftRightNablaD[a__]^n_Integer :>
				1/2^n (Sequence @@ Table[(RightNablaD[a] - LeftNablaD[a]), {j, n}]),
			LeftRightNablaD2[a__]^n_Integer :>
				(DOT @@ Table[(RightNablaD[a] + LeftNablaD[a]), {j, n}])
		} /. {
			LeftRightPartialD[a__]^n_ /; Head[n] =!= Integer :>
				(i =  Unique["k"]; OPESum[DOT[1/2^n, Binomial[n, i], (-1)^(n-i), (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}]),

			LeftRightPartialD2[a__]^n_ /; Head[n] =!= Integer :>
				(i =  Unique["k"]; OPESum[DOT[Binomial[n, i], (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}])
		} /. {
			LeftRightPartialD[a__] :>
				(1/2 (RightPartialD[a] - LeftPartialD[a])),
			LeftRightPartialD2[a__] :>
				(RightPartialD[a] + LeftPartialD[a]),

			LeftRightNablaD[a__] :>
				(1/2 (RightNablaD[a] - LeftNablaD[a])),
			LeftRightNablaD2[a__] :>
				(RightNablaD[a] + LeftNablaD[a])
		} /. {
			RightNablaD[a_]/; MemberQ[{CartesianIndex,CartesianMomentum}, Head[a]] :>
				FeynCalc`Package`MetricS RightPartialD[a],

			LeftNablaD[a_]/; MemberQ[{CartesianIndex,CartesianMomentum}, Head[a]] :>
				FeynCalc`Package`MetricS LeftPartialD[a],


			RightNablaD[{a_,rest___}]/; MemberQ[{CartesianIndex,CartesianMomentum}, Head[a]] :>
				FeynCalc`Package`MetricS RightPartialD[{a,rest}],

			LeftNablaD[{a_,rest___}]/; MemberQ[{CartesianIndex,CartesianMomentum}, Head[a]] :>
				FeynCalc`Package`MetricS LeftPartialD[{a,rest}]
		};

		res

	];

FCPrint[1,"ExplicitPartialD.m loaded."];
End[]
