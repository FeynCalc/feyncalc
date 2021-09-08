(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitPartialD													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Partial derivative												*)

(* ------------------------------------------------------------------------ *)

ExplicitPartialD::usage =
"ExplicitPartialD[exp] inserts the definitions for LeftRightPartialD and
LeftRightPartialD2.";

Begin["`Package`"]
End[]

Begin["`ExplicitPartialD`Private`"]

Options[ExplicitPartialD] = {
};

ExplicitPartialD[expr_, OptionsPattern[]] :=
	Block[	{i, j , ex, res},

		ex = expr;

		If[	FreeQ2[ex,{LeftRightPartialD,LeftRightPartialD2}],
			Return[ex]
		];

		res = ex /.
			LeftRightPartialD[a__]^n_Integer :> 1/2^n (Sequence @@ Table[(RightPartialD[a] - LeftPartialD[a]), {j, n}]) /.
			LeftRightPartialD2[a__]^n_Integer :>	(DOT @@ Table[(RightPartialD[a] + LeftPartialD[a]), {j, n}]) /.
			LeftRightPartialD[a__]^n_ /; Head[n] =!= Integer :>
				(i =  Unique["k"]; OPESum[DOT[1/2^n, Binomial[n, i], (-1)^(n-i), (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}])  /.
			LeftRightPartialD2[a__]^n_ /; Head[n] =!= Integer :>
				(i =  Unique["k"]; OPESum[DOT[Binomial[n, i], (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}]) /.
			LeftRightPartialD[a__] :> (1/2 (RightPartialD[a] - LeftPartialD[a])) /.
				LeftRightPartialD2[a__] :>
					(RightPartialD[a] + LeftPartialD[a]);

		res

	];

FCPrint[1,"ExplicitPartialD.m loaded."];
End[]
