(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitPartialD													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary: Partial derivative												*)

(* ------------------------------------------------------------------------ *)

ExplicitPartialD::usage =
"ExplicitPartialD[exp] inserts the definitions for \
LeftRightPartialD[z] and LeftRightPartialD2[z].";

Begin["`Package`"]
End[]

Begin["`ExplicitPartialD`Private`"]


ExplicitPartialD[x_] :=
	Block[ {i, j , leftr2},
		leftr2 = LeftRightPartialD2;
		leftr2 =
		If[ FreeQ2[x, {LeftRightPartialD, leftr2}],
			x,
			x /.	LeftRightPartialD[a_]^n_Integer :> 1/2^n (Sequence @@ Table[(RightPartialD[a] - LeftPartialD[a]), {j, n}]) /.

					LeftRightPartialD2[a_]^n_Integer :>	(DOT @@ Table[(RightPartialD[a] + LeftPartialD[a]), {j, n}]) /.

					LeftRightPartialD[a_]^n_ /; Head[n] =!= Integer:> (i =  Unique["k"];
					OPESum[DOT[1/2^n, Binomial[n, i], (-1)^(n-i), (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}])  /.

					LeftRightPartialD2[a_]^n_ /; Head[n] =!= Integer:> (i =  Unique["k"];
					OPESum[DOT[Binomial[n, i], (LeftPartialD[a]^(n-i)), (RightPartialD[a]^i)], {i, 0, n}]) /.

					LeftRightPartialD[a_] :> (1/2 (RightPartialD[a] - LeftPartialD[a])) /.

					LeftRightPartialD2[a_] :> (RightPartialD[a] + LeftPartialD[a])
		]
	];

FCPrint[1,"ExplicitPartialD.m loaded."];
End[]
