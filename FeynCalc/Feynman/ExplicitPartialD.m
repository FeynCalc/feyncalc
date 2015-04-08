(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

ExplicitPartialD::usage =
"ExplicitPartialD[exp] inserts in exp the definition for
LeftRightPartialD[z] (and LeftRightPartialD2[z]).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ExplicitPartialD`Private`"]

(* ******************************************************************** *)


ExplicitPartialD[x_] :=
	Block[ {i, j , leftr2},
		If[ True,
			leftr2 = LeftRightPartialD2
		];
		leftr2 =
		If[ FreeQ2[x, {LeftRightPartialD, leftr2}],
			x,
			x /. { LeftRightPartialD[a_]^n_Integer :>
					1/2^n ((*DOT*)(*Change to allow any mltiplication. F.Orellana, 24/2-2003.*)
					Sequence @@ Table[(RightPartialD[a] - LeftPartialD[a]), {j, n}])
				} /.
				{ LeftRightPartialD2[a_]^n_Integer :>
						(DOT @@ Table[(RightPartialD[a] + LeftPartialD[a]), {j, n}])
				} /.
				{
					LeftRightPartialD[a_]^n_ /; Head[n] =!= Integer:>
				(i =  Unique["k"];
				OPESum[DOT[1/2^n, Binomial[n, i], (-1)^(n-i),
												(LeftPartialD[a]^(n-i)),
												(RightPartialD[a]^i)
						], {i, 0, n}
					]
				)
				} /.
				{
					LeftRightPartialD2[a_]^n_ /; Head[n] =!= Integer:>
				(i =  Unique["k"];
				OPESum[DOT[Binomial[n, i], (LeftPartialD[a]^(n-i)),
										(RightPartialD[a]^i)
						], {i, 0, n}
					]
				)
				}
					/. {LeftRightPartialD[a_] :>
						(1/2 (RightPartialD[a] - LeftPartialD[a]))
					} /.
					{LeftRightPartialD2[a_] :>
						(RightPartialD[a] + LeftPartialD[a])
					}
		]
	];

FCPrint[1,"ExplicitPartialD.m loaded."];
End[]
