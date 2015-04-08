(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Solve3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: First version written 1995 for Tdec, slight modifications later*)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Solve2 and Solve3 *)

(* ------------------------------------------------------------------------ *)

Solve2::usage=
"Solve2 is equivalent to Solve, except that it works only for
linear equations (and returns just a list)
and accepts the options Factoring and FinalSubstitutions.";

Solve3::usage=
"Solve3 is equivalent to Solve, except that it works only for
linear equations (and returns just a list)
and uses the \"high school algorithm\" and is sometimes better than
Solve for systems involving rational polynomials.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Solve`Private`"]

Options[Solve2] = {Factoring -> Factor2, FinalSubstitutions -> {}};
Options[Solve3] = {Factoring -> False, FinalSubstitutions -> {}, ParallelMap -> False};

Solve2[a_/;Head[a]=!=List, b__] :=
	Solve2[{a}, b];
Solve2[a_, b_/;Head[b]=!=List, c___] :=
	Solve2[a, {b}, c];

Solve2[ai_List, bii_, ops___Rule] :=
	Block[{fixeq, temp, re, factor , finsub, a, b, bi,dumsub, dum},
		bi = Flatten[{bii}];
		dumsub = Table[bi[[ij]] -> dum[ij],{ij,Length[bi]}];
		a = Flatten[{ai}] /. dumsub;
		b = Last/@dumsub;
		factor = Factoring /. {ops} /. Options[Solve2];
		finsub = FinalSubstitutions/. {ops} /. Options[Solve2];
		fixeq[x_]:=
			Isolate[Collect2[If[Head[x] === Equal, x[[1]] - x[[2]], x],
			b, Factoring -> factor ,Expanding -> False], b, IsolateNames->$soso];
		temp = Map[fixeq, a];
		(* soback /: HoldForm[soback[i_]] := soback[i]; *)
		re = (Solve[Map[(# == 0)&, temp], b][[1]]);
		(* re = FixedPoint[(# /. $soso -> soback /. soback -> $soso	)&, re];*)
		re = FixedPoint[ReleaseHold,re]/.Map[Reverse,dumsub];
		If[factor === False,
			re = re /. finsub,
			re = Map[(#[[1]] -> factor[(#[[2]])/.finsub])&, re];
		];
		re
	];


Solve3[a_/;Head[a]=!=List, b__] :=
	Solve3[{a}, b];

Solve3[eqq_List, clii_List, ops___Rule] :=
	Block[{cli = clii, factor , finsub,newel, lneq, neqh,isol, neq, newneq,
		col,  new, res = {}, parmap, pmap, starttime = AbsoluteTime[]},
		factor = Factoring /. {ops} /. Options[Solve3];
		finsub = FinalSubstitutions/. {ops} /. Options[Solve3];
		parmap = ParallelMap /. {ops} /. Options[Solve3];
		(* High - school algorithm *)

		isol[xy__] :=
			If[Length[{xy}] < 10,
				isol[xy] = Isolate[Plus[xy],cli,IsolateNames->LL, IsolateSplit->Infinity],
							Isolate[Plus[xy],cli,IsolateNames->LL, IsolateSplit->Infinity]
			];

		With[{cli = cli},
			col = ( FCPrint[2," Collect with Factor "];
			Collect[#, cli, Factor] ) &
		];

		If[TrueQ[parmap],
			pmap = ParallelMap;
			DistributeDefinitions[ cli, col, FreeQ2, $VeryVerbose],
			pmap = Map
		];


		(*
		FCPrint[1,"PAREVAL = ", ParallelEvaluate[{$VeryVerbose, col}]];
		*)

		specsimp[{}, b_Rule] :=
			{b};
		specsimp[a_List, b_Rule] :=
			pmap[(#[[1]] -> (col[#[[2]] /. b]))&, a];

		neq = eqq /. Equal[a_, b_] :> (a-b);
		For[i = 1, i <= Length[eqq], i++,
			If[!FreeQ[neq, cli[[i]]],
				FCPrint[1,"solve3 i = ",i,"    time used : ",
				Round[(starttime-AbsoluteTime[])/60], " minutes" ];
				While[FreeQ[neq1 = (*col[*)neq[[1]] /. res(*]*), cli[[i]]],
					FCPrint[2,"rotating ", i];
					neq = RotateLeft[neq = Prepend[Rest[neq],neq1]]
				];
				FCPrint[2,"solving for ",cli[[i]]];
				(*{neq1,cli[[i]]}>>"neq1.s";*)
				new = Solve2[neq1, cli[[i]], Factoring -> False][[1]];
				(*
				FCPrint[3,"solution = ",new//InputForm];
				new >>"new.s";

				new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> col];
				CHANGE 20100110
				new = new[[1]] -> Collect2[new[[2]], cli, Factoring -> Factor2];
				*)
				new = new[[1]] -> Collect[new[[2]], cli, Factor];
				If[!FreeQ2[new[[2]], cli],
					new = new[[1]] -> Map[Cancel, new[[2]]];
				];
				FCPrint[3,"solution = ",new//InputForm];
					neq = Rest[neq];
				If[i>1,
					res = Append[specsimp[res, new], new],
					res = {new}
				];
				If[i<Length[eqq],
					FCPrint[1,"UPDATING ", LeafCount @ neq];
					newneq = {};
					(*
					neqh = Hold@@{neq};
					lneq = Length[neq];
					*)

					With[{col=col, neqres = neq /. res},
						neq = pmap[ col, neqres ];
					];
					(* For[iij = 1, iij <= lneq, iij++,
					FCPrint[2,"updating " , iij , " out of ",Length[neq]]

					newel = neqh[[1, iij]] /. res;
					If[newel === neqh[[1, iij]],
						AppendTo[newneq, newel],
						AppendTo[newneq, col[newel]]
					];
					Clear[newel];
					];
					neq = newneq; *)
					FCPrint[1,"leafcount neq = ", LeafCount[neq]];
				];
			];
		];
		res = res /. finsub;
		If[factor =!= False,
			res = pmap[(#[[1]] -> factor[#[[2]]])&,	res]
		];
		res
	];

FCPrint[1,"Solve.m loaded"];
End[]
