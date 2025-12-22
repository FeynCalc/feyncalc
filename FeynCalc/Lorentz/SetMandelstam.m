(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SetMandelstam													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Defines kinematical invariants

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

SetMandelstam::usage =
"SetMandelstam[s, t, u, p1 , p2 , p3 , p4 , m1 , m2 , m3 , m4 ] defines the
Mandelstam variables  $s=(p_1+p_2)^2$, $t=(p_1+p_3)^2$, $u=(p_1+p_4)^2$ and
sets the momenta on-shell: $p_1^2=m_1^2$, $p_2^2=m_2^2$, $p_3^2=m_3^2$,
$p_4^2=m_4^2$. Notice that $p_1+p_2+p_3+p_4=0$ is assumed.

SetMandelstam[x, {p1, p2, p3, p4, p5}, {m1, m2, m3, m4, m5}] defines $x[i, j]
= (p_i+p_j)^2$ and sets the $p_i$ on-shell. The $p_i$ satisfy: $p_1 + p_2 +
p_3 + p_4 + p_5 = 0$.";

SetMandelstam::failmsg =
"Error! SetMandelstam has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SetMandelstam`Private`"]

Options[SetMandelstam] = {
	Dimension 	-> {4, D},
	FCVerbose	-> False
};

(*smVarSimp*)
small2/: small2[x_]^n_ :=
	small2[x^2] /; n > 0;

small2/: small2[_] _ :=
	0;

small3/: small3[_] + a_ :=
	a;

small4[x_^m_] :=
	SmallVariable[x]^m;

smVarSimp[x_] :=
	x/;FreeQ[x,SmallVariable];

smVarSimp[x_] :=
	x/.SmallVariable->small2/.small2->small3/. small3->small4/.small4->SmallVariable /;!FreeQ[x,SmallVariable];


smVarSet[a_,b_,___] :=
	set[a,smVarSimp[(b//Expand)]]/.set->Set;

SetMandelstam[s_,t_,u_,p1_,p2_,p3_,p4_,m1_,m2_,m3_,m4_, OptionsPattern[]] :=
	Block[ {mandelstamRels, solveSpVars, sol, dims, smVerbose, solPair, solNotPair,
			varHead, setFunc},

		If [OptionValue[FCVerbose]===False,
			smVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				smVerbose=OptionValue[FCVerbose]
			];
		];

		dims = OptionValue[Dimension];

		If[ Head[dims] =!= List,
			dims = { dims }
		];

		If[	TrueQ[FreeQ[{m1,m2,m3,m4},SmallVariable]],
			{varHead, setFunc} = {Identity, Set},
			{varHead, setFunc} = {smVarSimp, smVarSet}
		];


		FCPrint[1, "SetMandelstam: Entering.", FCDoControl->smVerbose];

		FCPrint[2, "SetMandelstam: Entering with: ", {s,t,u}, " ", {p1,p2,p3,p4}, " ", {m1,m2,m3,m4}, FCDoControl->smVerbose];

		mandelstamRels = Union[Flatten[Join[Table[{
			ScalarProduct[p1,p1,Dimension->dims[[i]]] == m1^2,
			ScalarProduct[p2,p2,Dimension->dims[[i]]] == m2^2,
			ScalarProduct[p3,p3,Dimension->dims[[i]]] == m3^2,
			ScalarProduct[p4,p4,Dimension->dims[[i]]] == m4^2,
			ScalarProduct[p1,p2,Dimension->dims[[i]]] == varHead[1/2 s - 1/2 m1^2 - 1/2 m2^2],
			ScalarProduct[p1,p3,Dimension->dims[[i]]] == varHead[1/2 t - 1/2 m1^2 - 1/2 m3^2],
			ScalarProduct[p1,p4,Dimension->dims[[i]]] == varHead[1/2 u - 1/2 m1^2 - 1/2 m4^2],
			ScalarProduct[p2,p3,Dimension->dims[[i]]] == varHead[1/2 u - 1/2 m2^2 - 1/2 m3^2],
			ScalarProduct[p2,p4,Dimension->dims[[i]]] == varHead[1/2 t - 1/2 m2^2 - 1/2 m4^2],
			ScalarProduct[p3,p4,Dimension->dims[[i]]] == varHead[1/2 s - 1/2 m3^2 - 1/2 m4^2]
										}, {i, Length[dims]}
									]]]];

		FCPrint[3, "SetMandelstam: Relations for Mandelstam variables: ", mandelstamRels, FCDoControl->smVerbose];

		If[ FreeQ2[{p1,p2,p3,p4}, {Plus,Times}],

			(*Purely symbolic momenta, all ingoing*)
			mandelstamRels = Union[Flatten[{mandelstamRels, FCE[mandelstamRels]}]];

			(* This splitting prevents issues when using parallelization *)
			{solPair, solNotPair} = {SelectNotFree[mandelstamRels,Pair], SelectFree[mandelstamRels,Pair]};

			FCPrint[1, "SetMandelstam: Setting values for SPs and SPDs.", FCDoControl->smVerbose];
			solNotPair  = solNotPair /. Equal->setFunc;

			FCPrint[1, "SetMandelstam: Setting values for Pairs.", FCDoControl->smVerbose];
			solPair = SelectNotFree[mandelstamRels,Pair];

			solPair = solPair/. Equal->setFunc,

			(*Otherwise, mostly the case for scattering processes*)
			mandelstamRels = ExpandScalarProduct[mandelstamRels]//Expand;
			(*List containing all variations of momenta (FCI+FCE), fully expanded *)
			mandelstamRels = Union[mandelstamRels, FCE[mandelstamRels]];

			(* Need to solve this for all unique scalar product variables *)
			solveSpVars = Cases2[mandelstamRels, {Pair, SP, SPD}];

			(* Sanity check *)
			If[ Complement[Head/@solveSpVars,{Pair, SP, SPD}] =!= {},
				Message[SetMandelstam::failmsg, "Failed to set the scalar products."];
				Abort[]
			];

			sol = Solve[mandelstamRels, solveSpVars]//First;

			(* Sanity check *)
			If[ sol === {},
				Message[SetMandelstam::failmsg, "Failed to resolve the scalar products."];
				Abort[]
			];
			FCPrint[3, "SetMandelstam: Rules for scalar products: ", sol, FCDoControl->smVerbose];

			(* This splitting prevents issues when using parallelization *)
			{solPair, solNotPair} = {SelectNotFree[sol,Pair], SelectFree[sol,Pair]};

			FCPrint[1, "SetMandelstam: Setting values for SPs and SPDs.", FCDoControl->smVerbose];
			solNotPair  = solNotPair /. Rule->setFunc;

			FCPrint[1, "SetMandelstam: Setting values for Pairs.", FCDoControl->smVerbose];
			solPair = SelectNotFree[solPair,Pair];
			solPair = solPair/. Rule->setFunc;


		];


		FCPrint[1, "SetMandelstam: Leaving.", FCDoControl->smVerbose];
		Join[solPair,solNotPair]
	];


SetMandelstam[x_, pl_List, ml_List, OptionsPattern[]] :=
	Block[{	smVerbose, mandelstamRels, pairVars, sol, n = Length[ml], eqq,
			dims, var, nsol={}, solveSpVars, j1,j2, varHead, setFunc,
			solPair, solNotPair},

		If [OptionValue[FCVerbose]===False,
			smVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				smVerbose=OptionValue[FCVerbose]
			];
		];

		dims = OptionValue[Dimension];

		If[ Head[dims] =!= List,
			dims = { dims }
		];

		If[	TrueQ[FreeQ[ml,SmallVariable]],
			{varHead, setFunc} = {Identity, Set},
			{varHead, setFunc} = {smVarSimp, smVarSet}
		];


		mandelstamRels = Join[
			Table[ScalarProduct[pl[[i]], pl[[i]]] == ml[[i]]^2, {i,1,n}],
			Table[ScalarProduct[pl[[j]], pl[[j+1]]] == varHead[1/2 x[j,j+1] - 1/2 ml[[j]]^2 - 1/2 ml[[j+1]]^2], {j,1,n-1}],
			{ScalarProduct[ pl[[1]],pl[[n]] ] == varHead[1/2 x[1,n] - 1/2 ml[[1]]^2 - 1/2 ml[[n]]^2]}];

		FCPrint[3, "SetMandelstam: Relations for Mandelstam variables: ", mandelstamRels, FCDoControl->smVerbose];

		mandelstamRels  = mandelstamRels//ExpandScalarProduct//Expand;
		mandelstamRels = Union[Join@@(Map[ChangeDimension[mandelstamRels, #]&, dims])];
		mandelstamRels = Flatten[Union[Join[mandelstamRels, FCE[mandelstamRels]]]];

		solveSpVars = Cases2[mandelstamRels, {Pair, SP, SPD}];
		FCPrint[3, "SetMandelstam: solveSpVars: ", solveSpVars, FCDoControl->smVerbose];


		sol = Solve[mandelstamRels,solveSpVars]//First;

		(* Sanity check *)
		If[ sol === {},
			Message[SetMandelstam::failmsg, "Failed to resolve the scalar products, use FCClearScalarProducts[] and try again."];
			Abort[]
		];
		FCPrint[3, "SetMandelstam: Rules for scalar products: ", sol, FCDoControl->smVerbose];

		(* This splitting prevents issues when using parallelization *)
		{solPair, solNotPair} = {SelectNotFree[sol,Pair], SelectFree[sol,Pair]};

		FCPrint[1, "SetMandelstam: Setting values for SPs and SPDs from sol.", FCDoControl->smVerbose];
		solNotPair  = solNotPair /. Rule->setFunc;

		FCPrint[1, "SetMandelstam: Setting values for Pairs from sol.", FCDoControl->smVerbose];
		solPair = SelectNotFree[solPair,Pair];
		solPair = solPair/. Rule->setFunc;
		sol = Join[solPair,solNotPair];


		(* Remaining scalar products that were not set yet*)
		var = Flatten[Table[ScalarProduct[ pl[[k]], pl[[l]] ], {k, 1, n}, {l, k + 1, n}]]//ExpandScalarProduct;
		var = Cases2[var, Pair];
		var = Union[Join@@Map[ChangeDimension[var, #]&, dims]];
		var = Union[Join[var, FCE[var]]];

		If[ TrueQ[Length[var] > 0],

			eqq = Join[
				{ScalarProduct[Total[pl]] == 0},
				Table[ScalarProduct[pl[[l]] +  pl[[n]]] - ScalarProduct[-Total[Drop[pl, {l}]] + pl[[n]]] ==0 , {l, 2,n-3}],

				Table[If[EvenQ[j2 - j1], {
					ScalarProduct[pl[[j1]] + pl[[j2]]] - ScalarProduct[pl[[j1]] -Total[Drop[pl, {j2}]]] == 0,
					ScalarProduct[pl[[j1]] + pl[[j2]]] - ScalarProduct[-Total[Drop[pl, {j1}]]  + pl[[j2]]] == 0
				}, Unevaluated[Sequence[]]], {j1,n - 2}, {j2, j1 + 2, n}]

				]//Flatten//ExpandScalarProduct;

			eqq = Union[Join@@Map[ChangeDimension[eqq, #]&, dims]];
			eqq = Union[Join[eqq, FCE[eqq]]];


			nsol = Solve[ eqq, var ]//First;

			(* Sanity check *)
			If[ nsol === {},
				Message[SetMandelstam::failmsg, "Failed to resolve the scalar products, use FCClearScalarProducts[] and try again."];
				Abort[]
			];

			(* This splitting prevents issues when using parallelization *)
			{solPair, solNotPair} = {SelectNotFree[nsol,Pair], SelectFree[nsol,Pair]};

			FCPrint[1, "SetMandelstam: Setting values for SPs and SPDs from nsol.", FCDoControl->smVerbose];
			solNotPair  = solNotPair /. Rule->setFunc;

			FCPrint[1, "SetMandelstam: Setting values for Pairs from nsol.", FCDoControl->smVerbose];
			solPair = SelectNotFree[solPair,Pair];
			solPair = solPair/. Rule->setFunc;
			nsol = Join[solPair,solNotPair],
			nsol = {}
		];

		MapAll[Expand, Append[sol, nsol]//Flatten ]
	];

FCPrint[1,"SetMandelstam.m loaded."];
End[]
