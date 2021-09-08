(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SetMandelstam													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Defines of kinematical invariants								*)

(* ------------------------------------------------------------------------ *)

SetMandelstam::usage =
"SetMandelstam[s, t, u, p1 , p2 , p3 , p4 , m1 , m2 , m3 , m4 ] defines the
Mandelstam variables  $s=(p_1+p_2)^2$, $t=(p_1+p_3)^2$, $u=(p_1+p_4)^2$ and
sets the momenta on-shell: $p_1^2=m_1^2$, $p_2^2=m_2^2$, $p_3^2=m_3^2$,
$p_4^2=m_4^2$. Notice that $p_1+p_2+p_3+p_4=0$ is assumed.

SetMandelstam[x, {p1, p2, p3, p4, p5}, {m1, m2, m3, m4, m5}] defines $x[i, j]
= (p_i+p_j)^2$ and sets the $p_i$ on-shell. The $p_i$ satisfy: $p_1 + p_2 +
p_3 + p_4 + p_5 = 0$.\"";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SetMandelstam`Private`"]

Options[SetMandelstam] = {
	Dimension -> {4, D}
};

(*sma*)
small2/: small2[x_]^n_ :=
	small2[x^2] /; n > 0;

small2/: small2[_] _ :=
	0;

small3/: small3[_] + a_ :=
	a;

small4[x_^m_] :=
	SmallVariable[x]^m;

sma[x_] :=
	x/;FreeQ[x,SmallVariable];

sma[x_] :=
	x/.SmallVariable->small2/.small2->small3/. small3->small4/.small4->SmallVariable;


setit[a_,b_,___] :=
	set[a,sma[(b//Expand)]]/.set->Set;


SetMandelstam[s_,t_,u_, { {p1_, m12_}, {p2_, m22_} } -> { {p3_, m32_}, {p4_, m42_} }] :=
	SetMandelstam[s,t,u, p1, p2, -p3, -p4, Sqrt[m12], Sqrt[m22], Sqrt[m32], Sqrt[m42]];

SetMandelstam[s_,t_,u_,p1_,p2_,p3_,p4_,m1_,m2_,m3_,m4_, OptionsPattern[]] :=
	Block[ {settemp,setvars,sol, dims},

		dims = OptionValue[Dimension];

		If[ Head[dims] =!= List,
			dims = { dims }
		];

		settemp = Union[Flatten[Join[Table[{
			ScalarProduct[p1,p1,Dimension->dims[[i]]] == m1^2,
			ScalarProduct[p2,p2,Dimension->dims[[i]]] == m2^2,
			ScalarProduct[p3,p3,Dimension->dims[[i]]] == m3^2,
			ScalarProduct[p4,p4,Dimension->dims[[i]]] == m4^2,
			ScalarProduct[p1,p2,Dimension->dims[[i]]] == sma[1/2 s - 1/2 m1^2 - 1/2 m2^2],
			ScalarProduct[p1,p3,Dimension->dims[[i]]] == sma[1/2 t - 1/2 m1^2 - 1/2 m3^2],
			ScalarProduct[p1,p4,Dimension->dims[[i]]] == sma[1/2 u - 1/2 m1^2 - 1/2 m4^2],
			ScalarProduct[p2,p3,Dimension->dims[[i]]] == sma[1/2 u - 1/2 m2^2 - 1/2 m3^2],
			ScalarProduct[p2,p4,Dimension->dims[[i]]] == sma[1/2 t - 1/2 m2^2 - 1/2 m4^2],
			ScalarProduct[p3,p4,Dimension->dims[[i]]] == sma[1/2 s - 1/2 m3^2 - 1/2 m4^2]
										}, {i, Length[dims]}
									]]]];

		If[ FreeQ2[{p1,p2,p3,p4}, {Plus,Times}],
			settemp = Union[settemp, FCE[settemp]];
			sol = settemp /. Equal ->setit,

			settemp = ExpandScalarProduct[settemp]//Expand;
			settemp = Union[settemp, FCE[settemp]];

			(* want also for 4 or D dimensions to set SP[p,p] = ... etc. *)
			setvars = Cases2[settemp, {Pair, SP, SPD}];
			If[ Complement[Head/@setvars,{Pair, SP, SPD}] === {},
				sol = Solve[settemp, setvars]/.Rule->setit
			];
		];
		sol
	];

(* #################################################################### *)
(*                             Main56                                   *)
(* #################################################################### *)

scalarproduct[a_, b_] :=
	FeynCalcInternal[ScalarProduct[a,b]]//ExpandScalarProduct;

SetMandelstam[x_, pl_List, ml_List, OptionsPattern[]] :=
	Block[ {settemp, setvars, sol, n = Length[ml], psu, pkl, sq2, eqq,
		dims, var, npk, nsol, j1, j2, enm},

		dims = OptionValue[Dimension];

		If[ Head[dims] =!= List,
			dims = { dims }
		];

		settemp = Join[
			Table[scalarproduct[pl[[i]], pl[[i]]] == ml[[i]]^2, {i,1,n}],
			Table[scalarproduct[pl[[j]], pl[[j+1]]] == sma[1/2 x[j,j+1] - 1/2 ml[[j]]^2 - 1/2 ml[[j+1]]^2], {j,1,n-1}],
			{scalarproduct[ pl[[1]],pl[[n]] ] == sma[1/2 x[1,n] - 1/2 ml[[1]]^2 - 1/2 ml[[n]]^2]}]//ExpandScalarProduct//Expand;

		setvars = Cases[settemp, _Pair, -1];

		settemp = Union[Join@@(Map[ChangeDimension[settemp, #]&, dims])];
		settemp = Union[Join[settemp, FCE[settemp]]];
		setvars = Union[Join@@Map[ChangeDimension[setvars, #]&, dims]];
		setvars = Union[Join[setvars, FCE[setvars]]];

		sol = Solve[settemp,setvars ]/.Rule->setit;

		sq2[y_] :=
			scalarproduct[y, y]//ExpandScalarProduct//Expand;

		pkl = {};

		For[	k = 1, k<=n, k++,
				For[	l = k+1, l<= n, l++,
						npk = scalarproduct[ pl[[k]], pl[[l]] ]//ExpandScalarProduct;
						If[	(Head[npk] === Pair) || (Head[-npk]=== Pair),
							AppendTo[pkl,{pl[[k]], pl[[l]]}]
						]
				]
		];

		psu = Plus@@pl;

		enm[a_] :=
			Expand[- Apply[Plus, Drop[pl,{a,a}]]];

		Do[
			eqq = {sq2[psu] == 0};
			eqq = Join[ eqq, Table[ sq2[pl[[l]] +  pl[[n]]] - sq2[enm[l] + pl[[n]]] ==0 , {l, 2,n-3}]
								];
			For[ j1 = 1, j1<n-2, j1++,
				For[ j2 = j1 + 2, j2<n, j2++,
					If[ EvenQ[j2-j1],
						AppendTo[ eqq, sq2[pl[[j1]] + pl[[j2]]] - sq2[pl[[j1]] + enm[j2] ] == 0],
						AppendTo[ eqq, sq2[pl[[j1]] + pl[[j2]]] - sq2[enm[j1]  + pl[[j2]]] == 0]
					]
				]
			];

			var =  ExpandScalarProduct[scalarproduct@@#&/@pkl];
			var = Select[ Variables[var], Head[#]===Pair&];

			If[ Length[dims]>0,
				eqq = Union[Join@@Map[ChangeDimension[eqq, #]&, dims]];
				eqq = Union[Join[eqq, FCE[eqq]]];
				var = Union[Join@@Map[ChangeDimension[var, #]&, dims]];
				var = Union[Join[var, FCE[var]]];
			];

			If[ Length[var] > 0,
				nsol = Solve[ eqq, var ];
				nsol = nsol /. Rule -> setit
			]
		, {2}];

		MapAll[Expand, Append[sol, nsol]//Flatten ]
	];

FCPrint[1,"SetMandelstam.m loaded."];
End[]
