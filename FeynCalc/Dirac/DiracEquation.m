(* ::Package:: *)



(* :Title: DiracEquation													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Applies Dirac equation to simplify spinor chains				*)

(* ------------------------------------------------------------------------ *)


DiracEquation::usage =
"DiracEquation[exp] applies the Dirac equation without \
expanding exp. If that is needed, use DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracEquation`Private`"]

HoldDOT;

Options[DiracEquation] = {
	FCI -> False
};

DiracEquation[ex_, OptionsPattern[]] :=
	Block[{expr,res},

		If[	!OptionValue[FCI],
			expr  = FCI[ex],
			expr  = ex
		];

		(* If there are no spinors or no Dirac matrices, then we can't apply the Dirac equation	*)
		If[	FreeQ2[expr, {Spinor,DiracGamma}],
			Return[expr];
		];

		res  = DotSimplify[diraceq[expr], Expanding->False];

		res

	];

last[_. Momentum[pe__]] :=
	Momentum[pe];
last[x_Plus] :=
	PowerExpand[Sqrt[Last[x]^2]];
diraceq[x_] :=
	x/;FreeQ[x,Spinor];
diraceq[x_] :=
	Expand[ ToDiracGamma67[x,FCI->True] /. DOT->HoldDOT //.spCDieqRules /.HoldDOT->DOT, DOT ];

HoldDOT[a___,HoldDOT[b___],c___]:= HoldDOT[a,b,c];
HoldDOT[a___,b1_HoldDOT + b2_HoldDOT + b3_:0 ,c___]:=
	HoldDOT[a,b1,c]+HoldDOT[a,b2+b3,c];

spCDieqRules = {
	(* Now that we can use equations of motion only if all involved objects are in the same dimension!!! *)
	HoldDOT[ z___,Spinor[n_. Momentum[p_, dim_ : 4] + k_:0 ,m_, op___], DiracGamma[Momentum[p_, dim_ : 4],dim_ : 4],a___] :>
		(m/n HoldDOT[ z,Spinor[n Momentum[p, dim] + k,m,op ],a] -
		If[ (k===0),
			0,
			If[ last[n Momentum[p,dim] + k] =!= Momentum[p,dim],
				0,
				1/n HoldDOT[ z, Spinor[n Momentum[p,dim] + k,m,op], DiracGamma[k,dim],a]
			]
		])/; last[n Momentum[p,dim]+k]===Momentum[p,dim],
	HoldDOT[ a___,DiracGamma[Momentum[p_, dim_ : 4],dim_ : 4], Spinor[n_. Momentum[p_, dim_ : 4] + k_. ,m_,op___],z___] :>
		(m/n HoldDOT[ a,Spinor[ n Momentum[p, dim] + k,m,op ],z] -
		If[ (k===0),
			0,
			If[ last[n Momentum[p,dim] + k] =!= Momentum[p,dim],
				0,
				1/n HoldDOT[ a, DiracGamma[k,dim], Spinor[n Momentum[p,dim] + k,m,op ], z]
			]
		]) /; last[n Momentum[p]+k]===Momentum[p],

	HoldDOT[ a___,DiracGamma[Momentum[y_,dim_:4],dim_:4], DiracGamma[Momentum[y_,dim_:4],dim_:4],b___] :>
		ExpandScalarProduct[Pair[Momentum[y,dim],Momentum[y,dim]]] HoldDOT[a,b],

(* 	Here the situation is more complicated since we need to move the slash through a certain number
	of other Dirac matrices before we reach the spinor. Note that the matrices
	in the chain may have different dimensions.	*)

	(* reaching the first spinor, if we need to anticommute past another Dirac gamma (not gamma 5)	*)
	HoldDOT[	z___,	s: Spinor[n_. Momentum[p_, dim_ : 4] + k_. ,___],
					a___DiracGamma,
					DiracGamma[x: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum),di_:4],
					DiracGamma[Momentum[p_,dim_ : 4],dim_ : 4],b___] :>
					-HoldDOT[ z,s,a, DiracGamma[Momentum[p,dim],dim],DiracGamma[x,di],b] +
					2(PairContract[x,Momentum[p,dim]] /. PairContract -> Pair)*HoldDOT[ z,s,a,b]/;
					last[n Momentum[p,dim]+k] === Momentum[p,dim],



	(* reaching the first spinor, if we need to anticommute past a Dirac gamma 5	*)
	HoldDOT[	z___,	s: Spinor[n_. Momentum[p_, dim_ : 4] + k_. ,___],
					a___DiracGamma,
					DiracGamma[(u:5|6|7)],
					DiracGamma[Momentum[p_,dim_ : 4],dim_ : 4],b___] :>
					HoldDOT[ z,s,a, Anti5[DiracGamma[u].DiracGamma[Momentum[p,dim],dim]]/.DOT|Times->HoldDOT,b]/;
					last[n Momentum[p,dim]+k] === Momentum[p,dim],

	(* reaching the last spinor, if we need to anticommute past another Dirac gamma (not gamma 5)	*)
	HoldDOT[	a___,	DiracGamma[Momentum[p_,dim_:4],dim_:4],
					DiracGamma[x: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum),di_:4],
					b___DiracGamma,
					s: Spinor[n_. Momentum[p_, dim_: 4] + k_.,___],z___] :>
						(-HoldDOT[a,DiracGamma[x,di],DiracGamma[Momentum[p,dim],dim],b,s,z]+
						2(PairContract[x,Momentum[p,dim]] /. PairContract -> Pair)*HoldDOT[a,b,s,z])/;
						last[n Momentum[p,dim]+k]===Momentum[p,dim],

	(* reaching the last spinor, if we need to anticommute past a Dirac gamma 5	*)
	HoldDOT[	a___,	DiracGamma[Momentum[p_,dim_:4],dim_:4],
					DiracGamma[(u:5|6|7)],
					b___DiracGamma,
					s: Spinor[n_. Momentum[p_, dim_: 4] + k_.,___],z___] :>
						HoldDOT[a, Anti5[DiracGamma[Momentum[p,dim],dim].DiracGamma[u],-1]/.DOT|Times->HoldDOT,b,s,z]/;
						last[n Momentum[p,dim]+k] === Momentum[p,dim]
};

FCPrint[1,"DiracEquation.m loaded."];
End[]
