(* ::Package:: *)



(* :Title: DiracEquation													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Applies Dirac equation to simplify spinor chains				*)

(* ------------------------------------------------------------------------ *)


DiracEquation::usage =
"DiracEquation[exp] applies the Dirac equation without expanding exp. If
expansions are necessary, use DiracSimplify.";

DiracEquation::failmsg =
"Error! DiracEquation has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracEquation`Private`"]

HoldDOT::usage="";
tmp::usage="";
deqVerbose::usage="";

Options[DiracEquation] = {
	FCI 		-> False,
	FCE 		-> False,
	FCVerbose 	-> False
};

DiracEquation[a_ == b_, opts:OptionsPattern[]] :=
	DiracEquation[a,opts] == DiracEquation[b,opts];

DiracEquation[expr_List, opts:OptionsPattern[]]:=
	DiracEquation[#, opts]&/@expr;

DiracEquation[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, res, time},

		If [OptionValue[FCVerbose]===False,
			deqVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				deqVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"DiracEquation: Entering.", FCDoControl->deqVerbose];
		FCPrint[3,"DiracEquation: Entering with: ", expr, FCDoControl->deqVerbose];

		If[	!OptionValue[FCI],
			ex  = FCI[expr],
			ex  = expr
		];

		(* If there are no spinors or no Dirac matrices, then we can't apply the Dirac equation	*)
		If[	FreeQ2[ex, {Spinor,DiracGamma}],
			Return[ex];
		];

		time=AbsoluteTime[];
		FCPrint[1, "DiracEquation: Checking the spinor syntax.", FCDoControl->deqVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[ex]=!=True,
			Message[DiracEquation::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"DiracEquation: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->deqVerbose];



		res  = DotSimplify[FixedPoint[diraceq,ex,5]/.PairContract->Pair, Expanding->False];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"DiracEquation: Leaving.", FCDoControl->deqVerbose];
		FCPrint[3,"DiracEquation: Leaving with: ", res, FCDoControl->deqVerbose];

		res

	];

last[_. Momentum[pe__]] :=
	Momentum[pe];
last[x_Plus] :=
	PowerExpand[Sqrt[Last[x]^2]];
diraceq[x_] :=
	x/;FreeQ[x,Spinor];
diraceq[x_] :=
	Expand[x/. DOT->HoldDOT //. spCDieqRules /.HoldDOT->DOT, DOT ];

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
		]
		)/; last[n Momentum[p,dim]+k]===Momentum[p,dim],
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
					DiracGamma[Momentum[p_,dim_ : 4],dim_ : 4],b___] :> ( tmp =
					-HoldDOT[ z,s,a, DiracGamma[Momentum[p,dim],dim],DiracGamma[x,di],b] +
					2*((PairContract[x,Momentum[p,dim]]*HoldDOT[ z,s,a,b]) )
					)/;
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
						2*((PairContract[x,Momentum[p,dim]]*HoldDOT[a,b,s,z]) ))/;
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
