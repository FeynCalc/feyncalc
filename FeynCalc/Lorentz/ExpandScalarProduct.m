(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExpandScalarProduct expands scalar products *)

(* ------------------------------------------------------------------------ *)

ExpandScalarProduct::usage =
"ExpandScalarProduct[expr]  expands scalar products of sums of
momenta in expr.
ExpandScalarProduct[x, y] expands ScalarProduct[x, y], where
x and y may contain sums. ExpandScalarProduct does not use Expand on
expr.";

ScalarProductExpand::usage =
"ScalarProductExpand[expr]  expands scalar products of sums of
momenta in expr.
ScalarProductExpand[a, b] expands ScalarProduct[a, b].
ScalarProductExpand is equivalent to ExpandScalarProduct.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ExpandScalarProduct`Private`"]

ScalarProductExpand = ExpandScalarProduct;

Options[ExpandScalarProduct] = {FCI -> True};

(* since one never can remember this function  ...*)
(* well, not used, so commented out. F.Orellana, 8/11-2002. *)
(*FRH = FixedPoint[ReleaseHold, #]&;*)

ExpandScalarProduct[x_, OptionsPattern[]] :=
	Block[ {nx = x, pali},

		If[ OptionValue[FCI],
			nx = FCI[nx]
		];

		If[ FreeQ[nx,Pair],
			Return[nx]
		];


		(* this algorithm is much quicher on bigger expressions, maybe there should be a
		switch for smaller ones to not use this?
		Changed Nov 2003, RM
		 *)

		pali = Select[Cases2[nx, Pair], !FreeQ[#, LorentzIndex|Momentum]&];

		If[ pali =!= {},
			nx = nx /. Dispatch[Thread[pali -> oldExpandScalarProduct[pali]]]
		];

		nx
	];

oldExpandScalarProduct[x_,___Rule] :=
	FixedPoint[pairexpand1, x, 3]//MomentumExpand;

ExpandScalarProduct[x_, y_ /;Head[y] =!= Rule] :=
	scevdoit[x, y];

(* Catch Pair[LorentzIndex[mu], Momentum[a] + Momentum [b] +...]. *)
extraMomRule =
	Pair[LorentzIndex[a__], b : Plus[_. Momentum[__], _. Momentum[__]...]]  :> (Pair[LorentzIndex[a], #]& /@ b);

pairexpand1[x_] :=
	x /. Pair->scevdoit /. extraMomRule;

scevdoit[x_,y_] :=
	Distribute[sceins@@(Expand[ MomentumExpand/@{x,y} ])]/.
	sceins->sczwei/.sczwei->Pair;

sceins[0,_] :=
	0;

sceins[a_LorentzIndex b_, c_] :=
	b sceins[a, c];

sceins[a_Momentum b_, c_] :=
	b sceins[a, c];

sczwei[ _[_],_[_,_Symbol-4] ] :=
	0;

sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
	sczwei[v[x,di-4],w[y,di-4]];

sczwei[ w_[y_,_Symbol],v_[x_] ] :=
	sczwei[ v[x],w[y] ];

FCPrint[1,"ExpandScalarProduct.m loaded."];
End[]
