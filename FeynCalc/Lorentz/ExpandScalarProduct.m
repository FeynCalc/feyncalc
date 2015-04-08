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

ExpandScalarProduct[x_, op___?OptionQ] :=
	Block[ {nx = x, pali},
	(* RM: put the next line in front 20110408 *)
		If[ (FCI /. {op} /. Options[ExpandScalarProduct]),
			nx = FCI[nx]
		];
		If[ FreeQ[nx,Pair],
			nx,
(* this algorithm is much quicher on bigger expressions, maybe there should be a
	switch for smaller ones to not use this?
	Changed Nov 2003, RM
 *)
			pali = Select[Cases2[nx, Pair], !FreeQ[#, LorentzIndex|Momentum]&];
		(* Print["pali = ", pali//InputForm]; *)
			If[ pali =!= {},
				nx = nx /. Dispatch[Thread[pali -> oldExpandScalarProduct[pali]]]
			];
			nx
		]
	];

oldExpandScalarProduct[x_,___Rule] :=
(*
If[(FCI /. {ru} /. Options[ExpandScalarProduct]),
	FixedPoint[pairexpand1,FCI[x], 3]//MomentumExpandexpand,
*)
	FixedPoint[pairexpand1, x, 3]//MomentumExpand;
(*
	];
*)

ExpandScalarProduct[x_, y_ /;Head[y] =!= Rule] :=
	scev[x, y];

(* Catch Pair[LorentzIndex[mu], Momentum[a] + Momentum [b] +...].
	F.Orellana. 26/2-2003 *)
(* simplify slightly, R. Merig, Sept. 13th 2003 *)
extraMomRule = Pair[LorentzIndex[a__],
							b : Plus[_. Momentum[__], _. Momentum[__]...]]  :>
(*
							b : Plus[(___*Momentum[__] | Momentum[__]),
												(___*Momentum[__] | Momentum[__]) ...]]  :>
*)
							(Pair[LorentzIndex[a], #]& /@ b);

pairexpand1[x_] :=
	x /. Pair->scevdoit /. extraMomRule;


(* not always a good idea (IFPD)
scev[x_,y_]:= MemSet[ scev[x,y], scevdoit[x,y] ];
*)
scev = scevdoit;
scevdoit[x_,y_] :=
	Distribute[ sceins@@
											 ( Expand[ MomentumExpand/@{x,y} ] )
										 ]/.sceins->sczwei/.
										 sczwei(*->PairContract/.PairContract*)->Pair;

sceins[0,_] :=
	0;                               (*sceinsdef*)
sceins[a_LorentzIndex b_, c_] :=
	b sceins[a, c];
sceins[a_Momentum b_, c_] :=
	b sceins[a, c];
sczwei[ _[_],_[_,_Symbol-4] ] :=
	0;             (*sczweidef*)
sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
	sczwei[v[x,di-4],w[y,di-4]];
sczwei[ w_[y_,_Symbol],v_[x_] ] :=
	sczwei[ v[x],w[y] ];

FCPrint[1,"ExpandScalarProduct.m loaded."];
End[]
