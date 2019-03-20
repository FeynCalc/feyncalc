(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ComplexConjugate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 2:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: construct the complex conjugate amplitude,
			introducing complex conjugated indices automatically
*)

(* :Comments: ComplexConjugate does NOT work if complex
				quantities are in denominators!!!!!!!!!!!!!!!
*)

(* ------------------------------------------------------------------------ *)

ComplexConjugate::usage =
"ComplexConjugate[expr] complex conjugates expr. It operates on \
Fermion-lines, i.e., products of Spinor[..] .DiracMatrix[..] . Spinor[..]. \
For taking the spin sum (i.e. constructing the traces) use FermionSpinSum. \n \n
WARNING: In expr should be NO explicit I in denominators!";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ComplexConjugate`Private`"]

dotsim::usage="";

rev[yz__] :=
	(DOT @@ (Reverse[FRH[{ yz }]])) /; Length[Position[{yz}, Spinor]] < 3;

c$CCfrh /: HoldForm[c$CCfrh[ii_]] := c$CC[ii];

conpa[x__] :=
	conpa[x] = Pair[x]

(* for large expressions it is better to not use DotSimplify *)
Options[ComplexConjugate] = {
	Conjugate -> {},
	DotSimplify -> True,
	FCE -> False,
	FCI -> False,
	FCRenameDummyIndices -> True
};

ComplexConjugate[expr_, OptionsPattern[]]:=
	Block[{ex,res,conjugate, ruleConjugate,ru},

		conjugate=OptionValue[Conjugate];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		dotsim = OptionValue[DotSimplify];

		If[ Head[ex]===HoldForm && FreeQ2[ex, {DOT,LorentzIndex,SUNIndex,SUNTF,DiracGamma,PauliSigma,Complex}],
			Return[ex]
		];

		res = compcon[ex/.SUNTrace->suntrac]/. SUNDelta -> SUNDeltaContract /. compcon -> compcon2 /.
		compcon2 -> ComplexConjugate /. suntrac-> SUNTrace /. SUNDeltaContract->SUNDelta;

		If[	OptionValue[FCRenameDummyIndices],
			res = FCRenameDummyIndices[res]
		];

		If[	conjugate=!={} && Head[conjugate]===List,
			ruleConjugate= Thread[ru[conjugate,Conjugate/@conjugate]]/. ru->Rule;
			res = res /. ruleConjugate
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

compcon2[x_/;!FreeQ[x, HoldForm]] :=
	compcon[FRH[x]];

compcon[x_^n_?(Element[#,Reals]===True)&] :=
	compcon[x]^n;

compcon[x_Plus] :=
	compcon /@ x;

compcon[x_Times] :=
	compcon /@ x;

compcon[b_HoldForm] :=
	b /; FreeQ2[FRH[b], {DOT,LorentzIndex,SUNIndex,SUNTF,Complex}];

compcon[x:Except[_Plus | _Times]] :=
	Block[ {nx = x,oone, suntrac},

		If[ FreeQ2[nx, {DOT,Complex,DiracGamma,PauliSigma,SUNTF}],
			Return[nx]
		];

		If[ !FreeQ[nx, SUNF],
			nx = Expand[nx, SUNF]
		];
		If[ !FreeQ[nx,SUNT],
			If[ dotsim,
				nx = DotSimplify[nx, Expanding -> False]
			]
		];
		(* this is wrong if nx had Head List ... (change 02/99)
				nx = (DOT[oone, nx] /. DOT -> rev /. rev -> DOT); *)
		If[ !FreeQ[nx, SUNTF],
			nx = nx /.{SUNTF[{a__},b_,c_]:> SUNTF[Reverse[{a}], c, b] }
		];

		If[ !FreeQ[nx, Eps],
			nx = nx /. Eps[a__] :> Conjugate[$LeviCivitaSign]/$LeviCivitaSign Eps[a]
		];

		nx = nx /.
			DiracGamma[a__]:> DOT[oone, DiracGamma[a]]/; FreeQ[{5,6,7},Evaluate[{a}[[1]]]] /.
			DiracGamma[5]->(-DiracGamma[5]) /.
			{DiracGamma[6] :> DiracGamma[7], DiracGamma[7]:>DiracGamma[6]} /.
			DOT -> rev /. rev -> DOT /. oone -> 1;

		(* 	CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
			variables are in denominators!!!!, (which is the case in HEP, unless you
			have width in the propagators ...) *)

		nx = nx /. Complex[a_, b_] -> Complex[a, -b];

		If[ dotsim,
			nx = DotSimplify[nx, Expanding -> False]
		];

		nx
	]/; FreeQ[x, HoldForm];


FCPrint[1,"ComplexConjugate.m loaded."];
End[]
