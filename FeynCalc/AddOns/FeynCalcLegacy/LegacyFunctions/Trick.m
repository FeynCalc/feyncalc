(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Trick does non-commutative expansion and simple contractions *)

(* ------------------------------------------------------------------------ *)

Trick::usage =
"Trick[exp] performs several basic simplifications without expansion.
Trick[exp] uses Contract, DotSimplify and SUNDeltaContract.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Trick`Private`"];

Trick[x_] :=
	Block[{tt, sigident,doot,cov},

		SetAttributes[cov,HoldFirst];
		cov[y_] :=  y /. CovariantD[a__] :>	CovariantD[a, Explicit -> True];

		tt = DotSimplify[FeynCalcInternal[x]//cov(*//palr*),
		Expanding -> False] /. SUNDelta -> SUNDeltaContract /. SUNDeltaContract -> SUNDelta;
		If[!FreeQ2[tt, {LorentzIndex,CartesianIndex}],
			tt = Contract[tt, EpsContract -> False, Expanding -> False]
		];
		If[!FreeQ[tt, SUNT],
			tt = (tt /. DOT -> doot) //. {doot[a___,b_ /; FreeQ[b,SUNT], c__SUNT, d___] :>
			doot[a,c,b,d]} /. {doot[a__SUNT, b__] :> (doot[a] doot[b]) /; FreeQ[{b},SUNIndex]} /. doot -> DOT
		];
		If[!FreeQ[tt, SUNF],
			tt = tt /. ( SUNF[a_,b_,c_] SUNF[d_,e_,f_] :> SUNSimplify[SUNF[a,b,c] SUNF[d,e,f]] ) /.
			SUNDelta->SUNDeltaContract /. SUNDeltaContract->SUNDelta
		];

		tt = FeynAmpDenominatorCombine[tt];
		tt
	];

FCPrint[1,"Trick.m loaded"];
End[]
