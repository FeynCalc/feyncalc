(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

SUNDeltaContract::usage=
"SUNDeltaContract[exp] substitutes for all SUNDelta in exp SUNDeltaContract,
contracts the adjoint $\\text{SU}(N)$ indices and resubstitutes SUNDelta.  
SUNDeltaContract[i, j] is the Kronecker-delta for $\\text{SU}(N)$ in the
adjoint representation with contraction properties. It wraps the head SUNIndex
around its arguments.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SUNDeltaContract`Private`"]

SetAttributes[SUNDeltaContract, Orderless];

(* Added check for integers - noint. F.Orellana, 11/1-2001 *)
noint[x___] :=
		Not[Or @@
				Join[IntegerQ /@ {x}, IntegerQ /@
	({x} /. {SUNIndex -> Identity, ExplicitSUNIndex -> Identity})]];

SUNDeltaContract[expr_] :=
	(expr //. SUNDelta -> SUNDeltaContract /. SUNDeltaContract ->
	SUNDelta)/; !FreeQ[expr,SUNDelta];

SUNDeltaContract[x_ /; FreeQ[x, SUNIndex] && !IntegerQ[x] &&
	FreeQ[x, ExplicitSUNIndex], y_ /; FreeQ[y, SUNIndex] &&
	!IntegerQ[y] &&	FreeQ[y, ExplicitSUNIndex]] :=
		SUNDeltaContract[SUNIndex[x], SUNIndex[y]];

SUNDeltaContract[x_SUNIndex, x_SUNIndex] :=
	(SUNN^2 - 1) /; noint[x];

SUNDeltaContract /:
	SUNDeltaContract[j_ExplicitSUNIndex, _SUNIndex]^2 :=
		SUNDeltaContract[ExplicitSUNIndex[j], ExplicitSUNIndex[j]];

SUNDeltaContract /: SUNDeltaContract[i_SUNIndex, j_SUNIndex]^2 :=
										(SUNN^2 - 1) /; (i =!= j) && noint[i,j];

SUNDeltaContract/:
	SUNDeltaContract[i_SUNIndex, j_] SUNDeltaContract[a_, i_SUNIndex ] :=
		SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/:
	SUNDeltaContract[a_, i_SUNIndex ] SUNDeltaContract[i_SUNIndex, j_] :=
		SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_] SUNDeltaContract[i_SUNIndex, k_] :=
		SUNDeltaContract[j,k] /; noint[i];

SUNDeltaContract/:
	SUNDeltaContract[a_, i_SUNIndex ] SUNDeltaContract[b_, i_SUNIndex ] :=
		SUNDeltaContract[a,b] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_SUNIndex ] y_[z__] :=
	( y[z] /. i -> j ) /; FreeQ[y[z], _FeynArts`SumOver] &&
	!FreeQ[y[z]//Hold, i] && FreeQ[y[z], SUNDeltaContract[__]^n_Integer?Negative] /; noint[i,j];

FCPrint[1,"SUNDeltaContract.m loaded"];
End[]
