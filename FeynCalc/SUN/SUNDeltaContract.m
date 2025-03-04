(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNDeltaContract													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Kronecker delta for SU(N) in the adjoint representation   	*)

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

SUNDeltaContract[expr_] :=
	(expr //. SUNDelta -> SUNDeltaContract /. SUNDeltaContract -> SUNDelta)/; !FreeQ[expr,SUNDelta];

SUNDeltaContract[x_, y_ ] :=
	SUNDeltaContract[SUNIndex[x], SUNIndex[y]]/;
		FreeQ2[{x}, {SUNIndex, ExplicitSUNIndex, Pattern}] || FreeQ2[{y}, {SUNIndex, ExplicitSUNIndex, Pattern}];

SUNDeltaContract[expr_] :=
	expr/;FreeQ2[expr,{SUNT,SUNIndex,ExplicitSUNIndex,SUNDelta,Pattern,Sequence,BlankSequence,BlankNullSequence}];

SUNDeltaContract[x_SUNIndex, x_SUNIndex] :=
	(SUNN^2 - 1);

SUNDeltaContract /:
	SUNDeltaContract[j_ExplicitSUNIndex, _SUNIndex]^2 :=
		SUNDeltaContract[j, j];

SUNDeltaContract /:
	SUNDeltaContract[i_SUNIndex, j_SUNIndex]^2 :=
		(SUNN^2 - 1) /; (i =!= j);

SUNDeltaContract/:
	SUNDeltaContract[i_SUNIndex, j_] SUNDeltaContract[i_SUNIndex, k_ ] :=
		SUNDeltaContract[j,k];

SUNDeltaContract/:
	SUNDeltaContract[i_SUNIndex, j_SUNIndex ] y_[z__] :=
		(
		( y[z] /. {i -> j} )
		)/; FreeQ[y[z], _FeynArts`SumOver] && !FreeQ[y[z]//Hold, i] && FreeQ[y[z], SUNDeltaContract[__]^n_Integer?Negative];

FCPrint[1,"SUNDeltaContract.m loaded"];
End[]
