(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFDeltaContract												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Kronecker delta for SU(N) in the fundamental representation   *)

(* ------------------------------------------------------------------------ *)

SUNFDeltaContract::usage=
"SUNFDeltaContract[exp] substitutes for all SUNFDelta in exp SUNFDeltaContract,
contracts the fundamental $\\text{SU}(N)$ indices and resubstitutes
SUNFDelta.SUNFDeltaContract[i, j] is the Kronecker-delta for $\\text{SU}(N)$ in
the fundamental representation with contraction properties. It wraps the head
SUNFIndex around its arguments.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SUNFDeltaContract`Private`"];

SetAttributes[SUNFDeltaContract, Orderless];


SUNFDeltaContract[expr_] :=
	expr/;FreeQ2[expr,{SUNTF,SUNFIndex,ExplicitSUNFIndex,SUNFDelta,Pattern,Sequence,BlankSequence,BlankNullSequence}];

SUNFDeltaContract[expr_] :=
	(expr //. SUNFDelta -> SUNFDeltaContract /. SUNFDeltaContract -> SUNFDelta)/; !FreeQ[expr,SUNFDelta];

SUNFDeltaContract[x_, y_ ] :=
	SUNFDeltaContract[SUNFIndex[x], SUNFIndex[y]]/;
		FreeQ2[{x}, {SUNFIndex, ExplicitSUNFIndex, Pattern}] || FreeQ2[{y}, {SUNFIndex, ExplicitSUNFIndex, Pattern}];

SUNFDeltaContract[x_SUNFIndex, x_SUNFIndex] :=
	SUNN;

SUNFDeltaContract /:
	SUNFDeltaContract[j_ExplicitSUNFIndex, _SUNFIndex]^2 :=
		SUNFDeltaContract[j, j];

SUNFDeltaContract /:
	SUNFDeltaContract[i_SUNFIndex, j_SUNFIndex]^2 :=
		SUNN /; (i =!= j);

SUNFDeltaContract/:
	SUNFDeltaContract[i_SUNFIndex, j_] SUNFDeltaContract[i_SUNFIndex, k_ ] :=
		SUNFDeltaContract[j,k];

SUNFDeltaContract/:
	SUNFDeltaContract[i_SUNFIndex, j_SUNFIndex ] y_[z__] :=
		( y[z] /. i -> j ) /; FreeQ[y[z], _FeynArts`SumOver] &&
				!FreeQ[y[z]//Hold, i] && FreeQ[y[z], SUNFDeltaContract[__]^n_Integer?Negative];

FCPrint[1,"SUNFDeltaContract.m loaded"];
End[]
