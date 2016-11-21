(* ::Package:: *)



(* :Title: ToPaVe                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Converts scalar 1-loop integrals to Passarino Veltman
				scalar functions 											*)

(* ------------------------------------------------------------------------ *)


ToPaVe::usage = "ToPaVe[expr,q] converts all the scalar 1-loop integrals that \
depend on the momentum q to scalar Passarino Veltman functions
A0, B0, C0, D0 etc.";

OtherLoopMomenta::usage = "OtherLoopMomenta is an option of ToPaVe. It takes \
a list of loop momenta other than q that appear in the expression. Knowing \
about these momenta prevents ToPaVe from erroneously converting multiloop \
integrals into PaVe scalar functions. This is of course relevant only for \
multiloop calculations. For 1-loop you don't need to specify this option \
explicitly.";

ToPaVe::failmsg =
"Error! ToPaVe has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToPaVe`Private`"]

genpave::usage="";

Options[ToPaVe] = {
	GenPaVe->False,
	OtherLoopMomenta -> {},
	PaVeAutoOrder -> True,
	PaVeAutoReduce -> True
};

ToPaVe[expr_, q_, OptionsPattern[]] :=
	Block[{ex,loopInt,irrel,rel,repList,res},


		If [!FreeQ[$ScalarProducts, q],
			Message[ToPaVe::failmsg, "The loop momentum " <> ToString[q,InputForm] <>
					" has scalar product rules attached to it."];
			Abort[]
		];

		genpave = OptionValue[GenPaVe];

		ex = FCLoopSplit[expr,{q}];
		irrel = ex[[1]]+ex[[3]]+ex[[4]];
		rel = ex[[2]];
		rel = FCLoopIsolate[rel,{q},Head->loopInt];

		repList =
			Union[Cases[{rel},  loopInt[x_] :> Rule[loopInt[x],
				If[	FreeQ2[x,OptionValue[OtherLoopMomenta]],
					toPaVe[x,q,OptionValue[PaVeAutoOrder],OptionValue[PaVeAutoReduce]],
					x
				]
			], Infinity]];
		res = (rel/.repList) + irrel;

		If[	!FreeQ[res,toPaVe],
			Message[ToPaVe::failmsg,"Not all 1-loop scalar integrals could be converted to PaVe functions. Please apply FDS to the input and try again."];
			Abort[]
		];

		res

	];

toPaVe[x_,_,_,_]:=
	x/; !FreeQ2[Head[x],PaVeHeadsList];

(* A0 *)
toPaVe[FeynAmpDenominator[PD[Momentum[q_,_],m_]],q_,paveao_,pavear_]:=
	I Pi^2 PaVe[0,{},{m^2}, PaVeAutoOrder->paveao, PaVeAutoReduce->pavear]/; !genpave;

(* B0 *)
toPaVe[FeynAmpDenominator[PD[Momentum[q_,dim_],m1_],
	PD[Momentum[q_,dim_]+p_:0,m2_]],q_,paveao_,pavear_]:=
		I Pi^2 PaVeOrder[PaVe[0,{ExpandScalarProduct[Pair[p,p]]},{m1^2,m2^2},
			PaVeAutoOrder->paveao,
			PaVeAutoReduce->pavear]]/; !genpave;

(* C0 *)
toPaVe[	FeynAmpDenominator[
			PD[Momentum[q_,dim_],m1_],
			PD[Momentum[q_,dim_]+p1_:0,m2_],
			PD[Momentum[q_,dim_]+p2_:0,m3_]],q_,
			paveao_,pavear_]:=
	I Pi^2 PaVeOrder[PaVe[0,{ExpandScalarProduct[Pair[p1,p1]],
		ExpandScalarProduct[Pair[p1-p2,p1-p2]],
		ExpandScalarProduct[Pair[p2,p2]]},{m1^2,m2^2,m3^2},
			PaVeAutoOrder->paveao,
			PaVeAutoReduce->pavear]]/; !genpave;

(* D0 *)
toPaVe[	FeynAmpDenominator[
			PD[Momentum[q_,dim_],m1_],
			PD[Momentum[q_,dim_]+p1_:0,m2_],
			PD[Momentum[q_,dim_]+p2_:0,m3_],
			PD[Momentum[q_,dim_]+p3_:0,m4_]],q_,
			paveao_,pavear_
	]:=
	I Pi^2 PaVeOrder[PaVe[0,
			{ExpandScalarProduct[Pair[p1,p1]],
			ExpandScalarProduct[Pair[p1-p2,p1-p2]],
			ExpandScalarProduct[Pair[p2-p3,p2-p3]],
			ExpandScalarProduct[Pair[p3,p3]],
			ExpandScalarProduct[Pair[p2,p2]],
			ExpandScalarProduct[Pair[p1-p3,p1-p3]]}
			,{m1^2,m2^2,m3^2,m4^2},
			PaVeAutoOrder->paveao,
			PaVeAutoReduce->pavear]]/; !genpave;


(* E0 *)
toPaVe[	FeynAmpDenominator[
			PD[Momentum[q_,dim_],m1_],
			PD[Momentum[q_,dim_]+p1_:0,m2_],
			PD[Momentum[q_,dim_]+p2_:0,m3_],
			PD[Momentum[q_,dim_]+p3_:0,m4_],
			PD[Momentum[q_,dim_]+p4_:0,m5_]],q_,
			paveao_,pavear_
	]:=
	I Pi^2 PaVeOrder[PaVe[0,
			{ExpandScalarProduct[Pair[p1,p1]],
			ExpandScalarProduct[Pair[p1-p2,p1-p2]],
			ExpandScalarProduct[Pair[p2-p3,p2-p3]],
			ExpandScalarProduct[Pair[p3-p4,p3-p4]],
			ExpandScalarProduct[Pair[p2,p2]],
			ExpandScalarProduct[Pair[p1-p3,p1-p3]],
			ExpandScalarProduct[Pair[p2-p4,p2-p4]],
			ExpandScalarProduct[Pair[p4,p4]],
			ExpandScalarProduct[Pair[p1-p4,p1-p4]]}
			,{m1^2,m2^2,m3^2,m4^2,m5^2},
			PaVeAutoOrder->paveao,
			PaVeAutoReduce->pavear]]/; !genpave;


(* Even Higher multiplicities*)
toPaVe[	FeynAmpDenominator[
			(prs:PD[Momentum[_,_:4]+_:0,_]..)],q_,
			_,_
	]:=
	I Pi^2 GenPaVe[{0},	((MomentumExpand/@{prs})/.
			PD[Momentum[q, _:4] +p_:0, m_:0] :> {p, m})]/;
			Length[{prs}]>5 || genpave;


FCPrint[1,"ToPaVe.m loaded."];
End[]
