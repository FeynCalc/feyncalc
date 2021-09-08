(* ::Package:: *)



(* :Title: ToPaVe                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts scalar 1-loop integrals to Passarino Veltman
				scalar functions 											*)

(* ------------------------------------------------------------------------ *)


ToPaVe::usage =
"ToPaVe[exp, q]  converts all scalar 1-loop integrals in exp that depend on the
momentum q to scalar Passarino Veltman functions A0, B0, C0, D0 etc.";

OtherLoopMomenta::usage =
"OtherLoopMomenta is an option of ToPaVe. It takes a list of loop momenta other
than q that appear in the expression. Knowing about these momenta prevents
ToPaVe from erroneously converting multi-loop integrals into PaVe scalar
functions.

This is of course relevant only for multi-loop calculations. At 1-loop you
don't need to specify this option explicitly.";

ToPaVe::failmsg =
"Error! ToPaVe has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

momentumRoutingDenner;


Begin["`ToPaVe`Private`"]

optPaVeOrder::usage="";
genpave::usage="";

Options[ToPaVe] = {
	FCE					-> False,
	FCI					-> False,
	GenPaVe				-> False,
	OtherLoopMomenta	-> {},
	PaVeAutoOrder		-> True,
	PaVeAutoReduce		-> True,
	PaVeOrder			-> True,
	PaVeToABCD			-> True
};

ToPaVe[expr_, q_, OptionsPattern[]] :=
	Block[{	ex, loopInt, irrel, rel, repList, res, loopList,
			optOtherLoopMomenta, loopListEval},

		If [!FreeQ[$ScalarProducts, q],
			Message[ToPaVe::failmsg, "The loop momentum " <> ToString[q,InputForm] <> " has scalar product rules attached to it."];
			Abort[]
		];

		genpave 			= OptionValue[GenPaVe];
		optPaVeOrder 		= OptionValue[PaVeOrder];
		optOtherLoopMomenta = OptionValue[OtherLoopMomenta];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		ex = FCLoopSplit[ex,{q}, FCI->True];
		irrel = ex[[1]]+ex[[3]]+ex[[4]];
		rel = ex[[2]];
		rel = FCLoopIsolate[rel,{q},Head->loopInt, FCI->True, GFAD->False, CFAD->False, SFAD->True, PaVe->False];

		loopList = Cases2[rel,loopInt];

		loopListEval = FCLoopPropagatorPowersExpand[#,FCI->True]&/@(loopList/.loopInt->Identity);

		loopListEval = Map[
				If[	FreeQ2[#,optOtherLoopMomenta],
					toPaVe[#,q,OptionValue[PaVeAutoOrder],OptionValue[PaVeAutoReduce]],
					#
				]&,loopListEval];

		(* Not all SFADs can be converted to PaVe functions! *)
		loopListEval = loopListEval /. toPaVe[z_,_,_,_]/;!FreeQ[z,StandardPropagatorDenominator] :> z;

		If[ OptionValue[PaVeToABCD],
			loopListEval = PaVeToABCD[loopListEval]
		];

		repList = Thread[Rule[loopList,loopListEval]];

		res = (rel/. Dispatch[repList]) + irrel;

		If[	!FreeQ[res,toPaVe],
			Message[ToPaVe::failmsg,"Not all 1-loop scalar integrals could be converted to PaVe functions. Please apply FDS to the input and try again."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];


(* Determine kinematic invariants according to the conventions of Denner, c.f. arXiv:1604.06792 *)

momentumRoutingDenner[moms_List, fu_, OptionsPattern[]] :=
	Block[{firstLines, lastLine, kmax = (Length[moms] + 1)/2, res, p, repRule},
			repRule = Thread[Rule[Table[p[i], {i, 1, 2 kmax - 1}], moms]];
			firstLines = Transpose[Table[(p[k + l] - p[l])//fu, {l, 0, 2 kmax - 1}, {k, 1, kmax - 1}]];
			lastLine = Table[(p[kmax + l] - p[l])//fu, {l, 0, kmax - 1}];
			res = Join[Flatten[firstLines], lastLine] //. {p[2 kmax] -> p[0], p[x_] /; x > 2 kmax :> p[x - 2 kmax], p[0] -> 0};
			If[Length[res] =!= ((kmax - 1)*2 kmax + kmax),
				Message[ToPaVe::failmsg, "Wrong number of the kinematic invariants!"];
				Abort[]
			];
			(res /. Dispatch[repRule])
		]/; OddQ[(Length[moms])];

momentumRoutingDenner[moms_List, fu_, OptionsPattern[]] :=
	Block[{firstLines, lastLine, kmax = (Length[moms])/2, res, p, repRule},
			repRule = Thread[Rule[Table[p[i], {i, 1, 2 kmax}], moms]];
			res = Transpose[Table[(p[k + l] - p[l])//fu, {l, 0, 2 kmax}, {k, 1, kmax}]];
			res = Flatten[res //. {p[2 kmax + 1] -> p[0], p[x_] /; (x > 2 kmax + 1) :> p[x - 2 kmax - 1], p[0] -> 0}];
			If[Length[res] =!= (kmax*(2 kmax + 1)),
				Message[ToPaVe::failmsg, "Wrong number of the kinematic invariants!"];
				Abort[]
			];
			(res /. Dispatch[repRule])
		]/; EvenQ[(Length[moms])];

toPaVe[x_,_,_,_]:=
	x/; !FreeQ2[Head[x],PaVeHeadsList];

(* The conventions are according to Appendix A of arXiv:1604.06792 *)

toPaVe[FeynAmpDenominator[PD[Momentum[q_, dim_], m1_], re:PD[Momentum[q_, dim_] + _ : 0, _] ...],q_,paveao_,pavear_]:=
	Block[{tmp,res,pair},
		If[ {re} === {},
			tmp = {{},{}},
			tmp = Transpose[Cases[{re}, PD[Momentum[q, dim] + x_: 0, m_: 0] :> {x, m}]];
		];
		If[ Length[tmp[[1]]]=!=Length[{re}] || Length[tmp[[2]]]=!=Length[{re}],
			Message[ToPaVe::failmsg, "toPave: Wrong number of the kinematic invariants!"];
			Abort[]
		];
		res = I Pi^2 PaVe[0, ExpandScalarProduct[(momentumRoutingDenner[tmp[[1]],pair[#,#]&]/.pair->Pair)],
			Power[#, 2] & /@ Join[{m1},tmp[[2]]], PaVeAutoOrder->paveao, PaVeAutoReduce->pavear];

		If[ optPaVeOrder,
			res = PaVeOrder[res]
		];

		res
	]/;!genpave;

toPaVe[FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q_, dim_], 0, mm1_, {1,1}],
	re:StandardPropagatorDenominator[Momentum[q_, dim_] + _ : 0, 0, _, {1,1}] ...],q_,paveao_,pavear_]:=
	Block[{tmp,res,pair},
		If[ {re} === {},
			tmp = {{},{}},
			tmp = Transpose[Cases[{re}, StandardPropagatorDenominator[Momentum[q, dim] + x_: 0,0, m_: 0, {1,1}] :> {x, -m}]];
		];
		If[ Length[tmp[[1]]]=!=Length[{re}] || Length[tmp[[2]]]=!=Length[{re}],
			Message[ToPaVe::failmsg, "toPave: Wrong number of the kinematic invariants!"];
			Abort[]
		];
		res = I Pi^2 PaVe[0, ExpandScalarProduct[(momentumRoutingDenner[tmp[[1]],pair[#,#]&]/.pair->Pair)],
			Join[{-mm1},tmp[[2]]], PaVeAutoOrder->paveao, PaVeAutoReduce->pavear];

		If[ optPaVeOrder,
			res = PaVeOrder[res]
		];

		res
	]/;!genpave;

toPaVe[FeynAmpDenominator[(prs:PD[Momentum[_,_]+_:0,_]..)], q_,_,_]:=
	I Pi^2 GenPaVe[{0},	((MomentumExpand[prs])/. PD[Momentum[q, _:4] +p_:0, m_:0] :> {p, m})]/;genpave;

FCPrint[1,"ToPaVe.m loaded."];
End[]
