(* ::Package:: *)



(* :Title: PaVeUVPart                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts UV divergent parts of Passarino-Veltman
				coefficient functions 										*)

(* ------------------------------------------------------------------------ *)


PaVeUVPart::usage =
"PaVeUVPart[expr] replaces all occurring Passarino-Veltman functions by their
explicit values, where only the UV divergent part is preserved, while possible
IR divergences and the finite part are discarded. The function uses the
algorithm from [arXiv:hep-ph/0609282](https://arxiv.org/abs/hep-ph/0609282) by
G. Sulyok. This allows to treat Passarino-Veltman of arbitrary rank and
multiplicity";

PaVeUVPart::failmsg =
"Error! PaVeUVPart has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PaVeUVPart`Private`"]

MM::usage="";
PP::usage="";
pvuvVerbose::usage="";
dMinus4::usage="";
factoring::usage="";
uvp::usage="";
prefactor::usage="";

Options[PaVeUVPart] = {
	Dimension 		-> D,
	FCE 			-> False,
	FCI				-> False,
	FCLoopExtract 	-> True,
	FCVerbose 		-> False,
	Factoring 		-> True,
	Prefactor 		-> 1,
	ToPaVe2 		-> True,
	Together 		-> True
};

PaVeUVPart[expr_,  OptionsPattern[]] :=
	Block[{	ex,repList,res, dummy, rest,loopInts,intsUnique, intsUniqueEval,
			paVeInt, repRule, dMinus4, prodsUnique, prodsUniqueEval, dim},

		dim 		= OptionValue[Dimension];
		factoring	= OptionValue[Factoring];
		prefactor	= OptionValue[Prefactor];

		Switch[ factoring,
				False,
					factoring = Identity,
				True,
					factoring = Factor2,
				_,
					True
		];

		If [OptionValue[FCVerbose]===False,
			pvuvVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				pvuvVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PaVeUVPart: Entering.",  FCDoControl->pvuvVerbose];
		FCPrint[3, "PaVeUVPart: Entering with: ",  expr, FCDoControl->pvuvVerbose];

		If[ FreeQ2[expr,FeynCalc`Package`PaVeHeadsList],
			Return[expr]
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];


		If[	OptionValue[FCLoopExtract],

			(* Normal mode *)
			(* Here we need to handle the case (D-4)*PaVe explicitly *)
			FCPrint[1, "PaVeUVPart: Normal mode.", FCDoControl->pvuvVerbose];

			ex = ex /. dim -> dMinus4 + 4;

			{rest,loopInts,prodsUnique} = FCLoopExtract[ex,{dummy},paVeInt,PaVe->True, FCI->True, PaVeIntegralHeads -> Join[FeynCalc`Package`PaVeHeadsList,{dMinus4}]];


			(*Do not touch terms that are not multiplied by PaVe functions (e.g. counter-term contributions) *)
			loopInts = loopInts /. paVeInt[x_]/; FreeQ2[x,FeynCalc`Package`PaVeHeadsList] :> x;
			prodsUnique = SelectNotFree[prodsUnique,FeynCalc`Package`PaVeHeadsList];

			FCPrint[3, "PaVeUVPart: List of the unique integrals multiplied by D: ",  prodsUnique, FCDoControl->pvuvVerbose];

			intsUnique = Cases2[prodsUnique,FeynCalc`Package`PaVeHeadsList];
			FCPrint[3, "PaVeUVPart: List of the unique integrals: ",  intsUnique, FCDoControl->pvuvVerbose];

			If[	OptionValue[ToPaVe2],
				intsUniqueEval = ToPaVe2/@((intsUnique/.paVeInt->Identity));
				FCPrint[3, "PaVeUVPart: List of the unique integrals after ToPaVe2: ",  intsUniqueEval, FCDoControl->pvuvVerbose],

				intsUniqueEval = ((intsUnique/.paVeInt->Identity));
			];

			intsUniqueEval = intsUniqueEval  /. convRule /. uvp->uvpEval;
			FCPrint[3, "PaVeUVPart: List of the evaluated PaVe functions ",  intsUniqueEval, FCDoControl->pvuvVerbose];

			prodsUniqueEval = prodsUnique /. Dispatch[Thread[Rule[intsUnique,intsUniqueEval]]] /. paVeInt->Identity;

			FCPrint[3, "PaVeUVPart: List of the evaluated products: ",  prodsUniqueEval, FCDoControl->pvuvVerbose];

			If[	!FreeQ2[prodsUniqueEval,FeynCalc`Package`PaVeHeadsList],
				Message[PaVeUVPart::failmsg,"Something went wrong during the evaluation of the PaVe functions."];
				Abort[]
			];

			prodsUniqueEval = Map[If[TrueQ[(Factor[Denominator[Together[#]]/.dMinus4->0])=!=0],0,#]&, prodsUniqueEval];

			FCPrint[3, "PaVeUVPart: After sorting out (D-4)*PaVe cases: ",  intsUniqueEval, FCDoControl->pvuvVerbose];

			repRule = Thread[Rule[prodsUnique,prodsUniqueEval]];
			FCPrint[3, "PaVeUVPart: Replacement rule ",  repRule, FCDoControl->pvuvVerbose];

			res = rest + (loopInts /. repRule),

			(* Fast mode *)
			(* Here we assume that the input is a single PaVe function, so that the case (D-4)*PaVe cannot occur *)
			FCPrint[1, "PaVeUVPart: Fast mode.", FCDoControl->pvuvVerbose];

			If[	OptionValue[ToPaVe2],
				ex = ToPaVe2[ex]
			];

			res = ex /. convRule /. uvp->uvpEval
		];

		res = res /. dMinus4 -> dim - 4;

		If[	OptionValue[Together],
			res = Together[res]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "PaVeUVPart: Leaving.", FCDoControl->pvuvVerbose];
		FCPrint[3, "PaVeUVPart: Leaving with: ", res, FCDoControl->pvuvVerbose];

		res

	];


convRule = {
	PaVe[inds__?NumberQ, moms_List, masses_List, OptionsPattern[]] :>
		uvp[UVPartT[Length[masses]][inds], Thread[Rule[momentumRoutingDenner2[Length[masses] - 1], moms]],
		Thread[Rule[Table[MM[i], {i, 0, Length[masses] - 1}], masses]]]
};

uvpEval[exp_, ru1_, ru2_] :=
	prefactor/dMinus4 factoring[exp /. ru1 /. ru2]


SetAttributes[PP, Orderless];

momentumRoutingDenner2[nn_] :=
	MemSet[momentumRoutingDenner2[nn],
		Block[{firstLines, lastLine, kmax = (nn + 1)/2, res},
			firstLines = Transpose[Table[(PP[k + l, l]), {l, 0, 2 kmax - 1}, {k, 1, kmax - 1}]];
			lastLine = Table[(PP[kmax + l, l]), {l, 0, kmax - 1}];
			res = Join[Flatten[firstLines], lastLine] //. {
				PP[2 kmax, y_] :> PP[0, y], PP[x_, y_] /; x > 2 kmax :> PP[x - 2 kmax, y], PP[0] -> 0};

			res = Replace[res, 0 -> Unevaluated@Sequence[], 1];

			If[	Length[res] =!= ((kmax - 1)*2 kmax + kmax),
				Message[PaVeUVPart::failmsg, "Wrong number of the kinematic invariants!"];
				Abort[]
			];
			res
		]
	] /; OddQ[nn];


momentumRoutingDenner2[nn_] :=
	MemSet[momentumRoutingDenner2[nn],
		Block[{firstLines, lastLine, kmax = nn/2, res, p},
			res = Transpose[Table[(PP[k + l, l]), {l, 0, 2 kmax}, {k, 1, kmax}]];

			res = Flatten[res //. {
				PP[2 kmax + 1, y_] :> PP[0, y], PP[x_, y_] /; (x > 2 kmax + 1) :> PP[x - 2 kmax - 1, y], PP[0] -> 0}];

			res = Replace[res, 0 -> Unevaluated@Sequence[], 1];

			If[	Length[res] =!= (kmax*(2 kmax + 1)),
				Message[PaVeUVPart::failmsg, "Wrong number of the kinematic invariants!"];
				Abort[]
			];
			res
		]
	] /; EvenQ[nn];

(* This is a slightly modified version of G. Sulyok's original UVPartT from the Mathematica notebook shipped with arXiv:hep-ph/0609282 *)

UVPartT[1][k___] :=
	MemSet[UVPartT[1][k],
		-(MM[0]^(1 + Quotient[Count[{k}, 0],2])/(2^(Quotient[Count[{k}, 0],2] - 1) (Quotient[Count[{k}, 0],2] + 1)!))
	];

UVPartT[N_Integer?Positive /; N >= 2][k___] :=
	MemSet[UVPartT[N][k],
		Block[{indexanzahl, a, indexlist={}, ii, jj, nom, denom, res, range, tensIndCountNoZero, tensIndCountZero,
			pref, indexNumber, llow, lup, ll, tmp},

			indexNumber = N/2 (N + 1);
			tensIndCountNoZero = Count[{k}, Except[0]];
			tensIndCountZero = Count[{k}, 0];

			tmp = Table[ll[ii], {ii, 1, indexNumber - 1}];
			llow = Join[{(Quotient[tensIndCountZero, 2] - N + 2)}, tmp];
			lup = Join[tmp, {0}];

			With[{zzz = Sequence @@ Delete[{lup[[#]], 0, llow[[#]]} & /@ Range[indexNumber], {-1}]},
				Do[indexlist = Insert[indexlist, llow - lup, 1], zzz]];

			a[PN_] :=
				Join[PP[#, 0] & /@ Range[PN - 1], Flatten[Table[(PP[ii, 0] + PP[jj, 0] - PP[ii, jj]), {jj, 1, PN - 1}, {ii, jj + 1, PN - 1}]],
					-(PP[#, 0] - MM[#] + MM[0]) & /@ Range[PN - 1], {MM[0]}];

			range = Range[Length[indexlist]];

			nom = (Product[
				(Sum[indexlist[[#, (i/2) (2 N - 3 - i) + j]], {i, 1, j - 1}] +
				Sum[indexlist[[#, (j/2) (2 N - 1 - j) + i]], {i, 1, N - 1 - j}] +
				2 indexlist[[#, j]] + indexlist[[#, (N/2) (N - 1) + j]] + Count[{k}, j])!, {j, 1, N - 1}] &) /@ range;

			denom = (
				(2 Sum[indexlist[[#, i]], {i, 1, (N/2) (N - 1)}] +
				Sum[indexlist[[#, i]], {i, (N/2) (N - 1) + 1, (N/2) (N + 1) - 1}] +
				(N - 1) + tensIndCountNoZero)! &
			) /@ range;

			pref = (-1)^(1 + tensIndCountNoZero)/(2^(Quotient[tensIndCountZero, 2] - 1) (Quotient[tensIndCountZero, 2] - N + 2)!);

			res = pref Plus @@ ((Multinomial[Sequence @@ indexlist[[#]]] & /@ range) ((Times @@ (a[N]^indexlist[[#]])) & /@ range) nom/denom);

			res
		]
	];


FCPrint[1,"PaVeUVPart.m loaded."];
End[]
