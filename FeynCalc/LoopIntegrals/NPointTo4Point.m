(* ::Package:: *)



(* :Title: NPointTo4Point													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Reduction of IR finite N point functions to
				4 point functions											*)

(* ------------------------------------------------------------------------ *)

NPointTo4Point::usage =
"NPointTo4Point[expr, {q1, q2}] translates loop integrals \
.";

NPointTo4Point::failmsg = "Error! NPointTo4Point has encountered a fatal problem and \
must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`NPointTo4Point`Private`"]

nptfpVerbose=Null;
isoName = Null;
dim=Null;

Options[NPointTo4Point]  = {
	Dimension -> D,
	FCVerbose->False,
	IsolateNames -> False,
	FCE -> False,
	List -> False
};

SetAttributes[matrixElement, Orderless];

smn[_] :=
	0;

smdaemon[x_] :=
	x /. SmallVariable -> smn;

matrixElement[{pi_, mi_}, {pj_, mj_}] :=
	Expand[smdaemon[mi^2 + mj^2 - ExpandScalarProduct[Pair[Momentum[pi - pj, dim],Momentum[pi - pj, dim]]]]];


(*pli is the list of momenta (without q),being one less than mli*)
gr454[mli_List, pli_List] :=
	Block[{pl = Prepend[pli, 0], n = Length[mli], columnli},
		(* create a list of the columns of eq. (4.54) *)
		columnli = Prepend[Table[ matrixElement[{pl[[i]], mli[[i]]}, {pl[[j]], mli[[j]]}], {j,1,n}, {i,1,n}], Array[#^0&, n]];
		columnli
	];


(*define this such that sgr[{...},{...}, -1  ] is the Y_{ij} determinant *)
subDet[mli_, pli_, i_, OptionsPattern[]] :=
	Factor2[Det[Drop[gr454[mli, pli], {i+2,i+2}]]];

getDet[ml_List,pl_List,sps_,fad_, termHold_, OptionsPattern[]]:=
	{1/termHold[subDet[ml, pl, -1]], sps Sum[(-1)^j termHold[subDet[ml, pl, j]] Drop[fad, {j + 1, j + 1}], {j, 0, Length[fad] - 1}]};

subdethold[x_] :=
	Isolate[x, IsolateNames -> isoName];

NPointTo4Point[expr_Plus,q_,opts:OptionsPattern[]]:=
	Map[NPointTo4Point[#,q,opts]&,expr];

NPointTo4Point[sps_. fad_FeynAmpDenominator,qu_, OptionsPattern[]]:=
	Block[{pp, res, tim,pl,ml, termHold,tmp},

		If[	OptionValue[FCVerbose]===False,
			nptfpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				nptfpVerbose=OptionValue[FCVerbose]
			];
		];

		isoName = OptionValue[IsolateNames];
		dim = OptionValue[Dimension];

		If[ isoName===False,
			termHold = Identity,
			termHold = subdethold;
		];

		FCPrint[3,"NPointTo4Point: Entering with: ", sps fad, FCDoControl->nptfpVerbose];

		pp[a_, b_] := Expand[(a /. m_Momentum :> m[[1]]) - qu];

		pl = Rest[List@@(fad /. PropagatorDenominator -> pp)];

		If[	FCGramDeterminant[pl]===0,
			FCPrint[1,"NPointTo4Point: Zero Gram determinant encountered. Returning unevaluated result.", FCDoControl->nptfpVerbose];
			Return[sps fad]
		];

		FCPrint[3,"NPointTo4Point: pl: ", pl, FCDoControl->nptfpVerbose];

		ml = List @@ (fad /. pd_PropagatorDenominator :> pd[[2]]);
		FCPrint[3,"NPointTo4Point: ml: ", ml, FCDoControl->nptfpVerbose];

		(*Extract the determinant*)
		tmp = FCUseCache[getDet,{ml,pl,sps,fad, termHold}];

		If[	OptionValue[List],
			res = tmp,
			res = tmp[[1]] tmp[[2]]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3,"NPointTo4Point: res: ", res, FCDoControl->nptfpVerbose];

		res

	]/; Length[List@@fad]>=5;

NPointTo4Point[sps_. fad_FeynAmpDenominator,_, OptionsPattern[]]:=
	(sps fad)/; Length[List@@fad]<5;

NPointTo4Point[expr_,_, OptionsPattern[]]:=
	expr/; FreeQ[expr,FeynAmpDenominator];

FCPrint[1,"NPointTo4Point.m loaded."];
End[]
