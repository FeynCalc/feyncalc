(* ::Package:: *)



(* :Title: NPointTo4Point													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Reduction of IR finite scalar 5-point functions to
				scalar 4-point functions according to arXiv:0709.1075	*)

(* ------------------------------------------------------------------------ *)

NPointTo4Point::usage =
"NPointTo4Point[expr, q] reduces scalar IR finite 5-point functions to scalar
4-point functions according to Eq. 4.52 in
[arXiv:0709.1075](https://arxiv.org/abs/0709.1075).";

NPointTo4Point::failmsg = "Error! NPointTo4Point has encountered a fatal problem and \
must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`NPointTo4Point`Private`"]

nptfpVerbose::usage="";
isoName::usage="";
dim::usage="";

Options[NPointTo4Point]  = {
	Dimension 		-> D,
	FCE 			-> False,
	FCVerbose		-> False,
	Factoring 		-> Factor2,
	IsolateNames	-> False,
	List 			-> False
};

SetAttributes[matrixElement, Orderless];

smn[_] :=
	0;

smdaemon[x_] :=
	x /. SmallVariable -> smn;

(* single Y_ij*)
matrixElement[{pi_, mi_}, {pj_, mj_}] :=
	Expand[smdaemon[mi^2 + mj^2 - ExpandScalarProduct[Pair[Momentum[pi - pj, dim],Momentum[pi - pj, dim]]]]];

getDet[mli_List,pli_List,sps_,fad_, termHold_, detFunc_, factFunc_, OptionsPattern[]]:=
	Block[{pl = Prepend[pli, 0], n = Length[mli], Y, augmY, prefac},

		(* create a list of the columns of eq. (4.54) *)
		Y = Table[matrixElement[{pl[[i]], mli[[i]]}, {pl[[j]], mli[[j]]}], {j,1,n}, {i,1,n}];

		If[	!SymmetricMatrixQ[Y],
			Message[NPointTo4Point::failmsg, "Y-matrix is not symmetric."];
			Abort[]
		];

		(* Y-matrix augmented with a row of (1,1,1,1,1) *)
		augmY = Map[Prepend[#, 1] &, Y];

		FCPrint[3,"NPointTo4Point: getDet: Y-matrix: ", Y, FCDoControl->nptfpVerbose];
		FCPrint[3,"NPointTo4Point: getDet: augmented Y-matrix: ", augmY, FCDoControl->nptfpVerbose];

		(* Computing T_0^5 according to Eq. 4.52, working through the first row. *)
		prefac = 1/termHold[detFunc[Y]];
		{prefac, sps Sum[(-1)^i termHold[factFunc[detFunc[Drop[augmY, None, {i+1,i+1}]]]] Drop[fad, {i, i}], {i, 1, Length[fad]}]}
	];

subdethold[x_] :=
	Isolate[x, IsolateNames -> isoName];

NPointTo4Point[expr_Plus,q_,opts:OptionsPattern[]]:=
	Map[NPointTo4Point[#,q,opts]&,expr];

NPointTo4Point[pref_. fad_FeynAmpDenominator,qu_, OptionsPattern[]]:=
	Block[{pp, res, tim,pl,ml, termHold,tmp, factoring},


		If[	!FreeQ2[{pref fad}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[	OptionValue[FCVerbose]===False,
			nptfpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				nptfpVerbose=OptionValue[FCVerbose]
			];
		];

		If[ Length[List@@fad]=!=5 || !FreeQ[pref,qu],
			Return[pref fad]
		];

		FCPrint[0,"NPointTo4Point: The reduction of the scalar 5-point function ", fad ,"is valid only if \
all the IR divergences were explicitly regularized with fictitious masses!", FCDoControl->nptfpVerbose];

		isoName = OptionValue[IsolateNames];
		dim = OptionValue[Dimension];

		If[	OptionValue[Factoring]===False,
			factoring->Identity,
			factoring = OptionValue[Factoring]
		];

		If[ isoName===False,
			termHold = Identity,
			termHold = subdethold;
		];

		FCPrint[3,"NPointTo4Point: Entering with: ", pref fad, FCDoControl->nptfpVerbose];

		pp[a_, (*b*)_] := Expand[(a /. m_Momentum :> m[[1]]) - qu];

		pl = Rest[fad /. FeynAmpDenominator->List /. PropagatorDenominator -> pp];

		If[	FCGramDeterminant[pl]===0,
			FCPrint[1,"NPointTo4Point: Zero Gram determinant encountered. Returning unevaluated result.", FCDoControl->nptfpVerbose];
			Return[pref fad]
		];

		FCPrint[3,"NPointTo4Point: pl: ", pl, FCDoControl->nptfpVerbose];

		ml = fad /. FeynAmpDenominator->List /. pd_PropagatorDenominator :> pd[[2]];
		FCPrint[3,"NPointTo4Point: ml: ", ml, FCDoControl->nptfpVerbose];

		(*Extract the determinant*)
		tmp = FCUseCache[getDet,{ml,pl,pref,fad, termHold, Det, factoring}];

		If[	OptionValue[List],
			res = tmp,
			res = tmp[[1]] tmp[[2]]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3,"NPointTo4Point: res: ", res, FCDoControl->nptfpVerbose];

		res

	];

NPointTo4Point[pref_. fad_FeynAmpDenominator,_, OptionsPattern[]]:=
	(pref fad)/; Length[List@@fad]<5;

NPointTo4Point[expr_,_, OptionsPattern[]]:=
	expr/; FreeQ[expr,FeynAmpDenominator];

FCPrint[1,"NPointTo4Point.m loaded."];
End[]
