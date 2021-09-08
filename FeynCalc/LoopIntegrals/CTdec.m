(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CTdec															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Computes tensor decompositions for Cartesian multiloop
				integrals using projection methods			 				*)

(* ------------------------------------------------------------------------ *)

CTdec::usage =
"CTdec[{{qi, a}, {qj, b}, ...}, {p1, p2, ...}] or CTdec[exp, {{qi, a}, {qj, b},
...}, {p1, p2, ...}] calculates the tensorial decomposition formulas for
Cartesian integrals. The more common ones are saved in TIDL.";

CTdec::failmsg =
"Error! TID has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`CTdec`Private`"]

ctdecVerbose::usage="";

Options[CTdec] =	{
	BasisOnly 			-> False,
	Dimension 			-> D-1,
	FCVerbose 			-> False,
	Factoring 			-> {Factor2, Factor},
	FeynCalcExternal	-> True,
	List 				-> True,
	Parallelize	-> True,
	UseTIDL				-> True
};


CTdec[exp_:1, li : {{_, _} ..}, ppli_List/;FreeQ[ppli,OptionQ], OptionsPattern[]] :=
	Block[ {resTdec, tmp, tmpPair, time, rules, dimD, optDimension, heads,
			headsEval, ruleHeads, optBasisOnly, res, optFactoring, momHead},

		optDimension = OptionValue[Dimension];
		optBasisOnly = OptionValue[BasisOnly];
		optFactoring = OptionValue[Factoring];

		If [OptionValue[FCVerbose]===False,
			ctdecVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				ctdecVerbose=OptionValue[FCVerbose]
			]
		];

		FCPrint[1, "CTdec: Entering with ", exp, li, ppli, "" , FCDoControl->ctdecVerbose];

		(*
			We implement CTdec as a frontend for Tdec. The construction of the basis for
			Cartesian integrals proceeds exactly in the same way as for Minkowskian integrals,
			so there is no need to reinvent the wheel. The only thing one has to check in advance
			are possible vanishing Gram determinants.
		*)

		(* Abort decomposition if there are vanishing Gram determinants, unless
		we have a 1-point function or were requested just to provide the tensor basis *)
		If[!optBasisOnly && ppli=!={},
			FCPrint[1, "CTdec: Checking Gram determinant...", FCDoControl->ctdecVerbose];
			If[	FCGramDeterminant[ppli,Dimension->optDimension,Head -> {CartesianPair, CartesianMomentum}]===0,
				FCPrint[1, "CTdec: Tensor decomposition is not possible due to vanishing Gram determinants", FCDoControl->ctdecVerbose];
				tmp=Apply[Times, Map[CartesianPair[CartesianMomentum[#[[1]],optDimension], CartesianIndex[#[[2]],optDimension]]&, li]];
				rules={};

				If[ exp =!= 1,
					tmp = Contract[exp tmp, EpsContract -> False]
				];

				If[	TrueQ[OptionValue[List]],
					res = {rules, tmp},
					res = tmp
				];

				If[ OptionValue[FCE],
					res = FCE[res]
				];

				Return[res],
				FCPrint[1, "CTdec: The Gram determinant is non-vanishing.", FCDoControl->ctdecVerbose];
			];
		];

		time = AbsolutTime[];
		FCPrint[1, "CTdec: Calling Tdec.", FCDoControl->ctdecVerbose];
		resTdec = Tdec[exp,li,ppli, BasisOnly->optBasisOnly, Dimension->dimD, Factoring->OptionValue[Factoring],
				List->True, FCE->False, Parallelize->OptionValue[Parallelize], UseTIDL-> OptionValue[UseTIDL],
				Head -> momHead];
		FCPrint[1, "CTdec: Done calling Tdec, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ctdecVerbose];

		resTdec = resTdec /. Pair -> tmpPair //. {Momentum-> CartesianMomentum, LorentzIndex -> CartesianIndex, dimD -> optDimension} /.
			tmpPair -> CartesianPair /. momHead -> Identity;

		If[	!FreeQ2[resTdec,{Pair,LorentzIndex,Momentum}],
			Message[CTdec::failmsg, "Failed to convert the output of Tdec to Cartesian objects"]
		];

		If[	optBasisOnly,
			res = resTdec /. FCGV["PaVe"] -> FCGV["CartesianPaVe"];
			If[ exp =!= 1,
				res = Contract[exp res, EpsContract -> False]
			];
			If[ OptionValue[FCE],
				res = FCE[res]
			];
			Return[res]
		];

		{rules, tmp} = resTdec;

		If[	Head[rules]===List && Length[rules]>0,
			heads = Cases2[tmp, CartesianMomentum, CartesianIndex];
			headsEval = Map[(#/. rules)&, heads];
			ruleHeads = Thread[Rule[heads,headsEval]];
			tmp = tmp /. ruleHeads;
		];

		If[ exp =!= 1,
			tmp = Contract[exp tmp, EpsContract -> False]
		];

		If[	TrueQ[OptionValue[List]],
			res = {rules, tmp},
			If[	Head[rules]===List && Length[rules]>0,
				res = tmp /. rules,
				res = tmp
			]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "CTdec: Leaving.",  FCDoControl->ctdecVerbose];
		FCPrint[3, "CTdec: Leaving with: ", res,  FCDoControl->ctdecVerbose];

		res

	];

FCPrint[1, "CTdec.m loaded.", FCDoControl->ctdecVerbose];
End[]
