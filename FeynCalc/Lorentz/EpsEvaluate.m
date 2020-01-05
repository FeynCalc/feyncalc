(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsEvaluate														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary: Simplification of epsilon tensors								*)

(* ------------------------------------------------------------------------ *)

EpsEvaluate::usage =
"EpsEvaluate[expr] applies total antisymmetry and \
linearity (w.r.t. Momentum's) to all Levi-Civita tensors (Eps's) in expr .";

EpsEvaluate::failmsg =
"Error! EpsEvaluate has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`EpsEvaluate`Private`"]

Options[EpsEvaluate] = {
	FCE			-> False,
	FCI 		-> False,
	Momentum	-> All
};

EpsEvaluate[expr_, OptionsPattern[]]:=
	Block[{ex, momList, uniqList, rud, repRule, null1, null2, res},

		momList = OptionValue[Momentum];

		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		(*	Nothing to do...	*)
		If[	FreeQ[ex,Eps],
			Return[ex]
		];


		(* List of all the unique Epsilon tensors	*)
		uniqList = Cases[ex+null1+null2,_Eps,Infinity]//DeleteDuplicates//Sort;

		(*	If the user specified to perform expansion only for some
			special momenta, let's do it	*)
		If[ momList=!=All && Head[momList]===List,
			uniqList = Select[uniqList, !FreeQ2[#, momList]&]
		];

		(* List of the expanded epsilons	*)

		repRule = Thread[rud[uniqList, uniqList/.Eps->epsEval]];
		repRule = repRule /. rud[a_, a_] :> Unevaluated@Sequence[] /. rud->RuleDelayed;

		(* Simple cross check	*)
		If[ !FreeQ2[repRule,{epsEval,epsEvalLinearity,epsEvalAntiSymm}],
			Message[EpsEvaluate::failmsg, "Some expressions could not be evaluated."];
			Abort[]
		];

		res = ex /. Dispatch[repRule];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

epsEval[a_,b_,c__] :=
	(Expand/@(Distribute[DOT[a,b,c]//MomentumExpand]))/.
	DOT->epsEvalLinearity/.epsEvalLinearity->epsEvalAntiSymm/.epsEvalAntiSymm -> epsEvalAntiSymm2 /. epsEvalAntiSymm2 -> Eps


epsEvalLinearity[a___,b_ (c : (LorentzIndex | ExplicitLorentzIndex | Momentum | CartesianIndex | CartesianMomentum | TemporalMomentum)[__]),d___] :=
	b epsEvalLinearity[a,c,d];

epsEvalLinearity[___,0,___] :=
	0;

epsEvalAntiSymm[x__] :=
	0/; Signature[{x}]===0 && MemberQ[{3,4},Length[{x}]];

epsEvalAntiSymm[x__] :=
	Signature[{x}] epsEvalAntiSymm@@Sort[{x}] /; !OrderedQ[{x}] && MemberQ[{3,4},Length[{x}]];

epsEvalAntiSymm2[x___, ExplicitLorentzIndex[0], y__] :=
	(-1)^Length[{y}]*epsEvalAntiSymm2[x,y,ExplicitLorentzIndex[0]];

FCPrint[1,"EpsEvaluate.m loaded."];
End[]
