(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsEvaluate														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Simplification of epsilon tensors								*)

(* ------------------------------------------------------------------------ *)

EpsEvaluate::usage =
"EpsEvaluate[expr] applies total antisymmetry and \
linearity (w.r.t. Momentum's) to all Levi-Civita tensors (Eps's) in expr .";

EpsEvaluate::fail =
"Something went while simplifying epsilon tensors. Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`EpsEvaluate`Private`"]

Options[EpsEvaluate] = {
	FCI -> False,
	Momentum -> All
	};

EpsEvaluate[expr_, OptionsPattern[]]:=
	Block[{x,momList,uniqList,rud,repRule,null1,null2},

		momList = OptionValue[Momentum];

		If[ !OptionValue[FCI],
			x = FCI[expr],
			x = expr
		];

		(*	Nothing to do...	*)
		If[	FreeQ[x,Eps],
			Return[x]
		];


		(* List of all the unique Epsilon tensors	*)
		uniqList = Cases[x+null1+null2,_Eps,Infinity]//DeleteDuplicates//Sort;

		(*	If the user specified to perform expansion only for some
			special momenta, let's do it	*)
		If[ momList=!=All && Head[momList]===List,
			uniqList = Select[uniqList, !FreeQ2[#, momList]&]
		];

		(* List of the expanded epsilons	*)

		repRule = Thread[rud[uniqList, uniqList/.Eps->epsEval]] /. rud->RuleDelayed;
		repRule = repRule /. RuleDelayed[a_, a] :> Unevaluated@Sequence[];

		(* Simple cross check	*)
		If[ !FreeQ2[repRule,{epsEval,epsEvalLinearity,epsEvalAntiSym}],
			Message[EpsEvaluate::fail];
			Abort[]
		];

		x/.Dispatch[repRule]

	];

epsEval[a_,b_,c__] :=
	(Expand/@(Distribute[DOT[a,b,c]//MomentumExpand]))/.
	DOT->epsEvalLinearity/.epsEvalLinearity->epsEvalAntiSymm/.epsEvalAntiSymm -> Eps


epsEvalLinearity[a___,b_ (c : (LorentzIndex | ExplicitLorentzIndex | Momentum | CartesianIndex | CartesianMomentum | TemporalIndex | TemporalMomentum)[__]),d___] :=
	b epsEvalLinearity[a,c,d];

epsEvalLinearity[___,0,___] :=
	0;

epsEvalAntiSymm[x__] :=
	0/; Signature[{x}]===0 && MemberQ[{3,4},Length[{x}]];

epsEvalAntiSymm[x__] :=
	Signature[{x}] epsEvalAntiSymm@@Sort[{x}] /; !OrderedQ[{x}] && MemberQ[{3,4},Length[{x}]];

FCPrint[1,"EpsEvaluate.m loaded."];
End[]
