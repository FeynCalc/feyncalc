(* ::Package:: *)



(* :Title: EpsChisholm														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Applies the Chisholm identity backwards						*)

(* ------------------------------------------------------------------------ *)

EpsChisholm::usage =
"EpsChisholm[expr] substitutes for a gamma matrix contracted with \
a Levi Civita tensor (Eps) the Chisholm identity.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`EpsChisholm`Private`"]

esVerbose::usage="";

Options[EpsChisholm] = {
	FCI -> False,
	FCVerbose -> False
};



EpsChisholm[expr_, OptionsPattern[]] :=
	Block[ {new = 0,ex,terms,rest,res},

		If [OptionValue[FCVerbose]===False,
			esVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				esVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "EpsChisholm: Entering EpsChisholm", FCDoControl->esVerbose];
		FCPrint[3, "EpsChisholm: Entering with, ", expr , FCDoControl->esVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,{DiracGamma,Eps}],
			Return[ex]
		];

		{rest, terms} = FCSplit[ex,{DiracGamma,Eps}];

		FCPrint[3, "EpsChisholm: Relevant part of the expression, ", terms , FCDoControl->esVerbose];
		FCPrint[3, "EpsChisholm: Irrelevant part of the expression, ", rest , FCDoControl->esVerbose];

		new = terms /. {Eps[a_,b_,c_,d_]->eeps[a,b,c,d]}//.eepsOrder;

		FCPrint[3, "EpsChisholm: After properly ordering eps tensors: ",
			new , FCDoControl->esVerbose];

		new = Expand2[new,{eeps,DiracGamma}];
		new = Expand2[new//.epsspcrule,{eeps,DiracGamma}];

		FCPrint[3, "EpsChisholm: After applying Chisholm identity backwards: ",
			new , FCDoControl->esVerbose];

		res = (new/.eeps->Eps) + rest;
		FCPrint[3, "EpsChisholm: Leaving with: ", res , FCDoControl->esVerbose];

		res
	]

(* change maybe later *)
SpinorChainEvaluate = DiracSimplify;
scev = ExpandScalarProduct;


eepsOrder = {m_. DOT[x___, DiracGamma[LorentzIndex[in_]], y___] *
	eeps[a___, LorentzIndex[in_], b__] :>
	(-1^Length[{b}]) m DOT[x, DiracGamma[LorentzIndex[in]], y] eeps[a, b, LorentzIndex[in]]};

epsspcrule = {
	(* Inside DOT *)
	m_. DOT[ x___,DiracGamma[LorentzIndex[in_]],y___] eeps[a_,b_,c_,LorentzIndex[in_]] :>
		( Conjugate[$LeviCivitaSign]* I m ( DOT[x,DiracGamma[a], DiracGamma[b], DiracGamma[c], DiracGamma[5],y] -
		scev[a,b] DOT[x,DiracGamma[c],DiracGamma[5],y] -scev[b,c] DOT[x,DiracGamma[a].DiracGamma[5],y] +
		scev[a,c] DOT[x,DiracGamma[b],DiracGamma[5],y])//SpinorChainEvaluate)//Expand2[#,{DiracGamma,eeps}]&,
	(* Standalone *)
	DiracGamma[LorentzIndex[in_]] eeps[a_,b_,c_,LorentzIndex[in_]] :>
		( Conjugate[$LeviCivitaSign]* I ( DOT[x,DiracGamma[a],DiracGamma[b],DiracGamma[c], DiracGamma[5],y] -
		scev[a,b] DOT[x,DiracGamma[c],DiracGamma[5],y] -
		scev[b,c] DOT[x,DiracGamma[a],DiracGamma[5],y] +
		scev[a,c] DOT[x,DiracGamma[b],DiracGamma[5],y])//SpinorChainEvaluate)//Expand2[#,{DiracGamma,eeps}]&
};

FCPrint[1,"EpsChisholm.m loaded."];
End[]
