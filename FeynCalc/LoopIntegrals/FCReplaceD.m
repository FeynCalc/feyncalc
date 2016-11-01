(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCReplaceD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Replaces D by the given expression (e.g. 4-2*Epsilon)
				without changing D that specifies the dimension of scalar
				products and Dirac matrices.								*)

(* ------------------------------------------------------------------------ *)

FCReplaceD::usage =
"FCReplaceD[expr,rule] replaces D in expr accoding to the supplied replacement \
rule." <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCReplaceD"], StandardForm];

FCReplaceD::checkfail=
"Error! Failed to save the dimensionality of Pair and DiracGamma objects. Evaluation aborted.";

FCReplaceD::resfail=
"Error! Failed to restore the dimensionality of Pair and DiracGamma objects. Evaluation aborted.";

FCReplaceD::repfail=
"Error! Failed to replace all occurences of `1` with `2`. Evaluation aborted.";

FCReplaceD::error="
Error! Something went wrong while partial fractioning the loop integral `1` by FCReplaceD. \
Evaluation aborted";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

fcrdVerbose::usage="";

Begin["`FCReplaceD`Private`"]

Options[FCReplaceD] = {
	FCI -> False,
	FCVerbose -> False
};

FCReplaceD[expr_, replacement_Rule, OptionsPattern[]] :=
	Block[{ex,vectorSet,res,check, scalarTerm, vectorTerm=1, pref=1, tmp,
		scaleless1=0,scaleless2=0,ruleProtect,holddim,dim,diga},

		dim=First[replacement];
		FCPrint[1,"FCReplaceD: dim: " ,dim, FCDoControl->fcrdVerbose];

		If [OptionValue[FCVerbose]===False,
			fcrdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcrdVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		tmp = ex /. DiracGamma -> diga;
		tmp = tmp //. diga[a_,di_] :> diga[holddim[a,ToString[di,InputForm]]];

		FCPrint[1,"FCReplaceD: tmp: " ,tmp, FCDoControl->fcrdVerbose];

		tmp = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum)[a_,di_] :> h[holddim[a,ToString[di,InputForm]]];



		FCPrint[1,"FCReplaceD: tmp: " ,tmp, FCDoControl->fcrdVerbose];
		Global`XXX = tmp;
		If[	!FreeQ[Cases2[tmp,{ExplicitLorentzIndex,LorentzIndex,Momentum,DiracGamma}],dim],
			Message[FCReplaceD::checkfail];
			Abort[]
		];

		tmp = tmp //. replacement;

		If[	!FreeQ[tmp,dim],
			Message[FCReplaceD::repfail,dim,Last[replacement]];
			Abort[]
		];

		res = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum)[holddim[a_,str_String]]:> h[a,ToExpression[str]];

		res = res /. diga[holddim[a_,str_String]] :> DiracGamma[a,ToExpression[str]];

		If[	!FreeQ[res,holddim],
			Message[FCReplaceD::resfail];
			Abort[]
		];

		res
	];

FCPrint[1,"FCReplaceD.m loaded."];
End[]
