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
	FCE -> False,
	FCI -> False,
	FCVerbose -> False
};

FCReplaceD[expr_, Rule[dim_Symbol, x_], OptionsPattern[]] :=
	Block[{ex,vectorSet,res,check, scalarTerm, vectorTerm=1, pref=1, tmp,
		scaleless1=0,scaleless2=0,ruleProtect,holddim,diga, pasi},

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

		tmp = ex /. DiracGamma -> diga /. PauliSigma -> pasi;
		tmp = tmp //. {	diga[a_,di_] :> diga[holddim[a,ToString[di,InputForm]]],
						pasi[a_,di_] :> pasi[holddim[a,ToString[di,InputForm]]]};

		FCPrint[1,"FCReplaceD: tmp: " ,tmp, FCDoControl->fcrdVerbose];

		tmp = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_,di_] :> h[holddim[a,ToString[di,InputForm]]];



		FCPrint[1,"FCReplaceD: tmp: " ,tmp, FCDoControl->fcrdVerbose];

		If[	!FreeQ[Cases2[tmp,{ExplicitLorentzIndex,LorentzIndex,Momentum,CartesianIndex,CartesianMomentum,DiracGamma,PauliSigma}],dim],
			Message[FCReplaceD::checkfail];
			Abort[]
		];

		tmp = tmp //. {dim -> x};

		If[	!FreeQ[tmp,dim],
			Message[FCReplaceD::repfail,dim,x];
			Abort[]
		];

		res = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[holddim[a_,str_String]]:> h[a,ToExpression[str]];
		res = res /. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_]/;Head[a]=!=holddim :> h[a];

		res = res /. diga[holddim[a_,str_String]] :> DiracGamma[a,ToExpression[str]];
		res = res /. diga[a_]/;Head[a]=!=holddim :> DiracGamma[a];

		res = res /. pasi[holddim[a_,str_String]] :> PauliSigma[a,ToExpression[str]];
		res = res /. pasi[a_]/;Head[a]=!=holddim :> PauliSigma[a];

		If[	!FreeQ2[res,{holddim,diga,pasi}],
			Message[FCReplaceD::resfail];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCReplaceD.m loaded."];
End[]
