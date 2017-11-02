(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCReplaceMomentum												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Replaces the given momenta with other momenta				*)

(* ------------------------------------------------------------------------ *)

FCReplaceMomentum::usage =
"FCReplaceMomentum[expr,rule] replaces the given momentum according to the \
specified replacement rule. Various options can be used to customize the replacement \
procedure.";

FCReplaceMomentum::repfail=
"Error! Failed to replace all occurences of `1` with `2`. Evaluation aborted.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCReplaceMomentum`Private`"]

fcrdVerbose::usage="";

Options[FCReplaceMomentum] = {
	Dimensions -> All,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	MomentumExpand -> False
};

FCReplaceMomentum[expr_, Rule[initMom_, finMom_], OptionsPattern[]] :=
	Block[{ex,vectorSet,res,check, scalarTerm, vectorTerm=1, pref=1, tmp,
		scaleless1=0,scaleless2=0,ruleProtect,holddim,diga, pasi},

		FCPrint[1,"FCReplaceMomentum: Entering.", FCDoControl->fcrmVerbose];
		FCPrint[3,"FCReplaceMomentum: Entering with ", expr, FCDoControl->fcrmVerbose];

		If [OptionValue[FCVerbose]===False,
			fcrmVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcrmVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];




		tmp = ex /. DiracGamma -> diga /. PauliSigma -> pasi;
		tmp = tmp //. {	diga[a_,di_] :> diga[holddim[a,ToString[di,InputForm]]],
						pasi[a_,di_] :> pasi[holddim[a,ToString[di,InputForm]]]};

		FCPrint[1,"FCReplaceMomentum: tmp: " ,tmp, FCDoControl->fcrmVerbose];

		tmp = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_,di_] :> h[holddim[a,ToString[di,InputForm]]];



		FCPrint[1,"FCReplaceMomentum: tmp: " ,tmp, FCDoControl->fcrmVerbose];

		If[	!FreeQ[Cases2[tmp,{ExplicitLorentzIndex,LorentzIndex,Momentum,CartesianIndex,CartesianMomentum,DiracGamma,PauliSigma}],dim],
			Message[FCReplaceMomentum::checkfail];
			Abort[]
		];

		tmp = tmp //. {dim -> x};

		If[	!FreeQ[tmp,dim],
			Message[FCReplaceMomentum::repfail,dim,x];
			Abort[]
		];

		res = tmp //. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[holddim[a_,str_String]]:> h[a,ToExpression[str]];
		res = res /. (h:LorentzIndex|ExplicitLorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_]/;Head[a]=!=holddim :> h[a];

		res = res /. diga[holddim[a_,str_String]] :> DiracGamma[a,ToExpression[str]];
		res = res /. diga[a_]/;Head[a]=!=holddim :> DiracGamma[a];

		res = res /. pasi[holddim[a_,str_String]] :> PauliSigma[a,ToExpression[str]];
		res = res /. pasi[a_]/;Head[a]=!=holddim :> PauliSigma[a];

		If[	!FreeQ2[res,{holddim,diga,pasi}],
			Message[FCReplaceMomentum::resfail];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCReplaceMomentum.m loaded."];
End[]
