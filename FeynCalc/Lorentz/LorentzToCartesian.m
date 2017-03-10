(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LorentzToCartesian												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Converts selected Lorentz tensors into Cartesian tensors.		*)

(* ------------------------------------------------------------------------ *)


LorentzToCartesian::usage=
"LorentzToCartesian[expr] rewrites Lorentz tensors in form of Cartesian tensors \
(when possible). Using options one can specify which types of tensors
should be converted.";

LorentzToCartesian::fail=
"Error! LorentzToCartesian has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`LorentzToCartesian`Private`"]

tmpci::usage="";
lorIndex::usage="";

Options[LorentzToCartesian] = {
	DiracGammaExpand -> True,
	DotSimplify -> True,
	EpsEvaluate -> True,
	ExpandScalarProduct -> True,
	FCE -> False,
	FCI -> False,
	FCTensor -> True,
	FV -> True,
	GA -> True,
	GS -> True,
	LC -> True,
	LorentzIndex -> True,
	SP -> True
};

LorentzToCartesian[expr_, OptionsPattern[]]:=
	Block[{ex, heads, tmp, tensorList,  res, uniqList,null1,null2, uniqListEval, repRule, times,dotTimes},

		heads = {};

		tensorList = Complement[$FCTensorList, {CPair, Pair, Eps}];

		lorIndex = OptionValue[LorentzIndex];


		If[	OptionValue[SP] || OptionValue[FV],
			heads = Join[heads,{Pair}]
		];

		If[	OptionValue[LC],
			heads = Join[heads,{Eps}]
		];

		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		uniqList = Cases[ex+null1+null2,Alternatives@@(Blank/@heads),Infinity]//DeleteDuplicates//Sort;

		uniqListEval = uniqList;

		If[	OptionValue[ExpandScalarProduct],
			uniqListEval = ExpandScalarProduct[#,FCI->True]&/@uniqListEval;
		];

		If[	OptionValue[SP],
			uniqListEval = uniqListEval /. Pair -> spToCsp /. spToCsp -> Pair
		];

		If[	OptionValue[FV],
			uniqListEval = uniqListEval /. Pair -> fvToCv /. fvToCv -> Pair
		];

		If[	OptionValue[LC],
			uniqListEval = uniqListEval /. Eps -> ltensorToCTensor /. ltensorToCTensor -> Eps;
			If[ OptionValue[EpsEvaluate],
				uniqListEval = EpsEvaluate[uniqListEval,FCI->True]
			];
		];

		repRule = MapThread[Rule[#1, #2] &, {uniqList, uniqListEval}];

		ex = ex/.repRule;

		(* 	For DiracGammas and other tensors we cannot avoid introducing dummy indices. So if these objects appear in powers, we need
			to make sure that we do not break Einstein summation.  *)
		If[	OptionValue[DiracGammaExpand],
			ex = DiracGammaExpand[ex,FCI->True];
		];

		If[	(OptionValue[GA] || OptionValue[GS]) &&  !FreeQ[ex,Power],
			ex = ex /. Power[z_, n_Integer?Positive]/;!FreeQ[z, DiracGamma] :> Apply[dotTimes, Table[z, {Abs[n]}]]^Sign[n]
		];

		If[	(OptionValue[FCTensor]) &&  !FreeQ[ex,Power],
			ex = ex /. Power[z_, n_Integer?Positive]/;!FreeQ2[z, tensorList] :> Apply[times, Table[z, {Abs[n]}]]^Sign[n]
		];


		If[	OptionValue[GA],
			ex = ex /. DiracGamma -> diracGammaToCDiracGamma /. diracGammaToCDiracGamma -> DiracGamma
		];

		If[	OptionValue[GS],
			ex = ex /. DiracGamma -> diracSlashToCDiracSlash /. diracSlashToCDiracSlash -> DiracGamma
		];

		If[	OptionValue[FCTensor],
			Scan[(ex = ex /. # -> ltensorToCTensor /. ltensorToCTensor -> #)&, tensorList]
		];

		res = ex/. times->Times /. dotTimes->DOT;

		If[	OptionValue[DotSimplify],
			res = DotSimplify[res,FCI->False]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

spToCsp[Momentum[a_], Momentum[b_]]:=
	FeynCalc`Package`MetricT TPair[TIndex[],TMomentum[a]] TPair[TIndex[],TMomentum[b]] +
	FeynCalc`Package`MetricS CPair[CMomentum[a],CMomentum[b]];

spToCsp[Momentum[a_, dim_Symbol], Momentum[b_, dim_Symbol]]:=
	FeynCalc`Package`MetricT TPair[TIndex[],TMomentum[a]] TPair[TIndex[],TMomentum[b]] +
	FeynCalc`Package`MetricS CPair[CMomentum[a,dim-1],CMomentum[b,dim-1]];

spToCsp[Momentum[a_, dim_Symbol-4], Momentum[b_, dim_Symbol-4]]:=
	FeynCalc`Package`MetricS CPair[CMomentum[a,dim-4],CMomentum[b,dim-4]];

fvToCv[LorentzIndex[a_], Momentum[b_]]:=
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], TIndex[]] Pair[TIndex[], Momentum[b]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a],CMomentum[b]];

fvToCv[LorentzIndex[a_, dim_Symbol], Momentum[b_, dim_Symbol]]:=
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], TIndex[]] Pair[TIndex[], Momentum[b]] +
	FeynCalc`Package`MetricS  Pair[LorentzIndex[a,dim],CMomentum[b,dim-1]];

fvToCv[LorentzIndex[a_, dim_Symbol-4], Momentum[b_, dim_Symbol-4]]:=
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim-4],CMomentum[b,dim-4]];

ltensorToCTensor[a___, LorentzIndex[b_], c___]:=
	(
	tmpci = CIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricT ltensorToCTensor[a,TIndex[],c] Pair[TIndex[], LorentzIndex[b]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[b],tmpci] ltensorToCTensor[a,tmpci,c]
	)/; lorIndex

ltensorToCTensor[a___, LorentzIndex[b_, dim_Symbol], c___]:=
	(
	tmpci = CIndex[$MU[Unique[]],dim-1];
	FeynCalc`Package`MetricT ltensorToCTensor[a,TIndex[],c] Pair[TIndex[], LorentzIndex[b,dim]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[b,dim],tmpci] ltensorToCTensor[a,tmpci,c]
	)/; lorIndex

ltensorToCTensor[a___, Momentum[b_], c___]:=
	FeynCalc`Package`MetricT ltensorToCTensor[a,TIndex[],c] Pair[TIndex[], Momentum[b]] +
	FeynCalc`Package`MetricS ltensorToCTensor[a,CMomentum[b],c];

ltensorToCTensor[a___, Momentum[b_, dim_Symbol], c___]:=
	FeynCalc`Package`MetricT ltensorToCTensor[a,TIndex[],c] Pair[TIndex[], Momentum[b]] +
	FeynCalc`Package`MetricS ltensorToCTensor[a,CMomentum[b,dim-1],c];


diracGammaToCDiracGamma[LorentzIndex[a_]]:=
	(
	tmpci= CIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], TIndex[]] DiracGamma[TIndex[]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a], tmpci] DiracGamma[tmpci]
	);

diracGammaToCDiracGamma[LorentzIndex[a_, dim_Symbol], dim_Symbol]:=
	(
	tmpci= CIndex[$MU[Unique[]],dim-1];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], TIndex[]] DiracGamma[TIndex[]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim], tmpci] DiracGamma[tmpci,dim]
	);

diracGammaToCDiracGamma[LorentzIndex[a_, dim_Symbol-4], dim_Symbol-4]:=
	(
	tmpci= CIndex[$MU[Unique[]],dim-4];
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim-4], tmpci] DiracGamma[tmpci,dim-4]
	);

diracSlashToCDiracSlash[Momentum[p_]]:=
	FeynCalc`Package`MetricT DiracGamma[TIndex[]] Pair[TIndex[], Momentum[p]] +
	FeynCalc`Package`MetricS DiracGamma[CMomentum[p]];

diracSlashToCDiracSlash[Momentum[p_, dim_Symbol], dim_Symbol]:=
	FeynCalc`Package`MetricT DiracGamma[TIndex[]] Pair[TIndex[], Momentum[p]] +
	FeynCalc`Package`MetricS DiracGamma[CMomentum[p,dim-1],dim];

diracSlashToCDiracSlash[Momentum[p_, dim_Symbol-4], dim_Symbol-4]:=
	FeynCalc`Package`MetricS DiracGamma[CMomentum[p,dim-4],dim-4];


powerExpand[ex_, head_, times_]:=
	ex /. Power[Pattern[z,Blank[head]], n_Integer?Positive] :>
		Apply[times, Table[z, {Abs[n]}]]^Sign[n]/; !FreeQ[ex,Power];

powerExpand[ex_, _, _, _]:=
	ex/; FreeQ[ex,Power];

FCPrint[1,"LorentzToCartesian.m loaded."];
End[]
