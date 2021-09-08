(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LorentzToCartesian												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Converts selected Lorentz tensors into Cartesian tensors.		*)

(* ------------------------------------------------------------------------ *)


LorentzToCartesian::usage=
"LorentzToCartesian[exp]  rewrites Lorentz tensors in form of Cartesian tensors
(when possible). Using options one can specify which types of tensors should
be converted.";

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
	DiracGammaExpand	-> True,
	DotSimplify 		-> True,
	EpsEvaluate			-> True,
	EpsExpand			-> True,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCTensor 			-> True,
	FV 					-> True,
	GA 					-> True,
	GS 					-> True,
	LC 					-> True,
	LorentzIndex 		-> True,
	PauliSigmaExpand 	-> True,
	SI 					-> True,
	SIS 				-> True,
	SP 					-> True
};

LorentzToCartesian[expr_, OptionsPattern[]]:=
	Block[{ex, heads, tmp, tensorList,  res, uniqList,null1,null2, uniqListEval, repRule, times,dotTimes},

		heads = {};

		tensorList = Complement[$FCTensorList, {CartesianPair, Pair, Eps}];

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
				uniqListEval = EpsEvaluate[uniqListEval,FCI->True, EpsExpand->OptionValue[EpsExpand]]
			];
		];

		repRule = Thread[Rule[uniqList, uniqListEval]];

		ex = ex /. Dispatch[repRule];

		(* 	For DiracGammas and other tensors we cannot avoid introducing dummy indices. So if these objects appear in powers, we need
			to make sure that we do not break Einstein summation.  *)
		If[	OptionValue[DiracGammaExpand],
			ex = DiracGammaExpand[ex,FCI->True];
		];

		If[	OptionValue[PauliSigmaExpand],
			ex = PauliSigmaExpand[ex,FCI->True];
		];

		If[	(OptionValue[GA] || OptionValue[GS]) &&  !FreeQ[ex,Power],
			ex = ex /. Power[z_, n_Integer?Positive]/;!FreeQ[z, DiracGamma] :> Apply[dotTimes, Table[z, {Abs[n]}]]^Sign[n]
		];

		If[	(OptionValue[SI] || OptionValue[SIS]) &&  !FreeQ[ex,Power],
			ex = ex /. Power[z_, n_Integer?Positive]/;!FreeQ[z, PauliSigma] :> Apply[dotTimes, Table[z, {Abs[n]}]]^Sign[n]
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

		If[	OptionValue[SI],
			ex = ex /. PauliSigma -> pauliSigmaToCPauliSigma /. pauliSigmaToCPauliSigma -> PauliSigma
		];

		If[	OptionValue[SIS],
			ex = ex /. PauliSigma -> pauliSlashToCPauliSlash /. pauliSlashToCPauliSlash -> PauliSigma
		];

		If[	OptionValue[FCTensor],
			Scan[(ex = ex /. # -> ltensorToCTensor /. ltensorToCTensor -> #)&, tensorList]
		];

		res = ex/. times->Times /. dotTimes->DOT;

		If[	OptionValue[DotSimplify],
			res = DotSimplify[res,FCI->True]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

spToCsp[Momentum[a_], Momentum[b_]]:=
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]] TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[b]] +
	FeynCalc`Package`MetricS CartesianPair[CartesianMomentum[a],CartesianMomentum[b]];

spToCsp[Momentum[a_, dim_Symbol], Momentum[b_, dim_Symbol]]:=
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]] TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[b]] +
	FeynCalc`Package`MetricS CartesianPair[CartesianMomentum[a,dim-1],CartesianMomentum[b,dim-1]];

spToCsp[Momentum[a_, dim_Symbol-4], Momentum[b_, dim_Symbol-4]]:=
	FeynCalc`Package`MetricS CartesianPair[CartesianMomentum[a,dim-4],CartesianMomentum[b,dim-4]];

fvToCv[LorentzIndex[a_], Momentum[b_]]:=
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] Pair[ExplicitLorentzIndex[0], Momentum[b]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a],CartesianMomentum[b]];

fvToCv[LorentzIndex[a_, dim_Symbol], Momentum[b_, dim_Symbol]]:=
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] Pair[ExplicitLorentzIndex[0], Momentum[b]] +
	FeynCalc`Package`MetricS  Pair[LorentzIndex[a,dim],CartesianMomentum[b,dim-1]];

fvToCv[LorentzIndex[a_, dim_Symbol-4], Momentum[b_, dim_Symbol-4]]:=
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim-4],CartesianMomentum[b,dim-4]];

ltensorToCTensor[a___, LorentzIndex[b_], c___]:=
	(
	tmpci = CartesianIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricT ltensorToCTensor[a,ExplicitLorentzIndex[0],c] Pair[ExplicitLorentzIndex[0], LorentzIndex[b]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[b],tmpci] ltensorToCTensor[a,tmpci,c]
	)/; lorIndex

ltensorToCTensor[a___, LorentzIndex[b_, dim_Symbol], c___]:=
	(
	tmpci = CartesianIndex[$MU[Unique[]],dim-1];
	FeynCalc`Package`MetricT ltensorToCTensor[a,ExplicitLorentzIndex[0],c] Pair[ExplicitLorentzIndex[0], LorentzIndex[b,dim]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[b,dim],tmpci] ltensorToCTensor[a,tmpci,c]
	)/; lorIndex

ltensorToCTensor[a___, Momentum[b_], c___]:=
	FeynCalc`Package`MetricT ltensorToCTensor[a,ExplicitLorentzIndex[0],c] Pair[ExplicitLorentzIndex[0], Momentum[b]] +
	FeynCalc`Package`MetricS ltensorToCTensor[a,CartesianMomentum[b],c];

ltensorToCTensor[a___, Momentum[b_, dim_Symbol], c___]:=
	FeynCalc`Package`MetricT ltensorToCTensor[a,ExplicitLorentzIndex[0],c] Pair[ExplicitLorentzIndex[0], Momentum[b]] +
	FeynCalc`Package`MetricS ltensorToCTensor[a,CartesianMomentum[b,dim-1],c];


diracGammaToCDiracGamma[LorentzIndex[a_]]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] DiracGamma[ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a], tmpci] DiracGamma[tmpci]
	);

diracGammaToCDiracGamma[LorentzIndex[a_, dim_Symbol], dim_Symbol]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]],dim-1];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] DiracGamma[ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim], tmpci] DiracGamma[tmpci,dim]
	);

diracGammaToCDiracGamma[LorentzIndex[a_, dim_Symbol-4], dim_Symbol-4]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]],dim-4];
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim-4], tmpci] DiracGamma[tmpci,dim-4]
	);

diracSlashToCDiracSlash[Momentum[p_]]:=
	FeynCalc`Package`MetricT DiracGamma[ExplicitLorentzIndex[0]] Pair[ExplicitLorentzIndex[0], Momentum[p]] +
	FeynCalc`Package`MetricS DiracGamma[CartesianMomentum[p]];

diracSlashToCDiracSlash[Momentum[p_, dim_Symbol], dim_Symbol]:=
	FeynCalc`Package`MetricT DiracGamma[ExplicitLorentzIndex[0]] Pair[ExplicitLorentzIndex[0], Momentum[p]] +
	FeynCalc`Package`MetricS DiracGamma[CartesianMomentum[p,dim-1],dim];

diracSlashToCDiracSlash[Momentum[p_, dim_Symbol-4], dim_Symbol-4]:=
	FeynCalc`Package`MetricS DiracGamma[CartesianMomentum[p,dim-4],dim-4];


pauliSigmaToCPauliSigma[LorentzIndex[a_]]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a], tmpci] PauliSigma[tmpci]
	);

pauliSigmaToCPauliSigma[LorentzIndex[a_, dim_Symbol], dim_Symbol-1]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]],dim-1];
	FeynCalc`Package`MetricT Pair[LorentzIndex[a], ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim], tmpci] PauliSigma[tmpci,dim-1]
	);

pauliSigmaToCPauliSigma[LorentzIndex[a_, dim_Symbol-4], dim_Symbol-4]:=
	(
	tmpci= CartesianIndex[$MU[Unique[]],dim-4];
	FeynCalc`Package`MetricS Pair[LorentzIndex[a,dim], tmpci] PauliSigma[tmpci,dim-4]
	);

pauliSlashToCPauliSlash[Momentum[a_]]:=
	FeynCalc`Package`MetricT TemporalPair[TemporalMomentum[a], ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS PauliSigma[CartesianMomentum[a]];

pauliSlashToCPauliSlash[Momentum[a_, dim_Symbol], dim_Symbol-1]:=
	FeynCalc`Package`MetricT TemporalPair[TemporalMomentum[a], ExplicitLorentzIndex[0]] +
	FeynCalc`Package`MetricS PauliSigma[CartesianMomentum[a,dim-1],dim-1];

pauliSlashToCPauliSlash[Momentum[a_, dim_Symbol-4], dim_Symbol-4]:=
	FeynCalc`Package`MetricS PauliSigma[CartesianMomentum[a,dim-4],dim-4];


powerExpand[ex_, head_, times_]:=
	ex /. Power[Pattern[z,Blank[head]], n_Integer?Positive] :>
		Apply[times, Table[z, {Abs[n]}]]^Sign[n]/; !FreeQ[ex,Power];

powerExpand[ex_, _, _, _]:=
	ex/; FreeQ[ex,Power];

FCPrint[1,"LorentzToCartesian.m loaded."];
End[]
