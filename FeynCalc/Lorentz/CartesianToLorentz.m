(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CartesianToLorentz												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Converts selected Cartesian tensors into Lorentz tensors.		*)

(* ------------------------------------------------------------------------ *)


CartesianToLorentz::usage=
"CartesianToLorentz[exp] rewrites Cartesian tensors in form of Lorentz tensors
(when possible). Using options one can specify which types of tensors should
be converted.";

CartesianToLorentz::fail=
"Error! CartesianToLorentz has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CartesianToLorentz`Private`"]

Options[CartesianToLorentz] = {
	CGS 				-> True,
	CSP					-> True,
	DiracGammaExpand	-> True,
	DotSimplify			-> True,
	ExpandScalarProduct -> True,
	FCE					-> False,
	FCI					-> False
};

CartesianToLorentz[expr_, OptionsPattern[]]:=
	Block[{ex, heads, tmp, res, uniqList,null1,null2, uniqListEval, repRule},

		heads = {};

		If[	OptionValue[CSP],
			heads = Join[heads,{CartesianPair}]
		];

		If[	OptionValue[CGS],
			heads = Join[heads,{DiracGamma}]
		];

		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[ heads==={},
			Return[ex]
		];

		uniqList = Cases[ex+null1+null2,Alternatives@@(Blank/@heads),Infinity]//DeleteDuplicates//Sort;

		uniqListEval = uniqList;

		If[	OptionValue[ExpandScalarProduct],
			uniqListEval = ExpandScalarProduct[#,FCI->True]&/@uniqListEval;
		];

		If[	OptionValue[DiracGammaExpand],
			uniqListEval = DiracGammaExpand[uniqListEval,FCI->True];
		];

		If[	OptionValue[CSP],
			uniqListEval = uniqListEval /. CartesianPair -> cspToSp /. cspToSp -> CartesianPair
		];

		If[	OptionValue[CGS],
			uniqListEval = uniqListEval /. DiracGamma -> cdiracSlashToDiracSlash /. cdiracSlashToDiracSlash -> DiracGamma
		];

		repRule = Thread[Rule[uniqList, uniqListEval]];


		res = ex /. Dispatch[repRule];

		If[	OptionValue[DotSimplify],
			res = DotSimplify[res,FCI->True]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

cspToSp[CartesianMomentum[a_], CartesianMomentum[b_]]:=
	FeynCalc`Package`MetricS (Pair[Momentum[a],Momentum[b]] -
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]] TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[b]]);

cspToSp[CartesianMomentum[a_, dim_Symbol-1], CartesianMomentum[b_, dim_Symbol-1]]:=
	FeynCalc`Package`MetricS (Pair[Momentum[a,dim],Momentum[b,dim]] -
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]] TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[b]]);

cspToSp[CartesianMomentum[a_, dim_Symbol-4], CartesianMomentum[b_, dim_Symbol-4]]:=
	FeynCalc`Package`MetricS Pair[Momentum[a,dim-4],Momentum[b,dim-4]];

cdiracSlashToDiracSlash[CartesianMomentum[p_]]:=
	FeynCalc`Package`MetricS (DiracGamma[Momentum[p]] -
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]] DiracGamma[ExplicitLorentzIndex[0]]);

cdiracSlashToDiracSlash[CartesianMomentum[p_, dim_Symbol-1], dim_Symbol]:=
	FeynCalc`Package`MetricS (DiracGamma[Momentum[p,dim],dim] -
	FeynCalc`Package`MetricT TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]] DiracGamma[ExplicitLorentzIndex[0]]);

cdiracSlashToDiracSlash[CartesianMomentum[p_, dim_Symbol-4], dim_Symbol-4]:=
	FeynCalc`Package`MetricS DiracGamma[Momentum[p,dim-4],dim-4];

FCPrint[1,"CartesianToLorentz.m loaded."];
End[]
