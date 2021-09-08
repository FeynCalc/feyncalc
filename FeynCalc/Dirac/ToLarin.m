(* ::Package:: *)



(* :Title: ToLarin															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Rewrites chiral traces according to Larin's prescription	*)

(* ------------------------------------------------------------------------ *)

ToLarin::usage =
"ToLarin[exp]  substitutes $\\gamma^{\\mu} \\gamma^5$ with
$-\\frac{I}{6}\\varepsilon^{\\mu \\nu \\lambda \\sigma } \\gamma^{\\nu }
\\gamma^{\\lambda} \\gamma^{\\sigma }$.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToLarin`Private`"]

Options[ToLarin] = {
	Dimension	-> D,
	FCE 		-> False,
	FCI			-> False
};

ToLarin[a_ == b_, opts:OptionsPattern[]] :=
	ToLarin[a,opts] == ToLarin[b,opts];

ToLarin[expr_List, opts:OptionsPattern[]]:=
	ToLarin[#, opts]&/@expr;

ToLarin[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[ {ex,fi1,fi2,fi3,drsi,res, dotHold, dim},

		dim 	= OptionValue[Dimension];
		drsi	= $LeviCivitaSign;
		(*drsi is usually -1 *)

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		ex = ex /. DOT->dotHold;
		ex = ex //. dotHold[a___, DiracGamma[mUU: (_LorentzIndex | _Momentum), dim], DiracGamma[5], b___] :>
			({fi1, fi2, fi3} = LorentzIndex[#,dim]& /@ Unique[{"du","du","du"}];
			(drsi I/6 Eps[mUU, fi1, fi2, fi3] dotHold[a,DiracGamma[fi1,dim],DiracGamma[fi2,dim],DiracGamma[fi3,dim],b]));

		res = ex /. dotHold -> DOT;

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"ToLarin.m loaded."];
End[]
