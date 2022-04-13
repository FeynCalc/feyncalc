(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGammaCombine												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  The inverse of DiracGammaExpand								*)

(* ------------------------------------------------------------------------ *)

DiracGammaCombine::usage =
"DiracGammaCombine[exp] is (nearly) the inverse operation to DiracGammaExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracGammaCombine`Private`"]

dgcVerbose::usage="";

Options[DiracGammaCombine] = {
	FCE -> False,
	FCI -> False
};

DiracGammaCombine[a_ == b_, opts:OptionsPattern[]] :=
	DiracGammaCombine[a,opts] == DiracGammaCombine[b,opts];

DiracGammaCombine[expr_List, opts:OptionsPattern[]]:=
	DiracGammaCombine[#, opts]&/@expr;

DiracGammaCombine[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{	ex, tmp, res, holdDOT, freePart, diracPart, holdPlus, diracList, diracListEval, repRule,
			null1, null2},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex, DiracGamma],
			Return[ex]
		];

		ex = (ex/. Plus -> holdPlus);
		diracList = (Cases[ex+null1+null2,holdPlus[z__]/; !FreeQ[{z},DiracGamma], Infinity])//DeleteDuplicates//Sort;

		diracListEval = diracList /. p_holdPlus :> holdPlus@@Sort[List@@p] //. {
			holdPlus[a___, n1_. DiracGamma[Momentum[x_,dim_:4],dim_:4], n2_. DiracGamma[Momentum[y_, dim_:4], dim_:4], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, DiracGamma[Momentum[n1 x + n2 y, dim], dim], b],
			holdPlus[a___, n1_. DiracGamma[Momentum[x_,dim_:4],dim_:4], n2_. DiracGamma[Momentum[x_, dim_:4], dim_:4], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, (n1+n2)DiracGamma[Momentum[x, dim], dim], b],

			holdPlus[a___, n1_. DiracGamma[CartesianMomentum[x_,dim1_:3],dim2_:4], n2_. DiracGamma[CartesianMomentum[y_, dim1_:3], dim2_:4], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, DiracGamma[CartesianMomentum[n1 x + n2 y, dim1], dim2], b],

			holdPlus[a___, n1_. DiracGamma[CartesianMomentum[x_,dim1_:3],dim2_:4], n2_. DiracGamma[CartesianMomentum[x_, dim1_:3], dim2_:4], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, (n1+n2)DiracGamma[CartesianMomentum[x, dim1], dim2], b]


		} /. holdPlus -> Plus;

		repRule = Thread[Rule[diracList,diracListEval]];

		res = ex /. Dispatch[repRule] /. holdPlus -> Plus;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"DiracGammaCombine.m loaded"];
End[]
