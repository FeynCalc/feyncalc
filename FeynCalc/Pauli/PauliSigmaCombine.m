(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliSigmaCombine												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  The inverse of PauliSigmaExpand								*)

(* ------------------------------------------------------------------------ *)

PauliSigmaCombine::usage =
"PauliSigmaCombine[exp]  is (nearly) the inverse operation to PauliSigmaExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliSigmaCombine`Private`"]

pscVerbose::usage="";

Options[PauliSigmaCombine] = {
	FCI -> False,
	FCE -> False
};

PauliSigmaCombine[expr_, OptionsPattern[]] :=
	Block[{	ex, tmp, res, holdDOT, freePart, diracPart, holdPlus, pauliList, pauliListEval, repRule,
			null1, null2},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex, PauliSigma],
			Return[ex]
		];

		ex = (ex/. Plus -> holdPlus);
		pauliList = (Cases[ex+null1+null2,holdPlus[z__]/; !FreeQ[{z},PauliSigma], Infinity])//DeleteDuplicates//Sort;

		pauliListEval = pauliList /. p_holdPlus :> holdPlus@@Sort[List@@p] //. {
			holdPlus[a___, n1_. PauliSigma[CartesianMomentum[x_,dim_:3],dim_:3], n2_. PauliSigma[CartesianMomentum[y_, dim_:3], dim_:3], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, PauliSigma[CartesianMomentum[n1 x + n2 y, dim], dim], b],
			holdPlus[a___, n1_. PauliSigma[CartesianMomentum[x_,dim_:3],dim_:3], n2_. PauliSigma[CartesianMomentum[x_, dim_:3], dim_:3], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, (n1+n2)PauliSigma[CartesianMomentum[x, dim], dim], b],

			holdPlus[a___, n1_. PauliSigma[Momentum[x_,dim1_:4],dim2_:3], n2_. PauliSigma[Momentum[y_, dim1_:4], dim2_:3], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, PauliSigma[Momentum[n1 x + n2 y, dim1], dim2], b],

			holdPlus[a___, n1_. PauliSigma[Momentum[x_,dim1_:4],dim2_:3], n2_. PauliSigma[Momentum[x_, dim1_:4], dim2_:3], b___]/;(NumberQ[n1] && NumberQ[n2]) :>
				holdPlus[a, (n1+n2)PauliSigma[Momentum[x, dim1], dim2], b]


		} /. holdPlus -> Plus;

		repRule = Thread[Rule[pauliList,pauliListEval]];

		res = ex /. Dispatch[repRule] /. holdPlus -> Plus;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"PauliSigmaCombine.m loaded"];
End[]
