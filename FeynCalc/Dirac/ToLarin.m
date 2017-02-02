(* ::Package:: *)



(* :Title: ToLarin															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Rewrites chiral traces according to Larin's prescription	*)

(* ------------------------------------------------------------------------ *)

ToLarin::usage =
"ToLarin[exp] translates gamma[mu].gamma[5] into \
-I/6 Eps[mu,nu,la,si] gamma[nu,la,si].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToLarin`Private`"]

Options[ToLarin] = {
	Dimension -> D,
	FCI -> False
};

ToLarin[expr_, OptionsPattern[]] :=
	Block[ {ex,fi1,fi2,fi3,drsi,res, dotHold, dim},

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		dim = OptionValue[Dimension];
		drsi = $LeviCivitaSign;
		(*drsi is usually -1 *)

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		ex = ex /. DOT->dotHold;
		ex = ex //. dotHold[a___, DiracGamma[mUU_, dim], DiracGamma[5], b___] :>
			({fi1, fi2, fi3} = LorentzIndex[#,dim]& /@ Unique[{"du","du","du"}];
			(drsi I/6 Eps[mUU, fi1, fi2, fi3, Dimension->dim] dotHold[a,DiracGamma[fi1,dim],DiracGamma[fi2,dim],DiracGamma[fi3,dim],b]));

		res = ex /. dotHold -> DOT;
		res
	];

FCPrint[1,"ToLarin.m loaded."];
End[]
