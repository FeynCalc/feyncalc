(* ::Package:: *)



(* :Title: FCGram															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Gram matrix and Gram determinant		*)

(* ------------------------------------------------------------------------ *)

FCGramMatrix::usage =
"FCGramMatrix[{p1,p2,...}] creates Gram matrix from the given list of momenta.";

FCGramDeterminant::usage =
"FCGramDeterminant[{p1,p2,...}] computes the determinant of the Gram matrix created \
from the given list of momenta.";

FCGram::failmsg = "Error! FCGram has encountered a fatal problem and \
must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCGram`Private`"]

nptfpVerbose=Null;

Options[FCGramMatrix]  = {
	Dimension -> D,
	Prefactor -> 2,
	FCE -> False
};

Options[FCGramDeterminant]  = {
	Dimension -> D,
	Prefactor -> 2,
	ExpandScalarProduct -> True,
	FCE -> False
};

FCGramMatrix[moms_List,OptionsPattern[]]:=
	Block[{mat,len, dim, res},
		len = Length[moms];
		dim = OptionValue[Dimension];
		mat = Table[Pair[Momentum[moms[[i]],dim], Momentum[moms[[j]],dim]], {i, 1, len}, {j, 1, len}];

		If[	!MatrixQ[mat],
			Message[FCGram::failmsg, "The created Gram matrix is incorrect."];
			Abort[]
		];

		res = OptionValue[Prefactor] mat;

		If[	OptionValue[FCE],
			res = FCE[res];
		];

		res


	]/; moms=!={};

FCGramDeterminant[moms_List,OptionsPattern[]]:=
	Block[{mat,det},
		mat = FCGramMatrix[moms,Dimension->OptionValue[Dimension],Prefactor->OptionValue[Prefactor]];
		det = Det[mat];

		If[OptionValue[ExpandScalarProduct],
			det = ExpandScalarProduct[det]
		];

		If[	OptionValue[FCE],
			det = FCE[det];
		];

		det
	];

FCPrint[1,"FCGram.m loaded."];
End[]
