(* ::Package:: *)



(* :Title: FCGram															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Gram matrix and Gram determinant		*)

(* ------------------------------------------------------------------------ *)

FCGramMatrix::usage =
"FCGramMatrix[{p1, p2, ...}] creates a Gram matrix from the given list of
momenta.";

FCGramDeterminant::usage =
"FCGramDeterminant[{p1, p2, ...}] computes the determinant of the Gram matrix
created from the given list of momenta.";

FCGramMatrix::failmsg = "Error! FCGramMatrix has encountered a fatal problem and \
must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCGram`Private`"]

nptfpVerbose=Null;

Options[FCGramMatrix]  = {
	Dimension 			-> D,
	FCE					-> False,
	FCI					-> False,
	FinalSubstitutions	-> {},
	Head				-> {Pair, Momentum},
	Prefactor			-> 2
};

Options[FCGramDeterminant]  = {
	Dimension			-> D,
	ExpandScalarProduct	-> True,
	FinalSubstitutions	-> {},
	FCE					-> False,
	Head				-> {Pair, Momentum},
	Prefactor			-> 2
};

FCGramMatrix[moms_List,OptionsPattern[]]:=
	Block[{	mat, len, dim, res, pairHead, momHead, optKinematics},

		len				= Length[moms];
		dim				= OptionValue[Dimension];
		pairHead		= OptionValue[Head][[1]];
		momHead			= OptionValue[Head][[2]];
		optKinematics	= OptionValue[FinalSubstitutions];

		If[	optKinematics=!={} && !OptionValue[FCI],
			optKinematics = FCI[optKinematics]
		];

		mat = Table[pairHead[momHead[moms[[i]],dim], momHead[moms[[j]],dim]], {i, 1, len}, {j, 1, len}]/. optKinematics;

		If[	!MatrixQ[mat],
			Message[FCGramMatrix::failmsg, "The created Gram matrix is incorrect."];
			Abort[]
		];

		res = OptionValue[Prefactor] mat;

		If[	OptionValue[FCE],
			res = FCE[res];
		];

		res


	]/; moms=!={};

FCGramDeterminant[{},OptionsPattern[]]:=
	1;

FCGramDeterminant[moms_List,OptionsPattern[]]:=
	Block[{mat,det},
		mat = FCGramMatrix[moms,Dimension->OptionValue[Dimension],Prefactor->OptionValue[Prefactor], Head->OptionValue[Head],
			FinalSubstitutions-> OptionValue[FinalSubstitutions]];

		det = Det[mat];

		If[OptionValue[ExpandScalarProduct],
			det = ExpandScalarProduct[det, FCI->True]
		];

		If[	OptionValue[FCE],
			det = FCE[det];
		];

		det
	]/; moms=!={};

FCPrint[1,"FCGram.m loaded."];
End[]
