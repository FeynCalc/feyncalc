(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliChainFactor													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Factors Pauli chains with explicit indices using linearity	*)

(* ------------------------------------------------------------------------ *)

PauliChainFactor::usage =
"PauliChainFactor[exp] factors out all expressions inside a PauliChain to which
the chain doesn't apply. For example, all objects that are not Pauli matrices
can be safely factored out from every Pauli chain.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliChainFactor`Private`"]

Options[PauliChainFactor] = {
	FCI -> False,
	FCE -> False
};

PauliChainFactor[a_ == b_, opts:OptionsPattern[]] :=
	PauliChainFactor[a,opts] == PauliChainFactor[b,opts];

PauliChainFactor[expr_List, opts:OptionsPattern[]]:=
	PauliChainFactor[#, opts]&/@expr;

PauliChainFactor[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[ {ex, moms,res, PauliChains},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ[ex, PauliChain],
			Return[ex]
		];



		PauliChains = Cases2[ex, PauliChain];

		If[ PauliChains =!= {},
			res = ex /. Dispatch[Thread[PauliChains -> chainFactor[PauliChains]]]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

chainFactor[x_] :=
	x /. DOT -> holdDOT /. PauliChain -> factorPauli /. holdDOT[] -> 1 /.
	factorPauli -> PauliChain /. holdDOT->DOT;

holdDOT[a___,b_,c___]:=
	b holdDOT[a,c]/; NonCommFreeQ[b];

holdDOT[a___,b1_ b2_,c___]:=
	b1 holdDOT[a,b2,c]/; NonCommFreeQ[b1] && !NonCommFreeQ[b2];

factorPauli[a_,i_,j_] :=
	a PauliChain[1,i,j]/; NonCommFreeQ[a];

factorPauli[a_PauliTrace b_., i_,j_] :=
	a factorPauli[b,i,j];

factorPauli[a_ b_,i_,j_] :=
	a factorPauli[b,i,j]/; NonCommFreeQ[a] && !NonCommFreeQ[b];


FCPrint[1,"PauliChainFactor.m loaded."];
End[]
