(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainFactor													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Factors Dirac chains with explicit indices using linearity	*)

(* ------------------------------------------------------------------------ *)

DiracChainFactor::usage =
"DiracChainFactor[exp] factors out all expressions inside a DiracChain to which
the chain doesn't apply. For example, all objects that are not Dirac matrices
can be safely factrored out from every Dirac chain.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainFactor`Private`"]

Options[DiracChainFactor] = {
	FCI -> False,
	FCE -> False
};

DiracChainFactor[a_ == b_, opts:OptionsPattern[]] :=
	DiracChainFactor[a,opts] == DiracChainFactor[b,opts];

DiracChainFactor[expr_List, opts:OptionsPattern[]]:=
	DiracChainFactor[#, opts]&/@expr;

DiracChainFactor[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[ {ex, moms,res, diracChains},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ[ex, DiracChain],
			Return[ex]
		];



		diracChains = Cases2[ex, DiracChain];

		If[ diracChains =!= {},
			res = ex /. Dispatch[Thread[diracChains -> chainFactor[diracChains]]]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

chainFactor[x_] :=
	x /. DOT -> holdDOT /. DiracChain -> factorDirac /. holdDOT[] -> 1 /.
	factorDirac -> DiracChain /. holdDOT->DOT;

holdDOT[a___,b_,c___]:=
	b holdDOT[a,c]/; NonCommFreeQ[b];

holdDOT[a___,b1_ b2_,c___]:=
	b1 holdDOT[a,b2,c]/; NonCommFreeQ[b1] && !NonCommFreeQ[b2];

factorDirac[a_,i_,j_] :=
	a DiracChain[1,i,j]/; NonCommFreeQ[a];

factorDirac[a_DiracTrace b_., i_,j_] :=
	a factorDirac[b,i,j];

factorDirac[a_ b_,i_,j_] :=
	a factorDirac[b,i,j]/; NonCommFreeQ[a] && !NonCommFreeQ[b];


FCPrint[1,"DiracChainFactor.m loaded."];
End[]
