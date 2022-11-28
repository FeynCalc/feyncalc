(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetScalarProducts												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Returns defined scalar products						    	*)

(* ------------------------------------------------------------------------ *)

FCGetScalarProducts::usage =
"FCGetScalarProducts[{moms}] returns all scalar products involving external
momenta moms that were set using down values. The lhs is wrapped with Hold.
Applying FRH to the output gives the values of the scalar products.
";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCGetScalarProducts`Private`"]

Options[FCGetScalarProducts] = {
	SetDimensions->{4,D}
};

FCGetScalarProducts[{}, OptionsPattern[]]:=
	{};

FCGetScalarProducts[{moms__}, OptionsPattern[]] :=
	Block[{spList, res, mom1, mom2, pattern, listHold,
		ruleDelayed, optSetDimensions, spRules, cspRules},

		optSetDimensions = OptionValue[SetDimensions];

		spList = SelectNotFree[$ScalarProducts,moms];

		spRules = Map[ruleDelayed[List[Momentum[pattern[mom1,Blank[]],BlankNullSequence[]],
			Momentum[pattern[mom2,Blank[]],	BlankNullSequence[]]],Hold[Pair][Momentum[mom1,#],Momentum[mom2,#]]]&,
			optSetDimensions] /. pattern->Pattern /. ruleDelayed->RuleDelayed;

		cspRules = Map[ruleDelayed[List[CartesianMomentum[pattern[mom1,Blank[]],BlankNullSequence[]],
			CartesianMomentum[pattern[mom2,Blank[]], BlankNullSequence[]]],Hold[CartesianPair][CartesianMomentum[mom1,#],CartesianMomentum[mom2,#]]]&,
			optSetDimensions] /. pattern->Pattern /. ruleDelayed->RuleDelayed /. {
				CartesianMomentum[x_,_Symbol] -> CartesianMomentum[x,_Symbol - 1],
				CartesianMomentum[x_,4] -> CartesianMomentum[x,3]
			};

		res = Flatten[SelectNotFree[Map[(spList/.#)&, Join[spRules,cspRules]],{Hold[Pair],Hold[CartesianPair]}]];

		res = Select[res,!MemberQ[{Pair,CartesianPair},Head[FRH[#]]]&];

		res = Map[Rule[#,FRH[#]]&,res];

		res
	];

FCPrint[1,"FCGetScalarProducts.m loaded"];
End[]
