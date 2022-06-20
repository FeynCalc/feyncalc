(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanParameterJoin											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  	Joins propagators using Feyman parametrization				*)

(* ------------------------------------------------------------------------ *)

FCFeynmanParameterJoin::usage =
"FCFeynmanParameterJoin[{{{prop1,prop2,x},prop3,y},...}, {p1,p2,...}] joins all
propagators in int using Feynman parameters but does not integrate over the
loop momenta $p_i$. The function returns {fpInt,pref,vars}, where fpInt is the
piece of the integral that contains a single GFAD-type propagator and pref is
the part containing the res. The introduced Feynman parameters are listed in
vars. The overall Dirac delta is omitted.

Notice that each inner list must contain exactly thee elements, the first two
being propagators (or products of propagators) and the last one denoting the
head of Feynman parameter variables used to join those propagators. For
example,
{FAD[{p1,m1}],FAD[{p2,m2}],x},  but also
{FAD[{p1,m1}],FAD[{p2,m2}]FAD[{p3,m3}],x} or even
{FAD[{p1,m1}]FAD[{p2,m2}],FAD[{p3,m3}]FAD[{p4,m4}],x} represent valid examples
of such lists, while something like {FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p3,m3}],x}
(4 instead of 3 list elements) is invalid and will not work.

Having obtained an output of FCFeynmanParameterJoin (e.g. called intT), you
should use the following syntax to pass this to
FCFeynmanParametrize:  FCFeynmanParametrize[intT[[1]],intT[[2]],{lmom1, lmom2,
...},Variables->intT[[3]]]";

FCFeynmanParameterJoin::failmsg =
"Error! FCFeynmanParameterJoin has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanParameterJoin`Private`"]

fcfpjVerbose::usage="";
overallPref::usage="";
allVars::usage="";
allLmoms::usage="";
optIndexed::usage="";
optFactoring::usage="";
optFinalSubstitutions::usage="";
optDiracDelta::usage="";

Options[FCFeynmanParameterJoin] = {
	DiracDelta			-> False,
	FCE					-> False,
	FCI					-> False,
	FCVerbose			-> False,
	Factoring 			-> Factor2,
	FinalSubstitutions	-> {},
	Indexed				-> True
};



FCFeynmanParameterJoin[exprs_List, lmomsRaw_List /; ! OptionQ[lmomsRaw], OptionsPattern[]]:=
	Block[{	res, gfad, pref, vars, lmoms, tmp, ex},

		If [OptionValue[FCVerbose]===False,
			fcfpjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcfpjVerbose=OptionValue[FCVerbose]
			];
		];

		optIndexed 			  	= OptionValue[Indexed];
		optFactoring		  	= OptionValue[Factoring];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optDiracDelta			= OptionValue[DiracDelta];

		FCPrint[1,"FCFeynmanParameterJoin: Entering. ", FCDoControl->fcfpjVerbose];
		FCPrint[3,"FCFeynmanParameterJoin: Entering  with: ", exprs, FCDoControl->fcfpjVerbose];

		If[OptionValue[FCI],
			ex = exprs,
			ex = FCI[exprs]
		];

		overallPref = 1;
		allVars = {};
		allLmoms = lmomsRaw;

		tmp = ex //. List[a_, b_, var_] /; Head[a] =!= List && Head[b] =!= List :>
			feynmanJoin[a, b, var];

		FCPrint[3,"FCFeynmanParameterJoin: After feynmanJoin: ", tmp, FCDoControl->fcfpjVerbose];


		res = {tmp, overallPref, Flatten[allVars]};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCFeynmanParameterJoin: Leaving.", FCDoControl->fcfpjVerbose];
		FCPrint[3,"FCFeynmanParameterJoin: Leaving with: ", res, FCDoControl->fcfpjVerbose];

		res
];

feynmanJoin[a_, b_, var_]:=
	Block[{gfad,pref,vars,lmoms},

		lmoms = Select[allLmoms, !FreeQ[{a,b},#]&];

		FCPrint[4,"FCFeynmanParameterJoin: feynmanJoin: Entering with: ", {a,b}, FCDoControl->fcfpjVerbose];

		{gfad,pref,vars} = FCFeynmanPrepare[a b,  lmoms, FCFeynmanParameterJoin -> True,
				FinalSubstitutions -> optFinalSubstitutions, Indexed->optIndexed,
				Names -> var, Factoring -> optFactoring];

		FCPrint[4,"FCFeynmanParameterJoin: feynmanJoin: Obtained: ", {gfad,pref,vars}, FCDoControl->fcfpjVerbose];

		If[	optDiracDelta,
			pref = pref*DiracDelta[1-Total[vars]]
		];

		overallPref = overallPref pref;
		allVars 	= Join[allVars,vars];
		gfad
	];


FCPrint[1,"FCFeynmanParameterJoin.m loaded."];
End[]
