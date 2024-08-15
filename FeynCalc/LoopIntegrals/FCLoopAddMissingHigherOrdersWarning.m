(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopAddMissingHigherOrdersWarning										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Add scalings of masses and momenta							*)

(* ------------------------------------------------------------------------ *)

FCLoopAddMissingHigherOrdersWarning::usage =
"FCLoopAddMissingHigherOrdersWarning[expr, ep, fun] determines the highest
ep-power $n$ in the given expression and adds a warning flag of order
$\\textrm{ep}^n+1$. This is meant to prevent incorrect results stemming
insufficient high expansions of expr in ep";

FCLoopAddMissingHigherOrdersWarning::failmsg =
"Error! FCLoopAddMissingHigherOrdersWarning has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopAddMissingHigherOrdersWarning`Private`"]

fclamhoVerbose::usage = "";

Options[FCLoopAddMissingHigherOrdersWarning] = {
	FCE 		-> False,
	FCI 		-> False,
	FCVerbose 	-> False,
	Names		-> Automatic,
	"LHS"		-> None,
	Complex		-> True
};

FCLoopAddMissingHigherOrdersWarning[ex_List, ep_, fun_, opts:OptionsPattern[]]:=
	Map[FCLoopAddMissingHigherOrdersWarning[#, ep, fun, opts]&,ex];

FCLoopAddMissingHigherOrdersWarning[Rule[a_,b_], ep_, fun_, opts:OptionsPattern[]]:=
	Rule[a,FCLoopAddMissingHigherOrdersWarning[b, ep, fun, opts, "LHS"->a]];

FCLoopAddMissingHigherOrdersWarning[RuleDelayed[a_,b_], ep_, fun_, opts:OptionsPattern[]]:=
	rd[a,FCLoopAddMissingHigherOrdersWarning[b, ep, fun, opts, "LHS"->a]] /. rd ->RuleDelayed;

FCLoopAddMissingHigherOrdersWarning[expr_, ep_, fun_, OptionsPattern[]] :=
Block[{	epPower, res , optNames, pref, epHelpName, optLHS},


	optNames 		= OptionValue[Names];
	optLHS			= OptionValue["LHS"];

	If [OptionValue[FCVerbose]===False,
				fclamhoVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fclamhoVerbose=OptionValue[FCVerbose]
				];
	];

	If[	OptionValue[Complex],
		pref = (1+I),
		pref = 1
	];

	If[ optNames===Automatic,

		If[	optLHS===None,
			epHelpName = fun,
			Which[
				Head[optLHS]===GLI,
					epHelpName=fun[FCLoopGLIToSymbol[optLHS]],
				True,
					Message[FCLoopAddMissingHigherOrdersWarning::failmsg, "Unsupported value of the LHS option."];
					Abort[]
			];
		],


		epHelpName = fun
	];


	FCPrint[1,"FCLoopAddMissingHigherOrdersWarning: Entering.", FCDoControl->fclamhoVerbose];
	FCPrint[3,"FCLoopAddMissingHigherOrdersWarning: Entering with: ", expr, FCDoControl->fclamhoVerbose];

	epPower = Exponent[expr,ep];

	If[	!IntegerQ[epPower],
		Message[FCLoopAddMissingHigherOrdersWarning::failmsg, "Failed to extract the highest power of " <> ToString[ep]];
		Abort[]
	];

	res = expr + pref*epHelpName*ep^(epPower+1);

	FCPrint[1,"FCLoopAddMissingHigherOrdersWarning: Leaving.", FCDoControl->fclamhoVerbose];

	res

]/; !MemberQ[{Rule,RuleDelayed,List},Head[expr]]


FCPrint[1,"FCLoopAddMissingHigherOrdersWarning.m loaded."];
End[]
