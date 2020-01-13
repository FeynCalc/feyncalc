(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMPToSymbol														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:	Introduces abbreviations for scalar products of external
				momenta, SMPs and other variables which appear in the
				expression. Functions are not allowed. Mainly meant to be
				used for export of FeynCalc expressions	when interfacing
				with other tools.											*)

(* ------------------------------------------------------------------------ *)

SMPToSymbol::usage =
"SMPToSymbol[exp] converts objects of type SMP[\"sth\"] in exp to symbols using
ToExpression[\"sth\"]. The option StringReplace can be used to specify string
replacement rules that will take care of characters (e.g. ^ or _) that
cannot appear in valid expressions. SMPToSymbol is useful when exporting FeynCalc
expressions to other tools, e.g. FORM.";

SMPToSymbol::failmsg = "Error! SMPToSymbol has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`SMPToSymbol`"]
End[]

Begin["`SMPToSymbol`Private`"]

Options[SMPToSymbol] = {
	StringReplace-> {"_"->"", "^"->""},
	Conjugate -> "CC"
};

SMPToSymbol[expr_, OptionsPattern[]] :=
	Block[{res, allSMPs, symbols, optConjugate, optStringReplace, rule},

		optConjugate 		= OptionValue[Conjugate];
		optStringReplace 	= OptionValue[StringReplace];
		allSMPs = Cases2[expr, SMP];

		If[	allSMPs==={},
			Return[expr]
		];

		symbols = allSMPs /. {
			SMP[s_String]->s,
			SMP[{s_String, Complex[0,1]}]->s,
			SMP[{s_String, Complex[0,-1]}]:>s<>optConjugate
		};

		If[	!MatchQ[symbols,{_String..}],
			Message[SMPToSymbol::failmsg, "Failed to extract strings out of all SMPs."];
			Abort[]
		];

		symbols = StringReplace[symbols,optStringReplace];

		If[	!MatchQ[SyntaxQ/@symbols,{True..}],
			Message[SMPToSymbol::failmsg, "The final strings do not represent valid Mathematica expressions."];
			Abort[]
		];

		symbols = ToExpression/@symbols;

		If[	!MatchQ[Head/@symbols,{Symbol..}],
			Message[SMPToSymbol::failmsg, "The resulting expressions are not simple symbols."];
			Abort[]
		];

		rule = Thread[Rule[allSMPs,symbols]];


		res = expr /. Dispatch[rule];


		res
	];

FCPrint[1,"SMPToSymbol.m loaded."];
End[]
