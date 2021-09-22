(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMPToSymbol														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Introduces abbreviations for scalar products of external
				momenta, SMPs and other variables which appear in the
				expression. Functions are not allowed. Mainly meant to be
				used for export of FeynCalc expressions	when interfacing
				with other tools.											*)

(* ------------------------------------------------------------------------ *)

SMPToSymbol::usage =
"SMPToSymbol[exp] converts objects of type SMP[\"sth\"] in exp to symbols using
ToExpression[\"sth\"].

The option StringReplace can be used to specify string replacement rules that
will take care of special characters (e.g. ^ or _) that cannot appear in valid
Mathematica expressions. SMPToSymbol is useful when exporting FeynCalc
expressions to other tools, e.g. FORM.";

FCGVToSymbol::usage =
"FCGVToSymbol[exp] converts objects of type FCGV[\"sth\"] in exp to symbols
using ToExpression[\"sth\"].

The option StringReplace can be used to specify string replacement rules that
will take care of special characters (e.g. ^ or _) that cannot appear in valid
Mathematica expressions. SMPToSymbol is useful when exporting FeynCalc
expressions to other tools, e.g. FORM.";

FCLoopGLIToSymbol::usage =
"FCLoopGLIToSymbol[exp] converts GLIs to symbols.

The option Head determines the prefix of the symbol and can be set to
FCTopology (default) or GLI

The option Character specifies the separator between to prefix and the
indices.";

ToSymbol::failmsg = "Error! ToSymbol has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`ToSymbol`"]
End[]

Begin["`ToSymbol`Private`"]

Options[SMPToSymbol] = {
	Conjugate 		-> "CC",
	StringReplace	-> {"_"->"", "^"->""}
};

Options[FCGVToSymbol] = {
	StringReplace-> {"_"->"", "^"->""}
};

Options[FCLoopGLIToSymbol] =
{
	Character	-> "X",
	Head 		-> FCTopology,
	StringReplace-> {}
}

SMPToSymbol[expr_, OptionsPattern[]] :=
	Block[{res, optConjugate, repRule},

		optConjugate 		= OptionValue[Conjugate];

		repRule = {
			SMP[s_String]->s,
			SMP[{s_String, Complex[0,1]}]->s,
			SMP[{s_String, Complex[0,-1]}]:>s<>optConjugate
		};


		res = toSymbol[expr, SMP, repRule, OptionValue[StringReplace]];

		res
	];

FCGVToSymbol[expr_, OptionsPattern[]] :=
	Block[{res, optConjugate, repRule},

		repRule = {
			FCGV[s_String]->s
		};


		res = toSymbol[expr, FCGV, repRule, OptionValue[StringReplace]];

		res
	];



FCLoopGLIToSymbol[expr_, OptionsPattern[]] :=
	Block[{res, optHead, optCharacter, repRule},

		optHead 	 = OptionValue[Head];
		optCharacter = OptionValue[Character];

		Switch[optHead,
			FCTopology,
			repRule = {
				GLI[id_, inds_List] :> ToString[id]<>optCharacter<> StringJoin[ToString /@ inds]
			},

			GLI,
			repRule = {
				GLI[id_, inds_List] :> "GLI"<>optCharacter<>ToString[id]<>optCharacter<>StringJoin[ToString /@ inds]
			},
			_,
			Message[ToSymbol::failmsg, "Unknown value of the Head option.."];
			Abort[]
		];

		res = toSymbol[expr, GLI, repRule, OptionValue[StringReplace]];

		res
	];

toSymbol[expr_, head_, repRule_List, optStringReplace_] :=
	Block[{res, allSymbols, symbols, rule, message},

		allSymbols = Cases2[expr, head];

		If[	allSymbols==={},
			Return[expr]
		];

		symbols = allSymbols /. repRule;

		If[	!MatchQ[symbols,{_String..}],
			Message[ToSymbol::failmsg, "Failed to extract strings out of all "<>ToString[head]<>" symbols."];
			Abort[]
		];

		symbols = StringReplace[symbols,optStringReplace];

		If[	!MatchQ[SyntaxQ/@symbols,{True..}],
			Message[ToSymbol::failmsg, "The final strings do not represent valid Mathematica expressions."];
			Abort[]
		];

		symbols = ToExpression/@symbols;


		If[	!MatchQ[Head/@symbols,{Symbol..}],
			Message[ToSymbol::failmsg, "The resulting expressions are not simple symbols."];
			Abort[]
		];

		rule = Thread[Rule[allSymbols,symbols]];


		res = expr /. Dispatch[rule];


		res
	];


FCPrint[1,"SMPToSymbol.m loaded."];
End[]
