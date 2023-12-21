(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FromFCPartialFractionForm										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: Convert FCPartialFractionForm back to rational functions		*)

(* ------------------------------------------------------------------------ *)

FromFCPartialFractionForm::usage =
"FromFCPartialFractionForm[exp] converts all FCPartialFractionForm symbols
present in exp back into the standard representation.";


FromFCPartialFractionForm::failmsg =
"Error! FromFCPartialFractionForm has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"];

End[]

Begin["`FromFCPartialFractionForm`Private`"];

fpffVerbose::usage="";

Options[FromFCPartialFractionForm] = {
	FCVerbose	-> False,
	Factoring	-> False
};

FromFCPartialFractionForm[ex_List, opts:OptionsPattern[]] :=
	FromFCPartialFractionForm[#, opts]& /@ ex;

FromFCPartialFractionForm[expr_, OptionsPattern[]] :=
	Block[{	res, fpffList, fpffListEval, repRule, optFactoring},

		If [OptionValue[FCVerbose]===False,
			fpffVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fpffVerbose=OptionValue[FCVerbose]
			];
		];

		optFactoring = OptionValue[Factoring];

		FCPrint[1,"FromFCPartialFractionForm: Entering.", FCDoControl->fpffVerbose];
		FCPrint[3,"FromFCPartialFractionForm: Entering with: ", expr, FCDoControl->fpffVerbose];

		If[	FreeQ[expr,FCPartialFractionForm],
			(* Nothing to do *)
			FCPrint[1,"FromFCPartialFractionForm: Leaving.", FCDoControl->fpffVerbose];
			Return[expr]
		];

		fpffList = Cases2[expr,FCPartialFractionForm];
		fpffListEval = fromFCPartialFractionForm/@fpffList;

		FCPrint[3,"FromFCPartialFractionForm: fpffListEval: ", fpffListEval, FCDoControl->fpffVerbose];

		If[	TrueQ[optFactoring=!=False],
			fpffListEval = optFactoring/@fpffListEval;
			FCPrint[3,"FromFCPartialFractionForm: fpffListEval after simplifications: ", fpffListEval, FCDoControl->fpffVerbose]
		];

		If[	!FreeQ2[fpffListEval, {FCPartialFractionForm,fromFCPartialFractionForm,toRatFun}],
			Message[FromFCPartialFractionForm::failmsg, "Failed to eliminate all FCPartialFractionForm objects."];
			Abort[]
		];

		repRule = Thread[Rule[fpffList,fpffListEval]];

		res = expr/. Dispatch[repRule];


		FCPrint[1,"FromFCPartialFractionForm: Leaving.", FCDoControl->fpffVerbose];
		FCPrint[3,"FromFCPartialFractionForm: Leaving with ", res, FCDoControl->fpffVerbose];

		res

	];


fromFCPartialFractionForm[FCPartialFractionForm[pref_, fracs_List, var_]]:=
	MemSet[fromFCPartialFractionForm[FCPartialFractionForm[pref, fracs, var]],
		pref + Total[toRatFun/@fracs]
	];

toRatFun[{{den_, pow_}, pref_}] :=
	MemSet[toRatFun[{{den, pow}, pref}],
		pref den^pow
	];

FCPrint[1,"FromFCPartialFractionForm.m loaded"];
End[]
