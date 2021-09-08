(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCHideShowEpsilon												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Substitutes 1/Epsilon - EulerGamma + Log[4Pi] with
				SMP["Delta"] or vice versa									*)

(* ------------------------------------------------------------------------ *)

FCHideEpsilon::usage =
"FCHideEpsilon[expr] substitutes 1/Epsilon - EulerGamma + Log[4 Pi] with
SMP[\"Delta\"].";

FCShowEpsilon::usage =
"FCShowEpsilon[expr] substitutes SMP[\"Delta\"] with 1/Epsilon - EulerGamma +
Log[4 Pi].";

FCHideEpsilon::failmsg =
"Error! FCHideEpsilon has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCShowEpsilon::failmsg =
"Error! FCShowEpsilon has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCHideShowEpsilon`Private`"]

Options[FCHideEpsilon] = {
	Collecting	-> True,
	D			-> 4 - 2 Epsilon,
	Factoring	-> Factor,
	Subtract	-> EulerGamma - Log[4Pi]
};

Options[FCShowEpsilon] = {
	D 			-> 4 - 2 Epsilon,
	Subtract	-> EulerGamma - Log[4Pi]
};

FCHideEpsilon[expr_, OptionsPattern[]] :=
	Block[{tmp,wrap,factoring,pref, dVal, subtract},

		factoring = OptionValue[Factoring];
		dVal = OptionValue[D];
		subtract = OptionValue[Subtract];

		tmp = Collect2[expr,{Epsilon,EpsilonUV,EpsilonIR},Factoring->factoring];

		Which[
			dVal === 4 - 2 Epsilon,
				pref = 1,
			dVal === 4 -  Epsilon,
				pref = 2,
			True,
				Message[FCHideEpsilon::failmsg,"Unknown choice for D"];
				Abort[]
		];



		tmp = tmp/. {	1/Epsilon -> wrap[pref/Epsilon]/pref,
						1/EpsilonUV -> wrap[pref/EpsilonUV]/pref,
						1/EpsilonIR -> wrap[pref/EpsilonIR]/pref
		};

		tmp = tmp /. { 	wrap[pref/Epsilon] -> SMP["Delta"] + subtract,
						wrap[pref/EpsilonUV] -> SMP["Delta_UV"] + subtract,
						wrap[pref/EpsilonIR] -> SMP["Delta_IR"] + subtract
		};

		If[	OptionValue[Collecting],
			tmp = Collect2[tmp,{SMP["Delta"],SMP["Delta_UV"],SMP["Delta_IR"]},Factoring->factoring]
		];

		tmp

	];

FCShowEpsilon[expr_, OptionsPattern[]] :=
	Block[{tmp,pref, dVal, subtract},

		dVal = OptionValue[D];
		subtract = OptionValue[Subtract];

		Which[
			dVal === 4 - 2 Epsilon,
				pref = 1,
			dVal === 4 -  Epsilon,
				pref = 2,
			True,
				Message[FCShowEpsilon::failmsg,"Unknown choice for D"];
				Abort[]
		];

		tmp = expr/. { SMP["Delta"] -> pref/Epsilon - subtract,
					SMP["Delta_UV"] -> pref/EpsilonUV - subtract,
					SMP["Delta_IR"] -> pref/EpsilonIR - subtract
		};

		tmp

	];

FCPrint[1,"FCHideShowEpsilon.m loaded."];
End[]
