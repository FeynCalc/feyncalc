(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCHideShowEpsilon												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Substitutes 1/Epsilon - EulerGamma + Log[4Pi] with
				SMP["Delta"] or vice versa									*)

(* ------------------------------------------------------------------------ *)

FCHideEpsilon::usage =
"FCHideEpsilon[expr] substitutes 1/Epsilon - EulerGamma + Log[4Pi] with \
SMP[\"Delta\"]";

FCShowEpsilon::usage =
"FCShowEpsilon[expr] substitutes SMP[\"Delta\"] with 1/Epsilon - \
EulerGamma + Log[4Pi] with";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCHideShowEpsilon`Private`"]

Options[FCHideEpsilon] = {
	Factoring -> Factor,
	Collecting -> True
};

Options[FCShowEpsilon] = {};

FCHideEpsilon[expr_, OptionsPattern[]] :=
	Block[{tmp,wrap,factoring},

		factoring = OptionValue[Factoring];

		tmp = Collect2[expr,{Epsilon,EpsilonUV,EpsilonIR},Factoring->factoring];

		tmp = tmp/. {	1/Epsilon -> wrap[1/Epsilon],
						1/EpsilonUV -> wrap[1/EpsilonUV],
						1/EpsilonIR -> wrap[1/EpsilonIR]
		};

		tmp = tmp /. { 	wrap[1/Epsilon] -> SMP["Delta"] + EulerGamma - Log[4Pi],
						wrap[1/EpsilonUV] -> SMP["Delta_UV"] + EulerGamma - Log[4Pi],
						wrap[1/EpsilonIR] -> SMP["Delta_IR"] + EulerGamma - Log[4Pi]
		};

		If[	OptionValue[Collecting],
			tmp = Collect2[tmp,{SMP["Delta"],SMP["Delta_UV"],SMP["Delta_IR"]},Factoring->factoring]
		];

		tmp

	];

FCShowEpsilon[expr_, OptionsPattern[]] :=
	Block[{tmp},

		tmp = expr/. { SMP["Delta"] -> 1/Epsilon - EulerGamma + Log[4Pi],
					SMP["DeltaUV"] -> 1/EpsilonUV - EulerGamma + Log[4Pi],
					SMP["DeltaIR"] -> 1/EpsilonIR - EulerGamma + Log[4Pi]
		};

		tmp

	];

FCPrint[1,"FCHideShowEpsilon.m loaded."];
End[]
