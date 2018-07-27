(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SquareAmplitude						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Squares the amplitude 										*)

(* ------------------------------------------------------------------------ *)


SquareAmplitude::usage =
"SquareAmplitude[M,Mcc] multiplies the amplitudes from the list M with their
complex conjugate from the list Mcc to obtain the list of products M_i*Mcc_j.";

SquareAmplitude::failmsg =
"Error! SquareAmplitude has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SquareAmplitude`Private`"]

sqaVerbose::usage="";

Options[SquareAmplitude] = {
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	Indexed -> False,
	Real -> False,
	List -> True
};

diagmarkIdentity[_,_]:=
	1;

SquareAmplitude[m1_List, m2_List/;!OptionQ[m2], OptionsPattern[]] :=
	Block[ {ex, mList, mccList, diagmark, optIndexed, tmp, res},

		optIndexed = OptionValue[Indexed];


		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[	m1==={},
			Message[SquareAmplitude::failmsg, "The lists of amplitudes cannot be empty."];
			Abort[]
		];


		If[	Length[m1]=!=Length[m2],
			Message[SquareAmplitude::failmsg, "The lists of amplitudes have different lengths."];
			Abort[]
		];

		If[	optIndexed=!=False && (optIndexed===True || Head[optIndexed]=!=Symbol),
			Message[SquareAmplitude::failmsg, "The value of the option Indexed must be either False or a function name."];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			sqaVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				sqaVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			{mList, mccList} = {m1, m2},
			{mList, mccList} = FCI[{m1, m2}]
		];

		FCPrint[3, "SquareAmplitude: Entering.", FCDoControl->sqaVerbose];
		FCPrint[3, "SquareAmplitude: Entering with m1: ", mList, FCDoControl->sqaVerbose];
		FCPrint[3, "SquareAmplitude: Entering with m2: ", mccList, FCDoControl->sqaVerbose];

		If[	TrueQ[optIndexed===False],
			diagmark = diagmarkIdentity,
			diagmark = optIndexed
		];

		If[	OptionValue[Real],

			(*	If the matrix element is purely real.	*)
			tmp = Table[	If[	i===j,
							mList[[i]] mccList[[j]] diagmark[i,j],
							2 mList[[i]]mccList[[j]]diagmark[i,j]
						],
						{i,1,Length[mList]},{j,i}
			],
			(*	If the matrix element contains both real and imaginary parts.	*)
			tmp = Table[ mList[[i]] mccList[[j]] diagmark[i,j], {i,1,Length[mList]},{j,1,Length[mList]}]
		];


		FCPrint[3, "SquareAmplitude: Amplitude squared: ", tmp, FCDoControl->sqaVerbose];

		res = Flatten[tmp];

		If[	!OptionValue[List],
			res = Total[res]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SquareAmplitude: Leaving.", FCDoControl->sqaVerbose];

		res

	]

FCPrint[1,"SquareAmplitude.m loaded."];
End[]
