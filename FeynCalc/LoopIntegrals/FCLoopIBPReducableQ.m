(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIBPReducableQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Returns True if the integral contains propagators raised to
				integer powers and False otherwise							*)

(* ------------------------------------------------------------------------ *)

FCLoopIBPReducableQ::usage =
"FCLoopIBPReducableQ[int] checks if the integral contains propagators raised to
integer powers.";

FCLoopIBPReducableQ::failmsg =
"ErroFCLoopIBPReducableQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopBasis`Private`"]

(* Safe for memoization *)
FCLoopIBPReducableQ[sps_. fad_FeynAmpDenominator]:=
	MemSet[FCLoopIBPReducableQ[sps fad],
		Block[{fadList,res, propPowers},

			fadList = Sort[List@@fad];

			propPowers = Union[Flatten[{1,Cases[List@@fad, (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__,{n_,_}]:>n, Infinity]}]];

			res = (fadList=!=Union[fadList] || MatchQ[sps, _. Power[(Pair|CartesianPair)[__], _]] || MatchQ[propPowers,{1,__}]);
			res
		]
	];

FCPrint[1,"FCLoopIBPReducableQ.m loaded."];
End[]
