(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIBPReducableQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Returns True if the integral contains propagators raised to
				integer powers and False otherwise							*)

(* ------------------------------------------------------------------------ *)

FCLoopIBPReducableQ::usage =
"FCLoopBasisIncompleteQ[int] checks if the integral contains propagators raised to
integer powers";

FCLoopIBPReducableQ::failmsg =
"ErroFCLoopIBPReducableQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopBasis`Private`"]

(* Safe for memoization *)
FCLoopIBPReducableQ[sps_. fad_FeynAmpDenominator]:=
	MemSet[FCLoopIBPReducableQ[sps fad],
		Block[{fadList,res},
			fadList = Sort[List@@fad];
			res = (fadList=!=Union[fadList] || MatchQ[sps, _. Power[Pair[__], _]]);
			res
		]
	];

FCPrint[1,"FCLoopIBPReducableQ.m loaded."];
End[]
