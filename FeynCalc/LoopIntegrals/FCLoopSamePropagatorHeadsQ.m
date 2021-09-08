(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSamePropagatorHeadsQ										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Returns True if the FeynAmpDenominator contains propagator
				denominators of the same type. *)

(* ------------------------------------------------------------------------ *)

FCLoopSamePropagatorHeadsQ::usage =
"FCLoopSamePropagatorHeadsQ[exp] returns True if the FeynAmpDenominator of exp
contains only propagator denominators of the same type (e.g. only
StandardPropagatorDenominator or only CartesianPropagatorDenominator).";

FCLoopSamePropagatorHeadsQ::failmsg =
"FCLoopSamePropagatorHeadsQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopSamePropagatorHeadsQ`Private`"]

FCLoopSamePropagatorHeadsQ[sps_. fad_FeynAmpDenominator]:=
	Block[{heads},

		heads = Cases[sps fad, (h:PropagatorDenominator|StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__]:>h,Infinity]//
			Union;
		If[	heads==={},
			Message[FCLoopSamePropagatorHeadsQ::failmsg,"The integral contains unknown propagator denominators."];
		];

		(Length[heads] === 1)

	]/;FreeQ[sps,FeynAmpDenominator]

FCPrint[1,"FCLoopSamePropagatorHeadsQ.m loaded."];
End[]
