(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopNonIntegerPropagatorPowersFreeQ										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Returns True if the integral contains no noninteger
				propagator powers											*)

(* ------------------------------------------------------------------------ *)

FCLoopNonIntegerPropagatorPowersFreeQ::usage =
"FCLoopNonIntegerPropagatorPowersFreeQ[int] checks if the integral contains propagators raised to \
noninteger (i.e. fractional or symbolic) powers.";

Begin["`Package`"]
End[]

Begin["`FCLoopNonIntegerPropagatorPowersFreeQ`Private`"]


FCLoopNonIntegerPropagatorPowersFreeQ[sps_. fad_FeynAmpDenominator]:=
	Block[{props},
			props = Cases[fad, (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatoDenominator)[__,{n_/;Head[n]=!=Integer,_}],Infinity];
			props==={}
		]/;FreeQ[sps,FeynAmpDenominator]

FCPrint[1,"FCLoopNonIntegerPropagatorPowersFreeQ.m loaded."];
End[]
