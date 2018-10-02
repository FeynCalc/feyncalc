(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopMixedIntegralQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Returns True if the integral depends both on Lorentzian and
				Cartesian quantities *)

(* ------------------------------------------------------------------------ *)

FCLoopMixedIntegralQ::usage =
"FCLoopMixedIntegralQ[int] returns True if the integral contains both Lorentz
and Cartesian indices and momenta.";

FCLoopMixedIntegralQ::failmsg =
"FCLoopMixedIntegralQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopMixedIntegralQ`Private`"]

FCLoopMixedIntegralQ[sps_. fad_FeynAmpDenominator]:=
	Block[{heads,res},

			heads = Cases[sps fad, (h:LorentzIndex|CartesianIndex|TemporalIndex|Momentum|CartesianMomentum|TemporalMomentum)[__]:>h,Infinity]//Union;

			Which[
				heads==={},
				res=False,

				(FreeQ2[heads,{LorentzIndex,Momentum,TemporalIndex,TemporalMomentum}] && !FreeQ2[heads,{CartesianIndex,CartesianMomentum}]) ||
				(FreeQ2[heads,{LorentzIndex,Momentum,CartesianIndex,CartesianMomentum}] && !FreeQ2[heads,{TemporalIndex,TemporalMomentum}]) ||
				(FreeQ2[heads,{TemporalIndex,TemporalMomentum,CartesianIndex,CartesianMomentum}] && !FreeQ2[heads,{LorentzIndex,Momentum}]),
				res=False,

				(!FreeQ2[heads,{LorentzIndex,Momentum}] && !FreeQ2[heads,{CartesianIndex,CartesianMomentum}]) ||
				(!FreeQ2[heads,{LorentzIndex,Momentum}] && !FreeQ2[heads,{TemporalIndex,TemporalMomentum}]) ||
				(!FreeQ2[heads,{CartesianIndex,CartesianMomentum}] && !FreeQ2[heads,{TemporalIndex,TemporalMomentum}]) ||
				(!FreeQ2[heads,{LorentzIndex,Momentum}] && !FreeQ2[heads,{TemporalIndex,TemporalMomentum}] && !FreeQ2[heads,{CartesianIndex,CartesianMomentum}]),
				res=True,

				True,
				Message[FCLoopMixedIntegralQ::failmsg,"Unknown combination of heads."];
				Abort[]
			];

			res
		]/;FreeQ[sps,FeynAmpDenominator]

FCPrint[1,"FCLoopMixedIntegralQ.m loaded."];
End[]
