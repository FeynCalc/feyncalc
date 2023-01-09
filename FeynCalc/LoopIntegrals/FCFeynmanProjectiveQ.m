(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanProjectiveQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Checks projectivity of a Feynman integral					*)

(* ------------------------------------------------------------------------ *)

FCFeynmanProjectiveQ::usage =
"FCFeynmanProjectiveQ[int, x] checks if the given Feynman parameter integral
(without prefactors) depending on x[1], x[2], ...  is a projective form.

It is similar to FCFeynmanProjectivize but unlike the former it simply returns
True or False depending
on whether the integral is projective or not.";

Begin["`Package`"]
End[]

Begin["`FCFeynmanProjectiveQ`Private`"]


Options[FCFeynmanProjectiveQ] =
	Options[FCFeynmanProjectivize];


FCFeynmanProjectiveQ[ex_, var_, opts:OptionsPattern[]]:=
	Block[{tmp},
		tmp = FCFeynmanProjectivize[ex,var,{Check->False,FCVerbose->-1,FilterRules[{opts},Except[Check|FCVerbose]]}];
		tmp===ex
	];


FCPrint[1,"FCFeynmanProjectiveQ.m loaded."];
End[]
