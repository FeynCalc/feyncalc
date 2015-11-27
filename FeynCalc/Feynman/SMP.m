(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMP																*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary: Some Standard Model parameters									*)

(* ------------------------------------------------------------------------ *)

SMP::usage= "SMP[par] substitutes a symbol for the Standard Model parameter par. \
SMP[] gives the list of substitutions. par should be a string; e.g., MP[\"SW\"] gives \
FCGV[\"sw\"].";

Begin["`Package`"]
End[]

Begin["`SMP`Private`"]


SMP[] := SMP[] =
{"EL" :> FCGV["e"],
"CW" :> FCGV["cw"],
"ME" :> FCGV["me"],
"MH" :> FCGV["mh"],
"MW" :> FCGV["mw"],
"SW" :> FCGV["sw"]
};

SMP[par_String] := par /. SMP[];

FCPrint[1,"SMP.m loaded."];
End[]
