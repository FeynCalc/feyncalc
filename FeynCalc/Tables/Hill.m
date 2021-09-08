(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Hill											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Hill's identity												*)

(* ------------------------------------------------------------------------ *)


Hill::usage =
"Hill[x, y] gives the Hill identity with arguments x and y. The returned object
is 0.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Hill`Private`"]

Hill[x_, y_] :=
	Block[	{t0, t1,t2,t3,t4, t5},

		t0 = Factor2[1-x];
		t1 = Factor2[1-y];
		t2 = Factor2[(x-y)/x];
		t4 = Factor2[t0/t1];
		t3 = Factor2[y/x t4];
		t5 = Factor2[(x-y)/t1];

		PolyLog[2,x] - PolyLog[2,y] + PolyLog[2,Factor2[y/x]] + PolyLog[2, t4] - PolyLog[2, t3] - Pi^2/6 + Log[x] (Log[t0] - Log[t1]) +
		Log[t4] ( Log[t5]-Log[x]- Log[t2] + Log[t1]) - Log[t3] (Log[Factor2[(x-y)/(x (1-y))]] - Log[t2] + Log[t1])
	]
FCPrint[1,"Hill.m loaded."];
End[]
