(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DeclareFCTensor													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Specify that given heads should be treated as tensor objects	*)

(* ------------------------------------------------------------------------ *)



DeclareFCTensor::usage =
"DeclareFCTensor[a, b, ...] declares a,b, ... to be tensor heads, i.e.,
DataType[a,b, ...,  FCTensor] is set to True.";

UnDeclareFCTensor::usage =
"UnDeclareFCTensor[a, b, ...] undeclares a,b, ... to be tensor heads, i.e.,
DataType[a,b, ...,  FCTensor] is set to False.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`DeclareFCTensor`Private`"];

DeclareFCTensor[] :=
	(Message[DeclareFCTensor::argrx, DeclareFCTensor, 0, "1 or more"];
	Abort[]);

DeclareFCTensor[b__] :=
	(Map[Set[DataType[#, FCTensor], True]&, Flatten[{b}]];
	Null);

UnDeclareFCTensor[] :=
	(Message[UnDeclareFCTensor::argrx, UnDeclareFCTensor, 0, "1 or more"];
	Abort[]);

UnDeclareFCTensor[b__] :=
	(Map[Set[DataType[#, FCTensor], False]&, Flatten[{b}]];
	Null);

FCPrint[1,"DeclareFCTensor.m loaded."];
End[]
