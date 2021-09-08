(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCShowReferenceCard				                               *)

(*
This software is covered by the GNU General Public License 3.
Copyright (C) 1990-2018 Rolf Mertig
Copyright (C) 1997-2018 Frederik Orellana
Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(*  :Summary:  Shows reference cards.
*)

(* ------------------------------------------------------------------------ *)



FCShowReferenceCard::usage =
"FCShowReferenceCard[{name}]  shows the reference card that corresponds to
\"name\". Reference cards are stored in Tables/ReferenceCards inside the
FeynCalc main directory. FCShowReferenceCard[] lists available reference
cards.";

FCShowReferenceCard::nav =
"The reference card you requested is not available.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCShowReferenceCard`Private`"]

FCShowReferenceCard[l_List] :=
Block[{res = l},

	res = res /. refCards;
	If[	Head[res]===List,
		Message[FCShowReferenceCard::nav],
		Print[res]
	];


];

FCShowReferenceCard[]:=
	First[Transpose[refCards/. Rule -> List]];

refFiles = FileNames["*.ref", FileNameJoin[{$FeynCalcDirectory, "Tables", "ReferenceCards"}]]
refCards = {}
Scan[(refCards = Join[refCards, Get[#]];) &, refFiles]


FCPrint[1,"FCShowReferenceCard.m loaded."];
End[]
