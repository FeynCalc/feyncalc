(* ::Package:: *)



(* :Title: FCAlphaRepresentation											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Converts loop integrals to the alpha representation			*)

(* ------------------------------------------------------------------------ *)


FCAlphaRepresentation::usage = "FCAlphaRepresentation[expr,{q1,q2,...}] converts all the scalar loop integrals that \
depend on the momenta q1,q2,... ";

FCAlphaRepresentation::fail = "FCAlphaRepresentation failed to convert the given integrals to the alpha \
representation."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCAlphaRepresentation`Private`"]

genpave::usage="";

Options[FCAlphaRepresentation] = {
	GenPaVe->False,
	OtherLoopMomenta -> {},
	PaVeAutoOrder -> True,
	PaVeAutoReduce -> True
};

FCAlphaRepresentation[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{ex,loopInt,irrel,rel,repList,res},

		genpave = OptionValue[GenPaVe];

		ex = FCLoopSplit[expr,{q}];
		irrel = ex[[1]]+ex[[3]]+ex[[4]];
		rel = ex[[2]];
		rel = FCLoopIsolate[rel,{q},Head->loopInt];

		repList =
			Union[Cases[{rel},  loopInt[x_] :> Rule[loopInt[x],
				If[	FreeQ2[x,OptionValue[OtherLoopMomenta]],
					toPaVe[x,q,OptionValue[PaVeAutoOrder],OptionValue[PaVeAutoReduce]],
					x
				]
			], Infinity]];
		res = (rel/.repList) + irrel;

		If[!FreeQ[res,toPaVe],
			Messsage[FCAlphaRepresentation::fail];
			Abort[]
		];

		res

	];

FCPrint[1,"FCAlphaRepresentation.m loaded."];
End[]
