(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopExtract												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Extracts unique multiloop integrals from the given expression	*)

(* ------------------------------------------------------------------------ *)

FCLoopExtract::usage =
"FCLoopExtract[expr,{q1,q2,...},loopHead] exctracts loop integrals \
that depend on q1,q2,... from the given expression. The output is given as a list \
of three entries. The first one contains part of the original expression that consists of \
irrelevant loop integrals and terms that are free of any loop integrals. The second entry \
contains relevant loop integrals, where each integral is wrapped into loopHead. The third
entry is a list of all the unique loop integrals from the second entry and can be used as \
an input to another function. Note that if loop integrals contain free indices, those \
will not be canonicalized."

FCLoopExtract::fail =
"Error! FCLoopExtract failed extract loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopExtract`Private`"]

Options[FCLoopExtract] = {
	DropScaleless -> False,
	FCI -> False,
	FCLoopSplit -> {2,3,4},
	FeynAmpDenominatorCombine -> True,
	MultiLoop -> False,
	PaVe -> True
};

FCLoopExtract[ex_, lmoms_, loopHead_, OptionsPattern[]] :=
	Block[ {exp, tmp, rel, irrel, rest, loopInts, intsUnique, null1, null2},

		(*This is the list of all the loop integrals in the expression.*)
		If[ !OptionValue[FCI],
			exp = FeynCalcInternal[ex],
			exp = ex
		];

		If [ OptionValue[FeynAmpDenominatorCombine],
			exp = FeynAmpDenominatorCombine[exp]
		];

		(*	Split loop integrals from the rest	*)
		tmp = FCLoopSplit[exp,lmoms, FCI->True];

		rel = Sort[OptionValue[FCLoopSplit]];
		irrel = Complement[{1,2,3,4},rel];

		If[	Sort[Join[rel,irrel]]=!={1,2,3,4},
			Message[FCLoopExtract::fail,ToString[exp,InputForm]]
		];

		rest  = Plus@@tmp[[irrel]];
		loopInts = FCLoopIsolate[Plus@@tmp[[rel]], lmoms, FCI->True, Head->loopHead,
									DropScaleless-> OptionValue[DropScaleless],
									MultiLoop-> OptionValue[MultiLoop],
									PaVe-> OptionValue[PaVe],
									ExpandScalarProduct -> False];


		(*	Extract unique loop integrals	*)
		intsUnique = (Cases[loopInts+null1+null2,loopHead[__],Infinity]/.null1|null2->0)//Union;

		{rest,loopInts,intsUnique}
	]

FCPrint[1,"FCLoopExtract.m loaded."];
End[]
