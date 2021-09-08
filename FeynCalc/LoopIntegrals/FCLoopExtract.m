(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopExtract												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Extracts unique multiloop integrals from the given expression	*)

(* ------------------------------------------------------------------------ *)

FCLoopExtract::usage =
"FCLoopExtract[expr, {q1, q2, ...}, loopHead] exctracts loop integrals that
depend on q1, q2, ... from the given expression. The output is given as a list
of three entries. The first one contains part of the original expression that
consists of irrelevant loop integrals and terms that are free of any loop
integrals. The second entry contains relevant loop integrals, where each
integral is wrapped into loopHead. The third entry is a list of all the unique
loop integrals from the second entry and can be used as an input to another
function. Note that if loop integrals contain free indices, those will not be
canonicalized.";

FCLoopExtract::fail =
"Error! FCLoopExtract failed extract loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopExtract`Private`"]

Options[FCLoopExtract] = {
	CFAD 						-> True,
	Collecting					-> True,
	DropScaleless 				-> False,
	ExpandScalarProduct			-> False,
	Factoring 					-> {Factor2, 5000},
	FAD 						-> True,
	Full						-> False,
	FCI 						-> False,
	FCE 						-> False,
	FCLoopIBPReducableQ 		-> False,
	FCLoopSplit 				-> {2,3,4},
	FCLoopBasisSplit 			-> False,
	Factoring 					-> Factor,
	FeynAmpDenominatorCombine 	-> True,
	GFAD 						-> True,
	MultiLoop 					-> False,
	Numerator 					-> True,
	PaVe 						-> True,
	PaVeIntegralHeads 			-> FeynCalc`Package`PaVeHeadsList,
	SFAD 						-> True,
	TimeConstrained				-> 3
};

FCLoopExtract[ex_, lmoms_, loopHead_, OptionsPattern[]] :=
	Block[ {exp, tmp, rel, irrel, rest, loopInts, intsUnique, null1, null2},

		(*This is the list of all the loop integrals in the expression.*)
		If[ !OptionValue[FCI],
			exp = FCI[ex],
			exp = ex
		];

		If [ OptionValue[FeynAmpDenominatorCombine],
			exp = FeynAmpDenominatorCombine[exp]
		];

		(*	Split loop integrals from the rest	*)
		tmp = FCLoopSplit[exp,lmoms, FCI->True, Collecting->False];

		rel = Sort[OptionValue[FCLoopSplit]];
		irrel = Complement[{1,2,3,4},rel];

		If[	Sort[Join[rel,irrel]]=!={1,2,3,4},
			Message[FCLoopExtract::fail,ToString[exp,InputForm]]
		];

		rest  = Plus@@tmp[[irrel]];
		loopInts = FCLoopIsolate[Plus@@tmp[[rel]], lmoms,
									CFAD 				-> OptionValue[CFAD],
									Collecting			-> OptionValue[Collecting],
									DropScaleless		-> OptionValue[DropScaleless],
									ExpandScalarProduct	-> OptionValue[ExpandScalarProduct],
									FAD					-> OptionValue[FAD],
									FCI					-> True,
									FCLoopIBPReducableQ	-> OptionValue[FCLoopIBPReducableQ],
									Factoring			-> OptionValue[Factoring],
									Factoring			-> OptionValue[Factoring],
									Full				-> OptionValue[Full],
									GFAD				-> OptionValue[GFAD],
									Head				-> loopHead,
									MultiLoop			-> OptionValue[MultiLoop],
									Numerator			-> OptionValue[Numerator],
									PaVe				-> OptionValue[PaVe],
									PaVeIntegralHeads	-> OptionValue[PaVeIntegralHeads],
									SFAD				-> OptionValue[SFAD],
									TimeConstrained		-> OptionValue[TimeConstrained]
		];

		If[ OptionValue[FCLoopBasisSplit],
			loopInts = loopInts/.loopHead[zz_] :> FCLoopBasisSplit[zz,lmoms,List->False,Head->loopHead] /. loopHead[zz_,0]:>loopHead[zz]
		];

		(*	Extract the unique loop integrals	*)
		intsUnique = (Cases[loopInts+null1+null2,loopHead[__],Infinity]/.null1|null2->0)//Union;

		If[	OptionValue[FCE],
			{rest,loopInts,intsUnique} = FCE[{rest,loopInts,intsUnique}]
		];

		{rest,loopInts,intsUnique}
	]

FCPrint[1,"FCLoopExtract.m loaded."];
End[]
