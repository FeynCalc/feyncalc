(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopExtract												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Extracts unique multiloop integrals from the given expression	*)

(* ------------------------------------------------------------------------ *)

FCLoopExtract::usage =
"FCLoopExtract[expr,{q1,q2,...},loopHead] exctracts loop integrals \
that depend on q1,q2,... from the given expression "  <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCLoopExtract"],
StandardForm]

FCLoopExtract::fail =
"Error! FCLoopExtract failed extract loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopExtract`Private`"]

Options[FCLoopExtract] = {
	Collecting -> True,
	DropScaleless -> False,
	ExpandScalarProduct-> False,
	Full -> False,
	FCI -> False,
	FCE -> False,
	FCLoopIBPReducableQ -> False,
	FCLoopSplit -> {2,3,4},
	FCLoopBasisSplit -> False,
	Factoring -> Factor,
	FeynAmpDenominatorCombine -> True,
	MultiLoop -> False,
	PaVe -> True
};

FCLoopExtract[ex_, lmoms_, loopHead_, OptionsPattern[]] :=
	Block[ {exp, tmp, rel, irrel, rest, loopInts, intsUnique, null1, null2},

		If[	!FreeQ2[{ex}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		(*This is the list of all the loop integrals in the expression.*)
		If[ !OptionValue[FCI],
			exp = FeynCalcInternal[ex],
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
		loopInts = FCLoopIsolate[Plus@@tmp[[rel]], lmoms, FCI->True, Head->loopHead,
									Collecting -> OptionValue[Collecting],
									DropScaleless-> OptionValue[DropScaleless],
									MultiLoop-> OptionValue[MultiLoop],
									PaVe-> OptionValue[PaVe],
									ExpandScalarProduct -> OptionValue[ExpandScalarProduct],
									Full -> OptionValue[Full],
									Factoring->OptionValue[Factoring],
									FCLoopIBPReducableQ->OptionValue[FCLoopIBPReducableQ]];

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
