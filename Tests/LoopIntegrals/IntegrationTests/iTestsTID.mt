(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

(*$FCAdvice = False;*)

ClearAll[itests];
itests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals", "IntegrationTests"}]]
Get/@itests;
ClearAll[p,k1,k2,k3,m0,m1,m2,m3,k1z,k2z,k1g,k2g,kst,qst,pst,m];
FCClearScalarProducts[];
ScalarProduct[k1z, k1z] = 0;
ScalarProduct[k2z, k2z] = 0;
ScalarProduct[k1g, k2g] = 0;
ScalarProduct[k1g, k1g] = 0;
ScalarProduct[k2g, k2g] = 0;

ScalarProduct[qst, qst] = 0;
ScalarProduct[pst, pst] = m^2;
ScalarProduct[qst, pst] = (m^2)/2;

(*	Isolate checks DownValues before generating new abbreviations, which is why
	the results we get from TID can have slightly different form depending on the
	integrals that TID had to work out before. Of course all those are exactly the
	same (just written in a different way) and we use Simplify to check the equivalence	*)

Map[Test[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],0,TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisA,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisB,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisC,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisD,
		Tests`LoopIntegrals`fcitTIDFullRedA,
		Tests`LoopIntegrals`fcitTIDFullRedB,
		Tests`LoopIntegrals`fcitTIDFullRedCR1,
		Tests`LoopIntegrals`fcitTIDFullRedCR2,
		Tests`LoopIntegrals`fcitTIDSTests
		]];

FCClearScalarProducts[];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDMTests
		]];
