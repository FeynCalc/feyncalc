(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
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

ScalarProduct[pv1, pv1] = 0;
ScalarProduct[pv2, pv2] = 0;
ScalarProduct[pv3, pv3] = 0;
ScalarProduct[pv4, pv4] = 0;
ScalarProduct[pv1, pv2] = s/2;
ScalarProduct[pv1, pv3] = 0;
ScalarProduct[pv2, pv3] = s/2;

(*	Isolate checks DownValues before generating new abbreviations, which is why
	the results we get from TID can have slightly different form depending on the
	integrals that TID had to work out before. Of course all those are exactly the
	same (just written in a different way) and we use Simplify to check the equivalence	*)
$LimitTo4=True;
Map[Test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisA,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisB,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisC,
		Tests`LoopIntegrals`fcitTIDUsePaVeBasisD,
		Tests`LoopIntegrals`fcitTIDFullRedA,
		Tests`LoopIntegrals`fcitTIDFullRedB,
		Tests`LoopIntegrals`fcitTIDFullRedCR1,
		Tests`LoopIntegrals`fcitTIDFullRedCR2
		]];

Map[Test[ Simplify[ReplaceAll[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],kst],{B0[x__] :> B0[x, BReduce -> True],
	PaVe[x__, PaVeAutoReduce -> False, y___] :> PaVe[x, PaVeAutoReduce -> True, y]}]],0,TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDSTests
		]];



Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDMTests
		]];
$LimitTo4=False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitTIDPTests
		]];


Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[
		Tests`LoopIntegrals`fcitOneLoopMiscTests
		]];










