(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

(*$FCAdvice = False;*)

ClearAll[itests];
itests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals", "IntegrationTests"}]]
Get/@itests;


If[	$OnlySubTest=!="",
	testNames = "Tests`LoopIntegrals`fcit*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

ClearAll[p,k1,k2,k3,m0,m1,m2,m3,k1z,k2z,k1g,k2g,kst,qst,pst,m,p1,p2,p3,p4,p5];
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

SPD[p1] = 0;
SPD[p2] = 0;
SPD[p3] = 0;
SPD[p4] = 0;
SPD[p5] = 0;

(*	Isolate checks DownValues before generating new abbreviations, which is why
	the results we get from TID can have slightly different form depending on the
	integrals that TID had to work out before. Of course all those are exactly the
	same (just written in a different way) and we use Simplify to check the equivalence	*)

If[ Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisA"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisA"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisB"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisB"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisC"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisC"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisD"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDUsePaVeBasisD"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDFullRedA"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDFullRedA"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDFullRedB"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDFullRedB"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDFullRedCR1"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDFullRedCR1"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDFullRedCR2"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceRepeated[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],p],
		{B1[x__]/;FreeQ[{x},BReduce] :> B1[x, BReduce -> True],B0[x__]/;FreeQ[{x},BReduce] :> B0[x, BReduce -> True]}]],0,testID->#[[1]]]&,
		Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDFullRedCR2"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];


If[ Names["Tests`LoopIntegrals`fcitTIDSTests"]=!={},
	$LimitTo4=True;
	tmpTest = Map[test[ Simplify[ReplaceAll[ToPaVe[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],kst],{B0[x__] :> B0[x, BReduce -> True],
	PaVe[x__, PaVeAutoReduce -> False, y___] :> PaVe[x, PaVeAutoReduce -> True, y]}]],0,testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDSTests"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$LimitTo4=False
];

If[ Names["Tests`LoopIntegrals`fcitTIDMTests"]=!={},
	tmpTest = Map[test[ DiracSimplify[Simplify[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]]],0,testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDMTests"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;

];

If[ Names["Tests`LoopIntegrals`fcitTIDPTests"]=!={},
	$LimitTo4=False;
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&, Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDPTests"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

If[ Names["Tests`LoopIntegrals`fcitOneLoopMiscTests"]=!={},
	$LimitTo4=False;
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&, Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitOneLoopMiscTests"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];




If[ Names["Tests`LoopIntegrals`fcitTIDL"]=!={},
	$LimitTo4=False;
	FCClearScalarProducts[];
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&, Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcitTIDL"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];
