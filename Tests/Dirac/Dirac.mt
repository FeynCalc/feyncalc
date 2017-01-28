(* :Title: Dirac.mt															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Dirac" directory				*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Dirac"}]]
Get/@tests;

stingCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

DeclareNonCommutative[FCNCA,FCNCB,FCNCC];

$BreitMaison = False;
$Larin = False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstAnti5*"])];
$BreitMaison = False;
$Larin = False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstChisholm*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstChisholm2*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracSigmaExplicit*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstEpsChisholm*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstToDiracSigma*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracGammaCombine*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracGammaExpand*"])];

$BreitMaison = False;
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracEquation*"])];
$BreitMaison = False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracOrder*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstFCDiracIsolate*"])];


Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracReduce*"])];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
		Join[Tests`Dirac`fcstDiracSimplify]];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),TestID->#[[1]],
		MessagesEquivalenceFunction->stingCompare]&,
		Join[Tests`Dirac`fcstDiracSimplifyDotWarnings]];


$BreitMaison=False;
$Larin=False;
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracTrick*"])];

$BreitMaison=False;
$Larin=False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
		Join[	Tests`Dirac`fcstTr,
				Tests`Dirac`fcstTr4AllFreeNoGamma5,
				Tests`Dirac`fcstTrDAllFreeNoGamma5,
				Tests`Dirac`fcstTr4AllFreeOneGamma5]];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstDiracTrace*"])];


$BreitMaison=False;
$Larin=True;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
		Join[	Tests`Dirac`fcstTrLarin]];


$Larin=False;
$BreitMaison=False;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Dirac`fcstToLarin*"])];


