(* ::Package:: *)

$FeynCalcStartupMessages=False;
<<FeynCalc`


Needs["CodeInspector`"]


dirs={"Dirac","ExportImport","Feynman","LoopIntegrals","Lorentz","NonCommAlgebra",
"Pauli","QCD","Shared","SUN","Tables"};


files=Map[File/@(FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,#}],Infinity])&,dirs];


checkFu[x_]:=CodeInspectSummarize[#,"TagExclusions"->{"ImplicitTimesBlanks"}]&/@x;


res=MapThread[(Print["Checking files in ", #1]; checkFu[#2])&, {dirs,files}];


aux=Map[SelectFree[#,Text["No issues."]]&,res]/.{InspectedFileObject[FileNameJoin[{$FeynCalcDirectory,"Lorentz","ScalarProduct.m"}], xx__]}/;!FreeQ[{xx},"ImplicitTimesBlanks"]:>Unevaluated[Sequence[]]


Print["Result:", aux];
