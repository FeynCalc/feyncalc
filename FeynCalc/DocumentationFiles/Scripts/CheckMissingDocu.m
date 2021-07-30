(* ::Package:: *)

$FeynCalcStartupMessages=False;
<<FeynCalc`


fcSymbols=Names["FeynCalc`*"];
docFiles=FileBaseName/@FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,"DocumentationFiles","Mathematica"}],Infinity];


Print["FeynCalc symbols missing documentation pages:"];
Print[StringRiffle[Complement[fcSymbols,docFiles],"\n"]];

Print[""]; Print[""];
Print["Documentation pages for nonexisting FeynCalc symbols:"];
Print[StringRiffle[Complement[docFiles,fcSymbols],"\n"]];



