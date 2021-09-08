(* ::Package:: *)

 


(* ::Section:: *)
(*FCMakeIndex*)


(* ::Text:: *)
(*`FCMakeIndex[str1, str2, head]` generates an index with the given head out of the string `str1` and `str2`. For example, `FCMakeIndex["Lor","1",LorentzIndex]` yields `LorentzIndex[Lor1]`. The second argument can also be an integer. `FCMakeIndex` is useful for converting the output of different diagram generators such as FeynArts or QGAF into the FeynCalc notation. It uses memoization to improve the performance.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCMakeSymbols](FCMakeSymbols.md).*)


(* ::Subsection:: *)
(*Examples*)


FCMakeIndex["Lor","1"]
%//StandardForm


FCMakeIndex["Lor",{3,1,4},LorentzIndex]
%//StandardForm


FCMakeIndex["Sun",{"a",1,-4}]
%//StandardForm



