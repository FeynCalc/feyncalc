(* ::Package:: *)

 


(* ::Section:: *)
(*FRH2*)


(* ::Text:: *)
(*`FRH2[exp_, isoNames_]` is similar `FRH` but is specifically designed to reinsert abbreviations introduced by `Collect2` running in parallel mode.*)


(* ::Text:: *)
(*In such cases the user needs to set the `IsolateNames` option to a list containing  as many elements as there are parallel kernels. Then, each parallel kernel introduces its own set of abbreviations that are not known to other kernels. `FRH2` takes the value of the `IsolateNames` option as its second arguments, fetches abbreviation definitions from each parallel kernel and finally substitutes them back into `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md), [FRH](FRH.md),  [Isolate](Isolate.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This code needs to be run on FeynCalc in parallel mode with parallel kernels available!*)


(*isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]*)


(*exp=Table[Sum[a[i]b[j],{i,1,10}],{j,1,8}]*)


(*aux=Collect2[exp,b,IsolateNames->isoSymbols,FCParallelize->True]*)


(*FRH2[aux,isoSymbols]*)
