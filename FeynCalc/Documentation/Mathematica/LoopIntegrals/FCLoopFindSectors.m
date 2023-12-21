(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindSectors*)


(* ::Text:: *)
(*`FCLoopFindSectors[{GLI[...], ...}]` analyzes the indices of the GLI integrals in the given list and identifies sectors to which they belong. Notice that only `GLI`s with integer indices are supported.*)


(* ::Text:: *)
(*If the option `GatherBy` is set to `True` (default), the output will be a list of two lists, where the former contains the original integrals sorted w.r.t the identified sectors, while the latter is a list of all available sectors.*)
(**)
(*For `GatherBy->False`, the output is a list containing all identified sectors without the original integrals.*)


(* ::Text:: *)
(*Setting the option `Last` to `True`will return only the top sector.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)


ints={
GLI[topo1,{1,1,1,1}],
GLI[topo1,{2,1,2,1}],
GLI[topo2,{1,0,1,1}],
GLI[topo3,{1,0,1,-1}]
};


FCLoopFindSectors[ints]


FCLoopFindSectors[ints,Last->True]


FCLoopFindSectors[ints,GatherBy->False]


FCLoopFindSectors[ints,GatherBy->False,Last->True]
