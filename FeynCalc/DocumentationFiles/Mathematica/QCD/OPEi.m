(* ::Package:: *)

(* ::Section:: *)
(*OPEi*)


(* ::Text:: *)
(*`OPEi` etc. are variables with `DataType` `PositiveInteger` which are used in functions relating to the operator product expansion.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[OPEj](OPEj), [OPEk](OPEk), [OPEl](OPEl), [OPEn](OPEn), [OPEo](OPEo).*)


(* ::Subsection:: *)
(*Examples*)


OPEi


DataType[OPEi, OPEj,OPEk,OPEl, OPEm, OPEn, OPEo, PositiveInteger]


PowerSimplify[{(-1)^(2OPEi),(-1)^(2OPEj),(-1)^(2OPEk),(-1)^(2OPEl),(-1)^(2OPEm),(-1)^(2OPEn),(-1)^(2OPEo)}]


(* ::Text:: *)
(*Re has been changed:*)


{Re[OPEi]>-3, Re[OPEi]>-2, Re[OPEi]>-1,   Re[OPEi]>0, Re[OPEi]>1}


{Re[-OPEi + OPEm] > 0, Re[-OPEi + OPEm] > 1,Re[-OPEi + OPEm] > 2}


{Re[OPEm]>-3, Re[OPEm]>-2, Re[OPEm]>-1,   Re[OPEm]>0, Re[OPEm]>1}
