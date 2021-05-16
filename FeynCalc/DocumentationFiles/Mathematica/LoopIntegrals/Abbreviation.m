 
(* ::Section:: *)
(* Abbreviation *)
(* ::Text:: *)
(*Abbreviation is a function used by OneLoop and PaVeReduce for generating smaller files when saving results to the hard disk. The convention is that a definition like GP = GluonPropagator should be accompanied by the definition Abbreviation[GluonPropagator] = HoldForm[GP]..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*$Abbreviations, OneLoop, PaVeReduce, WriteOut, WriteOutPaVe, GluonPropagator, GluonVertex, QuarkPropagator.*)



(* ::Subsection:: *)
(* Examples *)


GP[p, {\[Mu], a}, {\[Nu], b}]
