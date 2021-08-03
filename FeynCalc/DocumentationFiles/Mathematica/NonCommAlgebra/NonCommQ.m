(* ::Package:: *)

 


(* ::Section:: *)
(*NonCommQ*)


(* ::Text:: *)
(*`NonCommQ[exp]` yields `True` if `exp` contains non-commutative objects (i.e. those objects which are listed in `$NonComm`) not inside `DiracTrace`s or `SUNTrace`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[$NonComm]($NonComm), [NonCommFreeQ](NonCommFreeQ), [DiracTrace](DiracTrace), [SUNTrace](SUNTrace).*)


(* ::Subsection:: *)
(*Examples*)


NonCommQ[xx+yy]


NonCommQ[GA[\[Mu]] . GS[p+m] . GA[\[Mu]]]


NonCommQ[DCHN[GA[\[Mu]],i,j]]
