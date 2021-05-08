 
(* ::Section:: *)
(* SimplifyPolyLog *)
(* ::Text:: *)
(*SimplifyPolyLog[y] performs several simplifications assuming that the variables occuring in the Log and PolyLog functions are between 0 and 1..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Nielsen.*)



(* ::Subsection:: *)
(* Examples *)



sip[y_] := y == SimplifyPolyLog[y]
sip[PolyLog[2,1/x]]

sip[PolyLog[2,x]]

sip[PolyLog[2,1-x^2]]

sip[PolyLog[2,x^2]]

sip[PolyLog[2,-x/(1-x)]]

sip[PolyLog[2,x/(x-1)]]

sip[Nielsen[1,2,-x/(1-x)]]

sip[PolyLog[3,-1/x]]

sip[PolyLog[3,1-x]]

sip[PolyLog[3,x^2]]

sip[PolyLog[3,-x/(1-x)]]

sip[PolyLog[3,1-1/x]]

sip[PolyLog[4,-x/(1-x)]]

sip[Log[a+b/c]]

sip[Log[1/x]]

sip[ArcTanh[x]]

sip[ArcSinh[x]]

sip[ArcCosh[x]]

Clear[sip]