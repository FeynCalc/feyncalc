 
(* ::Section:: *)
(*FeynAmp*)
(* ::Text:: *)
(*`FeynAmp[q, amp]` is the head of a Feynman amplitude, where amp denotes the analytical expression for the amplitude and q is the integration variable. `FeynAmp[q1, q2, amp]` denotes a two-loop amplitude. `FeynAmp` has no functional properties and serves just as a head. There are however special typesetting rules attached.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Amplitude](Amplitude.md).*)



(* ::Subsection:: *)
(*Examples*)
(* ::Text:: *)
(*This is a 1-loop gluon self-energy amplitude (ignoring factors of (2 \[Pi])).*)


FeynAmp[q,GV[p,\[Mu],a, q-p,\[Alpha],c, -q,\[Beta],e] GP[p-q, \[Alpha],c, \[Rho],d]GV[-p,\[Nu],b, p-q,\[Rho],d, q,\[Sigma],f] GP[q, \[Beta],e, \[Sigma],f]]


(* ::Text:: *)
(*This is a generic 2-loop amplitude.*)


FeynAmp[Subscript[q, 1],Subscript[q, 2],anyexpression]
