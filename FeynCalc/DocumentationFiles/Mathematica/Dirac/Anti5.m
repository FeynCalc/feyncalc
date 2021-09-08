 
(* ::Section:: *)
(*Anti5*)
(* ::Text:: *)
(*`Anti5[exp]` anticommutes all $\gamma^5$ in exp to the right. `Anti5[exp, n]` anticommutes all $\gamma^5$ $n$-times to the right. `Anti5[exp, -n]` anticommutes all $\gamma^5$ $n$-times to the left.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracOrder](DiracOrder.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).*)



(* ::Subsection:: *)
(*Examples*)


GA[5,\[Mu]] 
Anti5[%]
Anti5[%,-1]


GA[5,\[Alpha],\[Beta],\[Gamma],\[Delta]]
Anti5[%,2]
Anti5[%%,Infinity]
Anti5[%,-Infinity]


(* ::Text:: *)
(*In the naive $\gamma^5$-scheme $D$-dimensional $\gamma$-matrices anticommute with $\gamma^5$.*)


GA5.GAD[\[Mu]]
Anti5[%]


(* ::Text:: *)
(*`Anti5` also works in the t'Hooft-Veltman-Breitenlohner-Maison scheme*)


FCSetDiracGammaScheme["BMHV"];
Anti5[GA5.GAD[\[Mu]]]


FCSetDiracGammaScheme["NDR"];
