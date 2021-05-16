 
(* ::Section:: *)
(* FCLoopBasisPropagatorsToTopology *)
(* ::Text:: *)
(*FCLoopBasisPropagatorsToTopology[{pr1, pr2, ...}] takes the list of Pairs and FeynAmpDenominators pr1, p2, ... and converts it into a list of propagators that can be used to describe a topology.*)


(* ::Subsection:: *)
(* Examples *)


{FAD[q]}
FCLoopBasisPropagatorsToTopology[%]


{FAD[{q,m}]}
FCLoopBasisPropagatorsToTopology[%]


{FAD[{q,m}],SPD[q,p]}
FCLoopBasisPropagatorsToTopology[%]
