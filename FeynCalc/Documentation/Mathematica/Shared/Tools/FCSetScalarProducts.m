(* ::Package:: *)

 


(* ::Section:: *)
(*FCSetScalarProducts*)


(* ::Text:: *)
(*`FCSetScalarProducts[]` assigns values in the second list to scalar products (or other kinematic-related symbols such as `Momentum`, `CartesianMomentum`, `TC` etc.) in the first list.*)


(* ::Text:: *)
(*The values can be also modified if the quantities in the first list are entered by hand. To modify the definitions  programmatically without resorting to `With` and similar delayed evaluation tricks one can use placeholders in conjunction with the `InitialSubstitutions` option.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[];
FCSetScalarProducts[{SPD[p1],SPD[p2],SPD[p3,p4]},{0,xx1,xx2}];


{SPD[p1],SPD[p2],SPD[p3,p4]}


FCSetScalarProducts[{spd[p1]}, {val}, InitialSubstitutions -> {spd -> SPD}]
