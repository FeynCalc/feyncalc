 
(* ::Section:: *)
(* FCSetMetricSignature *)
(* ::Text:: *)
(*FCSetMetricSignature sets the signature of the Minkowski metric used when working with Cartesian objects, like CartesianPair, CartesianIndex, CartesianMomentum etc. The default choice is (1,-1,-1,-1) which corresponds to FCSetMetricSignature[{1,-1}].*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FCGetMetricSignature.*)



(* ::Subsection:: *)
(* Examples *)



FCSetMetricSignature[{-1,1}]
SPD[p,q]//LorentzToCartesian

FCSetMetricSignature[{1,-1}]
SPD[p,q]//LorentzToCartesian
