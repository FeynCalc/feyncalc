 
(* ::Section:: *)
(*FCGramDeterminant*)
(* ::Text:: *)
(*`FCGramDeterminant[{p1, p2, ...}]` computes the determinant of the Gram matrix created from the given list of momenta.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[FCGramMatrix](FCGramMatrix).*)



(* ::Subsection:: *)
(*Examples*)


FCGramDeterminant[{p1,p2,p3}]


FCGramDeterminant[{p1,p2,p3},Head->{CartesianPair,CartesianMomentum},Dimension->D-1]
