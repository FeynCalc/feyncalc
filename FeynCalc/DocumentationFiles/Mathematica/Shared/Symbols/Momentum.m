 
(* ::Section:: *)
(* Momentum *)
(* ::Text:: *)
(*Momentum[p] is the head of a four momentum (p). The internal representation of a four-dimensional p is Momentum[p]. For other than four dimensions: Momentum[p, dim]. Momentum[p, 4] simplifies to Momentum[p]..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGamma, Eps, LorentzIndex, MomentumExpand.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*This is a 4-dimensional momentum.*)


Momentum[p]


(* ::Text:: *)
(*As an optional second argument the dimension must be specified if it is different from 4.*)


Momentum[p,D]


(* ::Text:: *)
(*The dimension index is supressed in the output.*)


Momentum[p,d]

Momentum[-q]

%//StandardForm

 Momentum[p-q] + Momentum[2q]

%//StandardForm

%%//MomentumExpand//StandardForm

%%%//MomentumCombine//StandardForm

ChangeDimension[Momentum[p],d]//StandardForm
