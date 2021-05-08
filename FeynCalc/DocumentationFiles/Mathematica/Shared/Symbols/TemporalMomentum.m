 
(* ::Section:: *)
(* TemporalMomentum *)
(* ::Text:: *)
(*TemporalMomentum[p]  is the head of the temporal component of a four momentum $p^0$. The internal representation of the temporal component $p^0$ is TemporalMomentum[p]. TemporalMomentum may appear only inside TemporalPair.*)


(* ::Subsection:: *)
(* Examples *)
TemporalMomentum[p]

TemporalMomentum[-q]
%//StandardForm


TemporalMomentum[p+q]

%//MomentumExpand//StandardForm

%//MomentumCombine//StandardForm
