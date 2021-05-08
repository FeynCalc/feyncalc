 
(* ::Section:: *)
(* DiracGamma *)
(* ::Text:: *)
(*DiracGamma[x, dim] is the head of all Dirac matrices and slashes (in the internal representation). Use GA, GAD, GS or GSD for manual (short) input.DiracGamma[x, 4] simplifies to DiracGamma[x].DiracGamma[5] is $gamma ^5$DiracGamma[6] is $left.left(1+gamma ^5right)right/2.$ DiracGamma[7] is $left.left(1-gamma ^5right)right/2.$.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGammaExpand, GA, DiracSimplify, GS, DiracTrick.*)



(* ::Subsection:: *)
(* Examples *)



DiracGamma[5]

DiracGamma[LorentzIndex[\[Alpha]]]


(* ::Text:: *)
(*A Dirac-slash, i.e., $gamma ^{mu }q_{mu }$, is displayed as $gamma cdot q$.*)


DiracGamma[Momentum[q]] 

DiracGamma[Momentum[q]] . DiracGamma[Momentum[p-q]]

DiracGamma[Momentum[q,D],D] 

GS[p-q].GS[p]

DiracGammaExpand[%]

GAD[\[Mu]].GSD[p-q].GSD[q].GAD[\[Mu]]

DiracTrick[%]

DiracSimplify[%%]


(* ::Text:: *)
(*DiracGamma may also carry Cartesian indices or appear contracted with Cartesian momenta.*)


DiracGamma[CartesianIndex[i]]

DiracGamma[CartesianIndex[i,D-1],D]

DiracGamma[CartesianMomentum[p]]

DiracGamma[CartesianMomentum[p,D-1],D]


(* ::Text:: *)
(*Temporal indices are represented using ExplicitLorentzIndex[0]*)


DiracGamma[ExplicitLorentzIndex[0]]
