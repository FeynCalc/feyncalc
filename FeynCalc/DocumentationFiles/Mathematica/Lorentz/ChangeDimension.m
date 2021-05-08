 
(* ::Section:: *)
(* ChangeDimension *)
(* ::Text:: *)
(*ChangeDimension[exp, dim] changes all LorentzIndex and Momenta in exp to dimension dim (and also Levi-Civita-tensors, Dirac slashes and Dirac matrices)..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*LorentzIndex, Momentum, DiracGamma, Eps.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*Remember that LorentzIndex[mu, 4] is simplified to LorentzIndex[mu] and Momentum[p, 4] to Momentum[p]. Thus the fullowing objects are defined in four dimensions.*)


{LorentzIndex[\[Mu]], Momentum[p]}

ChangeDimension[%, D]

%//StandardForm


(* ::Text:: *)
(*This changes all non-4-dimensional objects to 4-dimensional ones.*)


ChangeDimension[%%, 4] // StandardForm


(* ::Text:: *)
(*Consider the following list of 4- and D-dimensional object.*)


{GA[\[Mu],\[Nu]] MT[\[Mu],\[Nu]], GAD[\[Mu],\[Nu]] MTD[\[Mu],\[Nu]] f[D]}

DiracTrick/@Contract/@%

DiracTrick/@Contract/@ChangeDimension[%%,n]


(* ::Text:: *)
(*Any explicit occurence of D (like in f(D)) is not replaced by ChangeDimension.*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]

 ChangeDimension[%,D]

Factor2[Contract[%^2]]

Contract[LC[\[Mu],\[Nu],\[Rho],\[Sigma]]^2]
