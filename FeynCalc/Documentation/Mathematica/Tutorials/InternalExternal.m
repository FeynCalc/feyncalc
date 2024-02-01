(* ::Package:: *)

 


(* ::Section:: *)
(*Internal vs. External Representations*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Internal representation*)


(* ::Text:: *)
(*The internal representation (`FeynCalcIntenral` or `FCI`) is how FeynCalc internally "sees" the objects.  For example, a $4$-dimensional $4$-vector is represented by*)


Pair[LorentzIndex[\[Mu]],Momentum[p]]


(* ::Text:: *)
(*Pair is one of the most basic FeynCalc objects. Depending on its arguments, it can represent a $4$-vector, a metric tensor*)


Pair[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]


(* ::Text:: *)
(*or a scalar product of two 4-vectors*)


Pair[Momentum[p],Momentum[q]]


(* ::Text:: *)
(*Another essential object is `DiracGamma` that is used to represent Dirac matrices. An uncontracted Dirac matrix is*)


DiracGamma[LorentzIndex[\[Mu]]]


(* ::Text:: *)
(*and for a Feynman slash we use*)


DiracGamma[Momentum[p]]


(* ::Text:: *)
(*The Levi-Civita-Tensor is*)


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]


(* ::Text:: *)
(*or, when contracted with $4$-momenta*)


Eps[Momentum[p1],Momentum[p2],Momentum[q1],Momentum[q2]]


(* ::Text:: *)
(*This notation (momenta in the index slots) is also used in many other tools (e.g. FORM). The advantage is, that we do not need to canonicalize the indices of the Levi-Civita-Tensor, e.g. to ensure that*)


diff=Eps[LorentzIndex[\[Mu]],Momentum[p2],Momentum[q1],Momentum[q2]]Pair[LorentzIndex[\[Mu]],Momentum[p1]]-
Eps[LorentzIndex[\[Nu]],Momentum[p2],Momentum[q1],Momentum[q2]]Pair[LorentzIndex[\[Nu]],Momentum[p1]]


diff//Contract


(* ::Text:: *)
(*is zero.*)


(* ::Subsection:: *)
(*External representation*)


(* ::Text:: *)
(*The internal representation is useful for the internal programming FeynCalc, but obviously too cumbersome for the user input. This is why FeynCalc also has an external representation (`FeynCalcExternal` or `FCE`), that is concise and convenient.*)


(* ::Text:: *)
(*Let us start with the $4$-vector. In the FCE-notation it is just `FV` ("FourVector")*)


FV[p,\[Mu]]


(* ::Text:: *)
(*It is not hard to guess that the scalar product is `SP`*)


SP[p,q]


(* ::Text:: *)
(*while for the metric tensor we write `MT`*)


MT[\[Mu],\[Nu]]


(* ::Text:: *)
(*To input a Dirac matrix or a Feynman slash, use `GA` or `GS` respectively*)


GA[\[Mu]]


GS[p]


(* ::Text:: *)
(*The Levi-Civita tensor is `LC`*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]


(* ::Text:: *)
(*The fully contracted form is entered via*)


LC[][p1,p2,q1,q2]


(* ::Text:: *)
(*It is also possible to enter a mixed form*)


LC[\[Mu]][p1,p2,q]


LC[\[Mu],\[Nu]][p1,p2]


(* ::Subsection:: *)
(*Switching between the representations*)


(* ::Text:: *)
(*To convert between the two representations we use the functions `FCI` and `FCE`, which are shortcuts for `FeynCalcInternal` and `FeynCalcExternal`. One cannot distinguish between the notations using the typesetting, i.e. when we see a typeset object in the `TraditionalForm`, we cannot really tell if it is in the `FCI` or `FCE` notation.*)


ex1=FV[p,\[Mu]]
ex2=Pair[Momentum[p],LorentzIndex[\[Mu]]]


(* ::Text:: *)
(*However, we can always use StandardForm to see the difference*)


ex1//StandardForm
ex2//StandardForm


(* ::Subsection:: *)
(*Why it matters*)


(* ::Text:: *)
(*All FeynCalc functions that are meant for users will automatically convert the user input in the `FCE` notation into the `FCI` notation. You do not have to do it by yourself.*)


(* ::Text:: *)
(*On the other hand, virtually all FeynCalc functions produce their output in the `FCI` form. So when you have an expression that was obtained from FeynCalc and want to apply some replacement rules to it, we have to use the `FCI` form in the rule*)


ex=Pair[Momentum[p],Momentum[q]]


(* ::Text:: *)
(*No surprise that following does not work*)


ex/.SP[p,q]->1


(* ::Text:: *)
(*But if we wrap the r.h.s of the rule with `FCI`, then everything is fine*)


ex/.FCI[SP[p,q]]->1
