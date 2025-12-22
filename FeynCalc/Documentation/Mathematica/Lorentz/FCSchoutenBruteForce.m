(* ::Package:: *)

 


(* ::Section:: *)
(*FCSchoutenBruteForce*)


(* ::Text:: *)
(*`FCSchoutenBruteForce[exp, {}, {}]`  can be used to show that certain terms are zero by repeatedly applying, Schouten's identity in a brute force way.*)


(* ::Text:: *)
(*The algorithm tries to find replacements which follow from the Schouten's identity and make the length of the given expression shorter.*)


(* ::Text:: *)
(*It is not guaranteed to terminate and in general can often get stuck. Still, with some luck it is often possible to show that certain terms vanish by a sequence of transformations that would be otherwise very difficult to find.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Schouten](Schouten.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*One may not recognize it easily, but the following expression is zero by Schouten's identity*)


FCClearScalarProducts[]

exp=LC[][p1,p2,p3,p4] SP[p5,p6]+LC[][p2,p3,p4,p5] SP[p1,p6]+
LC[][p3,p4,p5,p1] SP[p2,p6]+LC[][p4,p5,p1,p2] SP[p3,p6]-
LC[][p1,p2,p3,p5] SP[p4,p6]


FCSchoutenBruteForce[exp,{},{}]


(* ::Text:: *)
(*Here is a more involved example, where the identity must be applied multiple times*)


exp = SP[q1, q2] (MT[b, k] LC[a, j][q1, q2] - MT[b, j] LC[a, k][q1, q2] - 
   MT[a, k] LC[b, j][q1, q2] + MT[a, j] LC[b, k][q1, q2] + 
   FV[q2, b] LC[a, j, k][q1] - FV[q1, b] LC[a, j, k][q2] - 
   FV[q2, a] LC[b, j, k][q1] + FV[q1, a] LC[b, j, k][q2])


res1 = FCSchoutenBruteForce[exp, {}, {}, 
  SchoutenAllowNegativeGain -> True, SchoutenAllowZeroGain -> True]


FixedPoint[FCSchoutenBruteForce[#, {}, {}] &, res1]



