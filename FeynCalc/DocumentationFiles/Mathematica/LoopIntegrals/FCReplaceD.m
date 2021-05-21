 
(* ::Section:: *)
(* FCReplaceD *)
(* ::Text:: *)
(*`FCReplaceD[expr, rule]` replaces `D` in expr accoding to the supplied replacement rule (e.g. `D -> 4 - 2*Epsilon`) but doesn't touch `D` inside `Pair`s and `DiracGamma`s, i.e the dimension of scalar products, metric tensors and Dirac matrices is unchanged. The latter can and should be done via `ChangeDimension`.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Applying the replacement rule directly to the expression doesn't give the desired result*)



(* ::Subsection:: *)
(* Examples *)



FCI[D MTD[\[Mu],\[Nu]]]
%/.D->4-2Epsilon


(* ::Text:: *)
(*With `FCReplaceD` we get what we want*)


FCReplaceD[D MTD[\[Mu],\[Nu]],D->4-2Epsilon]
ChangeDimension[%,4]
