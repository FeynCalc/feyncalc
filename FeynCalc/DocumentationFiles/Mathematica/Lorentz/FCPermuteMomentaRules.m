 
(* ::Section:: *)
(* FCPermuteMomentaRules *)
(* ::Text:: *)
(*FCPermuteMomentaRules[{p1, p2, ...}]  returns a set of rules that contain all possible permutations of the momenta p1, p2, ... . This can be useful when working with amplitudes that exhibit a symmetry in some or all of the final state momenta or when trying to find mappings between loop integrals fromdifferent topologies..*)


(* ::Subsection:: *)
(* Examples *)
FCPermuteMomentaRules[{p1,p2}]

f[p1,p2]/.%

FCPermuteMomentaRules[{p1,p2,p3}]

f[p1,p2,p3]/.%
