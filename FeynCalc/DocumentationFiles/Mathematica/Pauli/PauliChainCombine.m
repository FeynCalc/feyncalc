(* ::Package:: *)

 


(* ::Section:: *)
(* PauliChainCombine *)


(* ::Text:: *)
(*`PauliChainCombine[exp]`  is (nearly) the inverse operation to `PauliChainExpand`.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*PauliChain, PCHN, PauliIndex, PauliIndexDelta, DIDelta, PauliChainJoin, PauliChainExpand, PauliChainFactor.*)


(* ::Subsection:: *)
(* Examples *)


(PCHN[CSISD[q],Dir3,Dir4] FAD[{k,me}])/(2 CSPD[q,q])+1/(2 CSPD[q,q]) FAD[k,{k-q,me}] (-2 DCHN[CSISD[q],Dir3,Dir4] CSPD[q,q]+2 DCHN[1,Dir3,Dir4] me CSPD[q,q]+DCHN[CSISD[q],Dir3,Dir4] (-me^2+CSPD[q,q]))
PauliChainCombine[%]
