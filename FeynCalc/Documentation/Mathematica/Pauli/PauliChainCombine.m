(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainCombine*)


(* ::Text:: *)
(*`PauliChainCombine[exp]`  is (nearly) the inverse operation to `PauliChainExpand`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [DIDelta](DIDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


(PCHN[CSISD[q],Dir3,Dir4] FAD[{k,me}])/(2 CSPD[q,q])+1/(2 CSPD[q,q]) FAD[k,{k-q,me}] (-2 DCHN[CSISD[q],Dir3,Dir4] CSPD[q,q]+2 DCHN[1,Dir3,Dir4] me CSPD[q,q]+DCHN[CSISD[q],Dir3,Dir4] (-me^2+CSPD[q,q]))
PauliChainCombine[%]
