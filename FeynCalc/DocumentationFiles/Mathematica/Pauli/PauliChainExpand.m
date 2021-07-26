(* ::Package:: *)

 


(* ::Section:: *)
(*PauliChainExpand*)


(* ::Text:: *)
(*`PauliChainExpand[exp]` expands all Pauli chains with explicit indices using linearity, e.g. `PCHN[CSIS[p1]+CSIS[p2]+m,i,j]` becomes `PCHN[CSIS[p1],i,j]+PCHN[CSIS[p2],i,j]+m*PCHN[1,i,j]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [PauliIndexDelta](PauliIndexDelta), [DIDelta](DIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainFactor](PauliChainFactor).*)


(* ::Subsection:: *)
(*Examples*)


PCHN[(CSIS[p]+m) . CSI[a],i,j]
PauliChainExpand[%]
