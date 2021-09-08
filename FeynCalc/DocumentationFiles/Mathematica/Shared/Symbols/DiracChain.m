(* ::Package:: *)

 


(* ::Section:: *)
(*DiracChain*)


(* ::Text:: *)
(*`DiracChain[x, i, j]` denotes a chain of Dirac matrices `x`, where the Dirac indices `i` and `j` are explicit.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A standalone Dirac matrix*)


DiracChain[DiracGamma[LorentzIndex[\[Mu]]],DiracIndex[i],DiracIndex[j]]


(* ::Text:: *)
(*A chain of Dirac matrices with open indices*)


DiracChain[DiracGamma[LorentzIndex[\[Mu],D],D] . DiracGamma[LorentzIndex[\[Nu],D],D],DiracIndex[i],DiracIndex[j]]


(* ::Text:: *)
(*A DiracChain with only two arguments denotes a spinor component*)


DiracChain[Spinor[Momentum[p],m],DiracIndex[i]]


DiracChain[Spinor[Momentum[-p],m],DiracIndex[i]]


DiracChain[DiracIndex[i],Spinor[Momentum[p],m]]


DiracChain[DiracIndex[i],Spinor[Momentum[-p],m]]


(* ::Text:: *)
(*The chain may also be partially open or closed*)


DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m+DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]],Spinor[Momentum[p],m,1],DiracIndex[j]]


DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m+DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]],DiracIndex[i],Spinor[Momentum[p],m,1]]


DiracChain[DiracGamma[LorentzIndex[\[Mu]]] . (m+DiracGamma[Momentum[p]]) . DiracGamma[LorentzIndex[\[Nu]]],Spinor[Momentum[p1],m1,1],Spinor[Momentum[p2],m2,1]]


DiracChain[1,Spinor[Momentum[p1],m1,1],Spinor[Momentum[p2],m2,1]]
