(* ::Package:: *)

 


(* ::Section:: *)
(*DCHN*)


(* ::Text:: *)
(*`DCHN[x, i, j]` is a chain of Dirac matrices x and is transformed into `DiracChain[FCI[x],DiracIndex[i],DiracIndex[j]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A standalone Dirac matrix with open Dirac indices*)


DCHN[GAD[\[Mu]],i,j]


(* ::Text:: *)
(*A chain of Dirac matrices with open Dirac indices*)


DCHN[GAD[\[Mu]] . GAD[\[Nu]],i,j]


(* ::Text:: *)
(*A single $\bar{u}$ spinor with an open Dirac index*)


DCHN[SpinorUBar[p,m],i]


(* ::Text:: *)
(*A single $\bar{v}$ spinor with an open Dirac index*)


DCHN[SpinorVBar[p,m],i]


(* ::Text:: *)
(*A single $u$ spinor with an open Dirac index*)


DCHN[i,SpinorU[p,m]]


(* ::Text:: *)
(*A single $v$ spinor with an open Dirac index*)


DCHN[i,SpinorV[p,m]]


(* ::Text:: *)
(*$\bar{u}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]] . GAD[\[Nu]],SpinorUBar[p,m],j]


(* ::Text:: *)
(*$\bar{v}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]] . GAD[\[Nu]],SpinorVBar[p,m],j]


(* ::Text:: *)
(* $u$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]] . GAD[\[Nu]],i,SpinorU[p,m]]


(* ::Text:: *)
(* $v$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]] . GAD[\[Nu]],i,SpinorV[p,m]]
