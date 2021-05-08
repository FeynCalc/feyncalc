 
(* ::Section:: *)
(* DCHN *)
(* ::Text:: *)
(*DCHN[x, i, j] is a chain of Dirac matrices x and is transformed into DiracChain[FCI[x],DiracIndex[i],DiracIndex[j]] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, DiracIndex, DiracIndexDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*A standalone Dirac matrix with open Dirac indices*)


DCHN[GAD[\[Mu]],i,j]


(* ::Text:: *)
(*A chain of Dirac matrices with open Dirac indices*)


DCHN[GAD[\[Mu]].GAD[\[Nu]],i,j]


(* ::Text:: *)
(*A single $overset{-}{u}$ spinor with an open Dirac index*)


DCHN[SpinorUBar[p,m],i]


(* ::Text:: *)
(*A single $overset{-}{v}$ spinor with an open Dirac index*)


DCHN[SpinorVBar[p,m],i]


(* ::Text:: *)
(*A single $text{u}$ spinor with an open Dirac index*)


DCHN[i,SpinorU[p,m]]


(* ::Text:: *)
(*A single $text{v}$ spinor with an open Dirac index*)


DCHN[i,SpinorV[p,m]]


(* ::Text:: *)
(* $overset{-}{u}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]].GAD[\[Nu]],SpinorUBar[p,m],j]


(* ::Text:: *)
(* $overset{-}{v}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]].GAD[\[Nu]],SpinorVBar[p,m],j]


(* ::Text:: *)
(* $text{u}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]].GAD[\[Nu]],i,SpinorU[p,m]]


(* ::Text:: *)
(* $text{v}$ spinor contracted with a chain of Dirac matrices*)


DCHN[GAD[\[Mu]].GAD[\[Nu]],i,SpinorV[p,m]]
