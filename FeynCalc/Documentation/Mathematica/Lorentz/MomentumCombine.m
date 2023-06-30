(* ::Package:: *)

 


(* ::Section:: *)
(*MomentumCombine*)


(* ::Text:: *)
(*`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [Momentum](Momentum.md), [MomentumExpand](MomentumExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


Momentum[p]-2 Momentum[q] // MomentumCombine // StandardForm


FV[p,\[Mu]] + 2 FV[q,\[Mu]] 

ex=MomentumCombine[%]


ex//StandardForm


ex//ExpandScalarProduct


3 Pair[LorentzIndex[\[Mu]],Momentum[p]]+2 Pair[LorentzIndex[\[Mu]],Momentum[q]]

ex=MomentumCombine[%]


ex//StandardForm


(* ::Text:: *)
(*In some cases one might need a better control over the types of expressions getting combined. For example, the following*)
(*expression will not be combined by default, since the coefficients of scalar products are not numbers*)


DataType[a1,FCVariable]=True;
DataType[a2,FCVariable]=True;


ex=SPD[a1 p,n]+SPD[a2 p,nb]


MomentumCombine[ex]


(* ::Text:: *)
(*Setting the option `NumberQ` to `False` we can still achieve the desired form*)


MomentumCombine[ex,NumberQ->False]


(* ::Text:: *)
(*However, in the following case combing $p^2$ with the other two scalar products is not useful*)


ex=SPD[p]+SPD[a1 p,n]+SPD[a2 p,nb]


MomentumCombine[ex,NumberQ->False]


(* ::Text:: *)
(*To prevent this from happening there is a somewhat hidden option `"Quadratic"` that can be set to `False`*)


MomentumCombine[ex,NumberQ->False,"Quadratic"->False]


ex=SPD[p]+SPD[a1 p,n]+SPD[a2 p,nb]+SPD[p,l]+SPD[p,k]


(* ::Text:: *)
(*In this case we we would like to prevent the scalar products involving `l` and `k` from being combined with*)
(*the rest. To that end we need to use the option `Except`*)


MomentumCombine[ex,NumberQ->False,"Quadratic"->False,Except->{k,l}]
