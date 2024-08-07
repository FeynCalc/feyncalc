(* ::Package:: *)

 


(* ::Section:: *)
(*MomentumCombine*)


(* ::Text:: *)
(*`MomentumCombine[expr]` is the inverse operation to `MomentumExpand` and `ExpandScalarProduct`. `MomentumCombine` combines also `Pair`s.*)
(*Notice, that `MomentumCombine` cannot complete squares. It can, however, bring expressions containing scalar products to a suitable form that allows for a square completion using other means.*)


(* ::Text:: *)
(*This function offers multiple options.*)


(* ::Text:: *)
(*The option `NumberQ` (default is `True`) specifies whether one should only merge quantities with numerical prefactors or not. Setting it to `False` allows for symbolic prefactors.*)


(* ::Text:: *)
(*Setting the option `"Quadratic"` to `False` (default is `True`) effectively means that momenta squared will not be combined with anything else.*)


(* ::Text:: *)
(*With the option `"ExcludeScalarProducts"` we can ensure that scalar products containing any of the momenta listed are not merged with anything else. So `a.x + a.y` can be merged either if `a` contains no such momenta, or if both `x` and `y` are free of them.*)


(* ::Text:: *)
(*The option `Except` forbids merging the listed momenta with anything else. It is much more restrictive than `"ExcludeScalarProducts"` that allows for merging terms linear in the listed momenta.*)


(* ::Text:: *)
(*The option `Select` allows for gathering all terms linear in the given momenta before applying any other combining rules.*)


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


(* ::Text:: *)
(*Suppose that we have an expression that can be written as a square. To achieve the desired combination of momenta we need to*)


(DataType[#,FCVariable]=True)&/@{gkin,meta,u0b};


ex=SPD[k1,k1]-2 SPD[k1,k2]+2 gkin meta SPD[k1,n]-2 gkin meta u0b SPD[k1,n]-meta u0b SPD[k1,nb]+
SPD[k2,k2]-2 gkin meta SPD[k2,n]+2 gkin meta u0b SPD[k2,n]+meta u0b SPD[k2,nb]


(* ::Text:: *)
(*The naive application of `MomentumCombine` doesn't return anything useful*)


MomentumCombine[ex]


(* ::Text:: *)
(*Here we actually want to gather terms linear in `k1` and `k2`first before trying to combine them together. To that aim we can use the option `Select`.*)
(*Employing the options `"Quadratic"` and `"ExcludeScalarProducts"` we can prevent `k1` and `k2` from getting combined with anything containing*)
(*those momenta. Furthermore, we enable symbolical prefactor by setting `NumberQ` to false*)


MomentumCombine[ex,Select->{k1,k2},"Quadratic"->False,"ExcludeScalarProducts"->{k1,k2},NumberQ->False]


(* ::Text:: *)
(*This result looks very good, but `k1` and `k2` were not combined because they are contracted to long linear combinations of 4-momenta that were not properly factorized. The option `Factoring` solves this issue*)


res=MomentumCombine[ex,Select->{k1,k2},"Quadratic"->False,"ExcludeScalarProducts"->{k1,k2},NumberQ->False,Factoring->Factor2]
