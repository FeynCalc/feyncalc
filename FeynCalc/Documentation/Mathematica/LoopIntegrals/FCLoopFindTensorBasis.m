(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindTensorBasis*)


(* ::Text:: *)
(*`FCLoopFindTensorBasis[{moms}, {rules}, n]` checks if the external momenta `moms` form a basis by calculating their Gram determinant and inserting the supplied rules for the scalar products.*)


(* ::Text:: *)
(*A vanishing Gram determinant signals a linear dependence between those momenta. In this case a loop integral depending on these momenta cannot be tensor reduced in the usual way.*)


(* ::Text:: *)
(*To circumvent this issue the function will suggest an alternative set of external vectors with respect to which the tensor reduction should be done. If some of the old vectors can be expressed in terms of the new ones, the corresponding rules will be provided as well.*)


(* ::Text:: *)
(*If some of the external momenta are light-like (i.e. their scalar products vanish), then an auxiliary vector `n` must be added to the basis. The scalar products of this vector with the existing momenta will form new kinematic invariants appearing in the result of the tensor reduction. The values of these invariants can be arbitrary, except that they must be nonvanishing. Upon doing the tensor reduction in this way, one will still need to perform an IBP reduction of the resulting scalar integrals. These integrals will depend on the new kinematic invariants but as the invariants should cancel in the final result for the reduced tensor integral. To see this cancellation explicitly one might need to use the linear relations between the external momenta uncovered by `FCLoopFindTensorBasis`*)


(* ::Text:: *)
(*Using the option `All` one can get all possible sets of new basis vectors. This can be useful if one needs to select one of them for the tensor reduction.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopAugmentTopology](FCLoopAugmentTopology.md), [FCLoopTensorReduce](FCLoopTensorReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*One light-like momentum. Here we need to add an auxiliary vector to our basis. There are no linearly dependent vectors*)


FCLoopFindTensorBasis[{k1},{SPD[k1]->0},n,Prefactor->pref]


(* ::Text:: *)
(*Two light-like momenta. Apart from constructing a new basis that contains an auxiliary vector, we also notice that `k2` can be expressed through `k1`*)


FCLoopFindTensorBasis[{k1,k2},
{SPD[k1]->0,SPD[k2]->0,SPD[k1,k2]->0},n,Prefactor->pref]


(* ::Text:: *)
(*Of course `{k1,n}` is not the only possible choice*)


FCLoopFindTensorBasis[{k1,k2},
{SPD[k1]->0,SPD[k2]->0,SPD[k1,k2]->0},n,Prefactor->pref,All->True]


(* ::Text:: *)
(*Here we have an interesting combination of 3 vectors. The kinematics is chosen such, that `k2` and `k3` actually turn out to be identically zero. Notice that this is possible only if `k1` or `k2` are chosen to be the new basis vectors. Selecting `k3` will require us to add an auxiliary vector to the basis.*)


FCLoopFindTensorBasis[{k1,k2,k3},{SPD[k1]->0,SPD[k2]->0,SPD[k3]->0,
SPD[k1,k3]->0,SPD[k1,k2]->c,SPD[k2,k3]->0},n,All->True]


(* ::Text:: *)
(*If for some reason, one one would like to avoid a choice where external momenta should be explicitly set to zero, one can use a special option "NoZeroVectors"*)


FCLoopFindTensorBasis[{k1,k2,k3},{SPD[k1]->0,SPD[k2]->0,SPD[k3]->0,
SPD[k1,k3]->0,SPD[k1,k2]->c,SPD[k2,k3]->0},n,All->True,"NoZeroVectors"->True]


(* ::Text:: *)
(*Of course, the routine also works for cases that do not involve any light-like momenta*)


FCLoopFindTensorBasis[{k1,k2},{SPD[k1]->c^2,SPD[k2]->d^2,SPD[k1,k2]-> c d },n]


FCLoopFindTensorBasis[{k1,k2},{SPD[k1]->m2,SPD[k2]->m2,SPD[k1,k2]-> m2 },n]


(* ::Text:: *)
(*Cartesian momenta also supported too*)


FCLoopFindTensorBasis[{k1,k2},{CSPD[k1]->m2,CSPD[k2]->m2,CSPD[k1,k2]-> m2 },n,
Head->{CartesianPair,CartesianMomentum}]
