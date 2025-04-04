(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopTensorReduce*)


(* ::Text:: *)
(*`FCLoopTensorReduce[exp, topos]` performs tensor reduction for the numerators of multi-loop integrals present in `exp`. Notice that `exp` is expected to be the output of `FCLoopFindTopologies` where all loop integrals have been written as `fun[num, GLI[...]]` with `num` being the numerator to be acted upon.*)


(* ::Text:: *)
(*The reduction is done only for loop momenta contracted with Dirac matrices, polarization vectors or Levi-Civita tensors. Scalar products with external momenta are left untouched. The goal is to rewrite everything in terms of scalar products involving only loop momenta and external momenta appearing in the given topology. These quantities can be then rewritten in terms of inverse propagators (`GLI`s with negative indices), so that the complete dependence on loop momenta will go into the `GLI`s.*)


(* ::Text:: *)
(*Unlike `FCMultiLoopTID`, this function does not perform any partial fractioning or shifts in the loop momenta.*)


(* ::Text:: *)
(*The default value for `fun` is  FCGV["GLIProduct"] set by the option `Head`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFindTopologies](FCLoopFindTopologies.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*1-loop tadpole topology*)


topo1=FCTopology["tad1l",{SFAD[{q,m^2}]},{q},{},{},{}]


amp1=FCGV["GLIProduct"][GSD[q] . GAD[\[Mu]] . GSD[q], GLI["tad1l",{1}]]


amp1Red=FCLoopTensorReduce[amp1,{topo1}]


topo2=FCTopology[prop1l,{SFAD[{q,m^2}],SFAD[{q-p,m^2}]},{q},{p},{},{}]


(* ::Text:: *)
(*1-loop self-energy topology*)


amp2=gliProduct[GSD[q] . GAD[\[Mu]] . GSD[q], GLI[prop1l,{1,2}]]


amp2Red=FCLoopTensorReduce[amp2,{topo2},Head->gliProduct]


(* ::Text:: *)
(*If the loop momenta are contracted with some external momenta that do not appear in the given integral topologies, they should be listed via the option `Uncontract`*)


amp3=gliProduct[SPD[q,x], GLI[prop1l,{1,2}]]


FCLoopTensorReduce[amp3,{topo2},Uncontract->{x},Head->gliProduct]


(* ::Text:: *)
(*2-loop self-energy topology*)


topo3=FCTopology["prop2L",{SFAD[{q1,m^2}],SFAD[{q2,m^2}],SFAD[q1-q2],SFAD[q1-p],SFAD[q2-p]},{q1,q2},{p},{},{}]


amp3=FCGV["GLIProduct"][GSD[q1] . GAD[\[Mu]] . GSD[q2], GLI["prop2L",{1,1,1,1,1}]]


amp3Red=FCLoopTensorReduce[amp3,{topo3}]


(* ::Text:: *)
(*Some choices of kinematics lead to the so-called zero Gram determinants, meaning that the external momenta are not linearly independent. This prevents the usual tensor reduction but can be handled via a basis change*)


FCClearScalarProducts[]
SPD[p]=0;


FCLoopTensorReduce[amp3,{topo3}]


(* ::Text:: *)
(*Using `FCLoopFindTensorBasis` we can construct an alternative basis and supply it to the reduction procedure. In this case we need to introduce an auxiliary vector `n`. For simplicity, we choose it to be light-like*)


FCLoopFindTensorBasis[{p},{},n]


(* ::Text:: *)
(*In this case to complete the reduction we need to use IBPs. Furthermore, the existing topology should be augmented to include the new auxiliary vector.*)


amp3red=FCLoopTensorReduce[amp3,{topo3},TensorReductionBasisChange->{{p}->{p,n}},AuxiliaryMomenta->{n},
FinalSubstitutions->{SPD[n]->0}]


{newtopo,gliRule}=FCLoopAugmentTopology[topo3,{SFAD[{{0,q1 . n}}],SFAD[{{0,q2 . n}}]}]


(* ::Text:: *)
(*In this form the expression can be converted into `GLI`s and passed to an IBP reduction tool*)


amp3red/.gliRule


(* ::Text:: *)
(*Not all cases of zero Gram determinants require an auxiliary vector. If the new basis can be constructed from a subset of the present external momenta, this should be sufficient for the reduction. Consider e.g. this threshold kinematics*)


FCClearScalarProducts[]
SPD[p1]=mm;
SPD[p2]=mm;
SPD[p1,p2]=mm;


topo4=FCTopology["tri1l",{SFAD[{q,m^2}],SFAD[{q-p1,0}],SFAD[{q-p2,0}]},{q},{p1,p2},{},{}]


amp4=FCGV["GLIProduct"][GSD[q] . GAD[\[Mu]] . GSD[r], GLI["tri1l",{1,1,1}]]


FCLoopTensorReduce[amp4,{topo4}]


FCLoopFindTensorBasis[{p1,p2},{},n]


(* ::Text:: *)
(*Here the momenta p1 and p2 are obviously identical, so we need to do the reduction w.r.t p1 only*)


FCLoopTensorReduce[amp4,{topo4},TensorReductionBasisChange->{{p1,p2}->{p1}}]
