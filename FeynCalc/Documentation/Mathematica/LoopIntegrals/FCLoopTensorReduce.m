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


amp1=FCGV["GLIProduct"][GSD[q].GAD[\[Mu]].GSD[q], GLI["tad1l",{1}]]


amp1Red=FCLoopTensorReduce[amp1,{topo1}]


topo2=FCTopology[prop1l,{SFAD[{q,m^2},{q-p,m^2}]},{q},{p},{},{}]


(* ::Text:: *)
(*1-loop self-energy topology*)


amp2=gliProduct[GSD[q].GAD[\[Mu]].GSD[q], GLI[prop1l,{1,2}]]


amp2Red=FCLoopTensorReduce[amp2,{topo2},Head->gliProduct]


(* ::Text:: *)
(*If the loop momenta are contracted with some external momenta that do not appear in the given integral topologies,*)
(*they should be listed via the option `Uncontract`*)


amp3=gliProduct[SPD[q,x], GLI[prop1l,{1,2}]]


FCLoopTensorReduce[amp3,{topo2},Uncontract->{x},Head->gliProduct]
