(* ::Package:: *)

 


(* ::Section:: *)
(*Loops*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Propagators*)


(* ::Text:: *)
(*All propagators (and products thereof) appearing in loop diagrams and integrals are represented via `FeynAmpDenominator`*)


(* ::Text:: *)
(*This container is capable of representing different propagator types, where the familiar quadratic propagators of the form $1/(q^2 - m^2 + i \eta)$ are described using `PropagatorDenominator`*)


FeynAmpDenominator[PropagatorDenominator[Momentum[q,D],m]]


(* ::Text:: *)
(*Again, for the external input we always use a shortcut*)


FAD[{q,m}]


FAD[{q,m0},{q+p1,m1},{q+p2,m1}]


(* ::Text:: *)
(*There is also a more versatile symbol called `StandardFeynAmpDenominator` or `SFAD` that allows entering eikonal propagators as well*)


SFAD[{{p, 0}, m^2}]


SFAD[{{p, 0}, {-m^2, -1}}]


SFAD[{{0, p . q}, m^2}]


SFAD[{{p, p . q}, m^2}]


(* ::Text:: *)
(*The presence of `FeynAmpDenominator` in an expression does not automatically mean that it is a loop amplitude. `FeynAmpDenominator` can equally appear in tree level amplitudes, where it stands for the usual 4-dimensional propagator.*)


(* ::Text:: *)
(*In FeynCalc there is no explicit way to distinguish between loop amplitudes and tree-level amplitudes. When you use functions that manipulate loop integrals, you need to tell them explicitly what is your loop momentum.*)


(* ::Subsection:: *)
(*Manipulations of FeynAmpDenominators*)


(* ::Text:: *)
(*There are several functions, that are useful both for tree- and loop-level amplitudes, depending on what we want to do*)


(* ::Text:: *)
(*For example, one can split one `FeynAmpDenominator` into many*)


FAD[{k1-k2},{k1-p2,m},{k2+p2,m}]
FeynAmpDenominatorSplit[%]
%//FCE//StandardForm


(* ::Text:: *)
(*or combine several into one*)


FeynAmpDenominatorCombine[FAD[k1-k2] FAD[{k1-p2,m}] FAD[{k2+p2,m}]]
%//FCE//StandardForm


(* ::Text:: *)
(*At the tree-level we often do not need the `FeynAmpDenominators` but rather want to express everything in terms of explicit scalar products, in order to exploit kinematic simplifications. This is handled by `FeynAmpDenominatorExplicit`*)


FeynAmpDenominatorExplicit[FAD[{k2+p2,m},k1-k2,{k1-p2,m}]]


(* ::Subsection:: *)
(*One-loop tensor reduction*)


(* ::Text:: *)
(*1-loop tensor reduction using Passarino-Veltman method is handled by `TID`*)


FVD[q,\[Mu]]FVD[q,\[Nu]]FAD[{q,m}]
TID[%,q]


int=FVD[q,\[Mu]]SPD[q,p]FAD[{q,m0},{q+p,m1}]
TID[%,q]


(* ::Text:: *)
(*By default, `TID` tries to reduce everything to scalar integrals with unit denominators.*)
(*However, if it encounters zero Gram determinants, it automatically switches to the coefficient functions*)


FCClearScalarProducts[]
SPD[p,p]=0;


TID[int,q]


(* ::Text:: *)
(*If we want the result to be express entirely in terms of Passarino-Veltman function, i. e. without `FAD`s, we can use `ToPaVe`*)


TID[int,q,ToPaVe->True]


(* ::Text:: *)
(*`ToPaVe` is actually also a standalone function, so it can be used independently of `TID`*)


FCClearScalarProducts[]
FAD[q,{q+p1},{q+p2}]
ToPaVe[%,q]


(* ::Text:: *)
(*Even if there are no Gram determinants, for some tensor integrals the full result in terms of scalar integrals is just too large*)


int=FVD[q,\[Mu]]FVD[q,\[Nu]]FAD[q,{q+p1},{q+p2}]
res=TID[int,q];


res//Short


(* ::Text:: *)
(*Of course we collect with respect to `FAD` and isolate the prefactors, but the full result still remains messy*)


Collect2[res,FeynAmpDenominator,IsolateNames->KK]


(* ::Text:: *)
(*In such cases, we can get a much more compact results , if we stick to coefficient functions and do not demand the full reduction to scalars. To do so, use the option `UsePaVeBasis`*)


res=TID[int,q,UsePaVeBasis->True]


(* ::Text:: *)
(*The resulting coefficient functions can be further reduced with `PaVeReduce`*)


pvRes=PaVeReduce[res];


pvRes//Short


(* ::Subsection:: *)
(*Multi-loop tensor reduction*)


(* ::Text:: *)
(*In the case of multi-loop integrals (but also 1-loop integrals with linear propagators) one should use `FCMultiLoopTID`*)


FVD[q,\[Mu]]FVD[q,\[Nu]]SFAD[{q,m^2},{{0,2l . q}}]
FCMultiLoopTID[%,{q}]


(* ::Subsection:: *)
(*Working with GLI and FCTopology symbols*)


(* ::Text:: *)
(*Integral families are encoded in form of `FCTopology[id, {props}, {lmoms}, {extmoms}, kinematics, reserved]` symbols*)


topos={
FCTopology["topoBox1L",{FAD[{q,m0}],FAD[{q+p1,m1}],FAD[{q+p2,m2}],FAD[{q+p2,m3}]},
{q},{p1,p2,p3},{},{}],
FCTopology["topoTad2L",{FAD[{q1,m1}],FAD[{q2,m2}],FAD[{q1-q2,0}]},{q1,q2},{},{},{}]}


(* ::Text:: *)
(*The loop integrals belonging to these topologies are written as `GLI[id, {powers}]` symbols*)


exp=a1 GLI["topoBox1L",{1,1,1,1}]+a2 GLI["topoTad2L",{1,2,2}]


(* ::Text:: *)
(*Using `FCLoopFromGLI` we can convert `GLI`s into explicit propagator notation*)


FCLoopFromGLI[exp,topos]


(* ::Subsection:: *)
(*Topology identification*)


(* ::Text:: *)
(*The very first step is usually to identify the occurring topologies in the amplitude (without attempting to minimize their number)*)


(* ::Text:: *)
(*Find topologies occurring in the 2-loop ghost self-energy amplitude*)


amp=Get[FileNameJoin[{$FeynCalcDirectory,"Documentation","Examples",
"Amplitudes","Gh-Gh-2L.m"}]];


res=FCLoopFindTopologies[amp,{q1,q2}];


res//Last


(* ::Text:: *)
(*The amplitude is the written as a linear combination of special products, where numerators with explicit loop momenta are multiplied by denominators written as `GLI`s*)


res[[1]][[1;;5]]


(* ::Subsection:: *)
(*Finding topology mappings*)


(* ::Text:: *)
(*Here we have a set of 5 topologies*)


topos1={
FCTopology[fctopology1,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology2,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p2-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology3,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology4,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology5,{SFAD[{{p3,0},{0,1},1}],
SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},
{p1,p2,p3},{Q},{},{}]};


(* ::Text:: *)
(*where 3 of them can be mapped to the other 2*)


mappings1=FCLoopFindTopologyMappings[topos1];


mappings1[[1]]


(* ::Text:: *)
(*And these are the final topologies*)


mappings1[[2]]


(* ::Subsection:: *)
(*Tensor reductions with GLIs*)


(* ::Text:: *)
(*Tensor reduction for topologies that have already been processed with `FCLoopFindTopologies` can be done using `FCLoopTensorReduce`*)


topo2=FCTopology[prop1l,{SFAD[{q,m^2},{q-p,m^2}]},{q},{p},{},{}]


amp2=gliProduct[GSD[q] . GAD[\[Mu]] . GSD[q], GLI[prop1l,{1,2}]]


amp2Red=FCLoopTensorReduce[amp2,{topo2},Head->gliProduct]


(* ::Subsection:: *)
(*Applying topology mappings*)


(* ::Text:: *)
(*This is a trial expression representing some loop amplitude that has already been processed using `FCFindTopologies`*)


ex=gliProduct[cc6*SPD[p1,p1],GLI[fctopology1,{1,1,2,1,1,1,1,1,1}]]+
gliProduct[cc2*SPD[p1,p2],GLI[fctopology2,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc4*SPD[p1,p2],GLI[fctopology4,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc1*SPD[p1,Q],GLI[fctopology1,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc3*SPD[p2,p2],GLI[fctopology3,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc5*SPD[p2,Q],GLI[fctopology5,{1,1,1,1,1,1,1,1,1}]]


(* ::Text:: *)
(*These mapping rules describe how the 3 topologies "fctopology3", "fctopology4" and "fctopology5" are mapped to the topologies "fctopology1" and "fctopology2"*)


mappings={
{FCTopology[fctopology3,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],{p1->-p1-p3+Q,p2->
-p2-p3+Q,p3->p3},
GLI[fctopology3,{n1_,n7_,n8_,n5_,n6_,n4_,n2_,n3_,n9_}]:>
GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology4,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},
{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
{p1->-p2+Q,p2->-p1+Q,p3->-p3},
GLI[fctopology4,{n1_,n6_,n5_,n8_,n7_,n3_,n2_,n4_,n9_}]:>
GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology5,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},
{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},
{}],{p1->p2,p2->p1,p3->p3},
GLI[fctopology5,{n1_,n3_,n2_,n4_,n6_,n5_,n7_,n8_,n9_}]:>
GLI[fctopology2,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]}}


(* ::Text:: *)
(*These are the two topologies onto which everything is mapped*)


finalTopos={
FCTopology[fctopology1,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology2,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p2-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}]}


(* ::Text:: *)
(*`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further*)


FCLoopApplyTopologyMappings[ex,{mappings,finalTopos},Head->gliProduct]


(* ::Text:: *)
(*The resulting `GLI`s in the expression are loop integrals that can be IBP-reduced*)


(* ::Subsection:: *)
(*Mappings between integrals*)


(* ::Text:: *)
(*To find one-to-one mappings between loop integrals use `FCLoopFindIntegralMappings`*)


FCClearScalarProducts[]
ClearAll[topo1,topo2]


topos={FCTopology[topo1,{SFAD[{p1,m^2}],SFAD[{p2,m^2}]},{p1,p2},{},{},{}],
FCTopology[topo2,{SFAD[{p3,m^2}],SFAD[{p4,m^2}]},{p3,p4},{},{},{}]}


glis={GLI[topo1,{1,1}],GLI[topo1,{1,2}],GLI[topo1,{2,1}],
GLI[topo2,{1,1}],GLI[topo2,{2,2}]}


mappings=FCLoopFindIntegralMappings[glis,topos]
