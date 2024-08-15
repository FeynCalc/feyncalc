## FCLoopFindTopologies

`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies.

Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.

Scaleless topologies are automatically removed, but this can be disabled by setting the option `FCLoopScalelessQ` to `True`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

Find topologies occurring in the 2-loop ghost self-energy amplitude

```mathematica
amp = Get[FileNameJoin[{$FeynCalcDirectory, "Documentation", "Examples", 
      "Amplitudes", "Gh-Gh-2L.m"}]];
```

```mathematica
res = FCLoopFindTopologies[amp, {q1, q2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologies: }\;\text{Your topologies depend on the follwing kinematic invariants that are not all entirely lowercase: }\{\text{Pair[Momentum[p, D], Momentum[p, D]]}\}$$

$$\text{FCLoopFindTopologies: }\;\text{This may lead to issues if these topologies are meant to be processed using tools such as FIRE, KIRA or Fermat.}$$

```mathematica
res // Last
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$

Find topologies occurring in the 2-loop QCD corrections to the $B_s$-meson mixing

```mathematica
topos = Get[FileNameJoin[{$FeynCalcDirectory, "Documentation", "Examples", 
      "Topologies", "BMixingTopos2L.m"}]];
```

```mathematica
topos // Length
```

$$544$$

```mathematica
res = FCLoopFindTopologies[topos, {k1, k2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }18$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }18$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologies: }\;\text{Your topologies depend on the follwing kinematic invariants that are not all entirely lowercase: }\{\text{mb},\text{mc},\text{Pair[Momentum[p1, D], Momentum[p1, D]]}\}$$

$$\text{FCLoopFindTopologies: }\;\text{This may lead to issues if these topologies are meant to be processed using tools such as FIRE, KIRA or Fermat.}$$

Show the first two topologies

```mathematica
(res // Last)[[1 ;; 2]]
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{((\text{k2}+\text{p1})^2+i \eta )},\frac{1}{(\text{k2}^2-\text{mb}^2+i \eta )},\frac{1}{(\text{k1}^2-\text{mc}^2+i \eta )},\frac{1}{((\text{k1}+\text{k2})^2-\text{mc}^2+i \eta )},\frac{1}{((\text{k1}-\text{p1})^2-\text{mc}^2+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\text{p1}\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{((\text{k2}-\text{p1})^2+i \eta )},\frac{1}{(\text{k2}^2-\text{mb}^2+i \eta )},\frac{1}{(\text{k1}^2-\text{mc}^2+i \eta )},\frac{1}{((\text{k1}+\text{p1})^2-\text{mc}^2+i \eta )},\frac{1}{((\text{k1}+\text{k2})^2-\text{mc}^2+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\text{p1}\},\{\},\{\}\right)\right\}$$

In practical calculations even the simple extraction of topologies from the given list of diagrams can take considerable amount of time. This is why it is better to parallelize this process as much as possible.

We can split this part into two steps, were we first apply `FCLoopIsolate` to the resulting amplitudes and then pass the output to `FCLoopFindTopologies`. To avoid the unnecessary application of `FCLoopIsolate` during this step, we should specify the head with which the topologies have been wrapped via the option `FCLoopIsolate`.

```mathematica
isolatedTopos = FCLoopIsolate[topos[[44 ;; 48]], {k1, k2},  Collecting -> False, Factoring -> False, Numerator -> False, Head -> loopDen];
```

```mathematica
res = FCLoopFindTopologies[isolatedTopos, {k1, k2}, FCLoopIsolate -> loopDen, Head -> ampDen, Collecting -> False];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologies: }\;\text{Your topologies depend on the follwing kinematic invariants that are not all entirely lowercase: }\{\text{mb},\text{mc},\text{Pair[Momentum[p1, D], Momentum[p1, D]]}\}$$

$$\text{FCLoopFindTopologies: }\;\text{This may lead to issues if these topologies are meant to be processed using tools such as FIRE, KIRA or Fermat.}$$