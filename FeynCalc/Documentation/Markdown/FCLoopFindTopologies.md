`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies. Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.

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

$$\text{Number of the initial candidate topologies: }3$$

$$\text{Number of the identified unique topologies: }3$$

$$\text{Number of the preferred topologies among the unique topologies: }0$$

$$\text{Number of the identified subtopologies: }0$$

```mathematica
res // Last
```

$$\left\{\text{FCTopology}\left(\text{fctopology1},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((\text{q1}+\text{q2})^2+i \eta )},\frac{1}{((p+\text{q1})^2+i \eta )},\frac{1}{((p-\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology2},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p+\text{q2})^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fctopology3},\left\{\frac{1}{(\text{q2}^2+i \eta )},\frac{1}{(\text{q1}^2+i \eta )},\frac{1}{((p-\text{q1})^2+i \eta )},\frac{1}{((p-\text{q1}+\text{q2})^2+i \eta )}\right\},\{\text{q1},\text{q2}\},\{p\},\{\},\{\}\right)\right\}$$