`FCLoopFindIntegralMappings[{int1, int2, ...}, {p1, p2, ...}]` finds mappings between scalar multiloop integrals `int1, int2, ...` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).

The current implementation is based on the `FindEquivalents` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808)

It is also possible to invoke the function as `FCLoopFindIntegralMappings[{GLI[...], ...}, {FCTopology[...], ...}] or FCLoopFindIntegralMappings[{FCTopology[...], ...}]`.

Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md)

### Examples

When given a list of `FeynAmpDenominator`-integrals, the function will merely group identical integrals into sublists

```mathematica
ints = {FAD[{p1, m1}], FAD[{p1 + q, m1}], FAD[{p1, m2}]}
```

$$\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2},\frac{1}{\text{p1}^2-\text{m2}^2}\right\}$$

```mathematica
FCLoopFindIntegralMappings[ints, {p1}]
```

$$\left\{\left\{\frac{1}{\text{p1}^2-\text{m1}^2},\frac{1}{(\text{p1}+q)^2-\text{m1}^2}\right\},\left\{\frac{1}{\text{p1}^2-\text{m2}^2}\right\}\right\}$$

The following 3 integrals look rather different from each other, but are actually identical

```mathematica
ints = {FAD[p1] FAD[p1 - p3 - p4] FAD[p4] FAD[p3 + q1] FAD[{p3, m1}] FAD[{p1 - p4, m1}] FAD[{p1 + q1, 0}, {p1 + q1, 0}], 
   FAD[p4] FAD[p1 - p3 + q1] FAD[p3 + q1] FAD[p1 + p4 + q1] FAD[{p3, m1}] FAD[{p1 + q1, m1}] FAD[{p1 + p4 + 2 q1, 0}, {p1 + p4 + 2 q1, 0}], 
   FAD[p1] FAD[p4 - 2 q1] FAD[p3 + q1] FAD[p1 - p3 - p4 + 2 q1] FAD[{p3, m1}] FAD[{p1 - p4 + 2 q1, m1}] FAD[{p1 + q1, 0}, {p1 + q1, 0}]}
```

$$\left\{\frac{1}{\text{p1}^2 \;\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^4 (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}-\text{p4})^2 \left((\text{p1}-\text{p4})^2-\text{m1}^2\right)},\frac{1}{\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}+\text{q1})^2 (\text{p1}+\text{p4}+\text{q1})^2 (\text{p1}+\text{p4}+2 \;\text{q1})^4 \left((\text{p1}+\text{q1})^2-\text{m1}^2\right)},\frac{1}{\text{p1}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^4 (\text{p3}+\text{q1})^2 (\text{p4}-2 \;\text{q1})^2 (\text{p1}-\text{p3}-\text{p4}+2 \;\text{q1})^2 \left((\text{p1}-\text{p4}+2 \;\text{q1})^2-\text{m1}^2\right)}\right\}$$

```mathematica
FCLoopFindIntegralMappings[ints, {p1, p3, p4}]
```

$$\left(
\begin{array}{ccc}
 \frac{1}{\text{p1}^2 \;\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^4 (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}-\text{p4})^2 \left((\text{p1}-\text{p4})^2-\text{m1}^2\right)} & \frac{1}{\text{p4}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p3}+\text{q1})^2 (\text{p1}-\text{p3}+\text{q1})^2 (\text{p1}+\text{p4}+\text{q1})^2 (\text{p1}+\text{p4}+2 \;\text{q1})^4 \left((\text{p1}+\text{q1})^2-\text{m1}^2\right)} & \frac{1}{\text{p1}^2 \left(\text{p3}^2-\text{m1}^2\right) (\text{p1}+\text{q1})^4 (\text{p3}+\text{q1})^2 (\text{p4}-2 \;\text{q1})^2 (\text{p1}-\text{p3}-\text{p4}+2 \;\text{q1})^2 \left((\text{p1}-\text{p4}+2 \;\text{q1})^2-\text{m1}^2\right)} \\
\end{array}
\right)$$

If the input is a list of `GLI`-integrals, `FCLoopFindIntegralMappings` will return a list containing two sublists. The former will be a list of replacement rules while the latter will contain all unique master integrals

```mathematica
topos = {
   FCTopology[topo1, {SFAD[{p1, m^2}], SFAD[{p2, m^2}]}, {p1, p2}, {}, {}, {}], 
   FCTopology[topo2, {SFAD[{p3, m^2}], SFAD[{p4, m^2}]}, {p3, p4}, {}, {}, {}] 
  }
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2-m^2+i \eta )},\frac{1}{(\text{p2}^2-m^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{p3}^2-m^2+i \eta )},\frac{1}{(\text{p4}^2-m^2+i \eta )}\right\},\{\text{p3},\text{p4}\},\{\},\{\},\{\}\right)\right\}$$

```mathematica
glis = {GLI[topo1, {1, 1}], GLI[topo1, {1, 2}], GLI[topo1, {2, 1}], GLI[topo2, {1, 1}], GLI[topo2, {2, 2}]}
```

$$\left\{G^{\text{topo1}}(1,1),G^{\text{topo1}}(1,2),G^{\text{topo1}}(2,1),G^{\text{topo2}}(1,1),G^{\text{topo2}}(2,2)\right\}$$

```mathematica
FCLoopFindIntegralMappings[glis, topos]
```

$$\left\{\left\{G^{\text{topo2}}(1,1)\to G^{\text{topo1}}(1,1),G^{\text{topo1}}(2,1)\to G^{\text{topo1}}(1,2)\right\},\left\{G^{\text{topo1}}(1,1),G^{\text{topo1}}(1,2),G^{\text{topo2}}(2,2)\right\}\right\}$$

This behavior can be turned off by setting the value of the option `List` to `True`

```mathematica
FCLoopFindIntegralMappings[glis, topos, List -> True]
```

$$\left\{\left\{G^{\text{topo1}}(1,1),G^{\text{topo2}}(1,1)\right\},\left\{G^{\text{topo1}}(1,2),G^{\text{topo1}}(2,1)\right\},\left\{G^{\text{topo2}}(2,2)\right\}\right\}$$

In practice, one usually has a list of preferred integrals onto which one would like to map the occurring master integrals. Such integrals can be specified via the `PreferredIntegrals` options

```mathematica
FCLoopFindIntegralMappings[glis, topos, PreferredIntegrals -> {GLI[topo2, {1, 1}], GLI[topo2, {2, 1}]}]
```

$$\left(
\begin{array}{ccc}
 G^{\text{topo1}}(1,1)\to G^{\text{topo2}}(1,1) & G^{\text{topo1}}(1,2)\to G^{\text{topo2}}(2,1) & G^{\text{topo1}}(2,1)\to G^{\text{topo2}}(2,1) \\
 G^{\text{topo2}}(1,1) & G^{\text{topo2}}(2,1) & G^{\text{topo2}}(2,2) \\
\end{array}
\right)$$