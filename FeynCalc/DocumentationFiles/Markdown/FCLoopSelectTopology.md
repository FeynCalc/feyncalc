`FCLoopSelectTopology[int, topos]` selects the topology that matches the `GLI` `int` from a list of topologies `topos`.

The first argument can be also a list, in which case the function will return a list of matching topologies.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

```mathematica
FCLoopSelectTopology[{GLI[topo2, {2, 2}]}, {    FCTopology[topo2, {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {}, {}],     FCTopology[topo1, {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {SPD[q] -> M^2,        SPD[q2] -> M2^2}, {}]}]
```

$$\left\{\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{\text{p1}^2-m^2},\frac{1}{(\text{p1}-q)^2-m^2}\right\},\{\text{p1}\},\{q\},\{\},\{\}\right)\right\}$$