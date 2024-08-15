## FCTopology

`FCTopology[id, {prop1, prop2, ...}, {l1, l2, ...}, {p1, p2, ...}, {kRule1, kRule2, ...}, {}]` denotes a topology with the identifier `id` that is characterized by the propagators `{prop1, prop2, ...}`. The propagators in the list do not necessarily have to form a valid basis, i.e. the basis may also be incomplete or overdetermined. The lists `{l1, l2, ...}` and `{p1, p2, ...}` stand for the loop and external momenta respectively. Furthermore, {kRule1, kRule2, ...} denotes replacement rules for kinematic invariants.

The last argument (an empty list) is reserved for future improvements.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFromGLI](FCLoopFromGLI.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md), [GLI](GLI.md).

### Examples

A 2-loop topology with one external momentum `Q`

```mathematica
FCTopology[topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2], SFAD[Q - p1]}, {p1, p2}, {Q}, {
   Hold[SPD[Q]] -> qq}, {}]
```

$$\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\text{Hold}[\text{SPD}(Q)]\to \;\text{qq}\},\{\}\right)$$

A 3-loop topology with one external momentum `Q`

```mathematica
topo = FCTopology[topo2, {SFAD[p1], SFAD[p2], SFAD[p3], SFAD[Q - p1 - p2 - p3], SFAD[Q - p1 - p2], 
    SFAD[Q - p1], SFAD[Q - p2], SFAD[p1 + p3], SFAD[p2 + p3]}, {p1, p2, p3}, {Q}, {}, {}]
```

$$\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}-\text{p3}+Q)^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )}\right\},\{\text{p1},\text{p2},\text{p3}\},\{Q\},\{\},\{\}\right)$$

Use `FCLoopValidTopologyQ` to check if the syntax of the given topology is correct.

```mathematica
FCLoopValidTopologyQ[topo]
```

$$\text{True}$$

The list of propagators in the topology essentially defines the propagator representation of a `GLI`. Notice that propagators are allowed to have symbolical or numerical prefactors, as long as those do not depend on loop or external momenta

```mathematica
topo2 = FCTopology[topoTest, {a SFAD[p1], b SFAD[p2], c SFAD[Q - p1 - p2], d SFAD[Q - p2], 
    e SFAD[Q - p1]}, {p1, p2}, {Q}, {}, {}]
```

$$\text{FCTopology}\left(\text{topoTest},\left\{\frac{a}{(\text{p1}^2+i \eta )},\frac{b}{(\text{p2}^2+i \eta )},\frac{c}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{d}{((Q-\text{p2})^2+i \eta )},\frac{e}{((Q-\text{p1})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right)$$

```mathematica
FCLoopFromGLI[GLI[topoTest, {1, 1, 1, 1, 1}], topo2]
```

$$\frac{a b c d e}{(\text{p1}^2+i \eta ) (\text{p2}^2+i \eta ) ((Q-\text{p1})^2+i \eta ) ((Q-\text{p2})^2+i \eta ) ((-\text{p1}-\text{p2}+Q)^2+i \eta )}$$