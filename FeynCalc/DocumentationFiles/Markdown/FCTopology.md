## FCTopology

`FCTopology[id, {prop1, prop2, ...}]` denotes a topology with the identifier id that is characterized by the propagators `{prop1, prop2, ...}`. The propagators in the list do not necessarily have to form a valid basis, i.e. the basis may also be incomplete or overdetermined.

### See also

[Overview](Extra/FeynCalc.md).

### Examples

A 2-loop topology with one external momentum `Q`

```mathematica
FCTopology[topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2], SFAD[Q - p1]}]
```

$$\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\}\right)$$

A 3-loop topology with one external momentum `Q`

```mathematica
FCTopology[topo2, {SFAD[p1], SFAD[p2], SFAD[p3], SFAD[Q - p1 - p2 - p3], SFAD[Q - p1 - p2], SFAD[Q - p1], SFAD[Q - p2], SFAD[p1 + p3], SFAD[p2 + p3]}]
```

$$\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{(\text{p3}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}-\text{p3}+Q)^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((\text{p1}+\text{p3})^2+i \eta )},\frac{1}{((\text{p2}+\text{p3})^2+i \eta )}\right\}\right)$$
