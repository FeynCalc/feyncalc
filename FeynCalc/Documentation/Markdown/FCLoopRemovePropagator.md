## FCLoopRemovePropagator

`FCLoopRemovePropagator[input,{pos1,pos2,...}]` returns a new `FCTopology` or `GLI` obtained from input by removing propagators at positions listed in `{pos1,pos2,...}`.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopCreatePartialFractioningRules](FCLoopCreatePartialFractioningRules.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

A 2-loop topology with one external momentum `Q`

```mathematica
topo = FCTopology[topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2], SFAD[Q - p1]}, {p1, p2}, {Q}, {
    Hold[SPD[Q]] -> qq}, {}]
```

$$\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\text{Hold}[\text{SPD}(Q)]\to \;\text{qq}\},\{\}\right)$$

The same topology with the 1st and 3rd propagators removed. Notice that the new name is created using the suffix specified via the option `Names`

```mathematica
FCLoopRemovePropagator[topo, {1, 3}]
```

$$\text{FCTopology}\left(\text{topo1PFR13},\left\{\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\text{Hold}[\text{SPD}(Q)]\to \;\text{qq}\},\{\}\right)$$

```mathematica
gli = GLI[topo2, {1, 1, 1, 2, 0, 1, 1}]
```

$$G^{\text{topo2}}(1,1,1,2,0,1,1)$$

```mathematica
FCLoopRemovePropagator[gli, {2, 4}]
```

$$G^{\text{topo2PFR24}}(1,1,0,1,1)$$