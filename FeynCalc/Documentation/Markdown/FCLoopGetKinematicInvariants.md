## FCLoopGetKinematicInvariants

`FCLoopGetKinematicInvariants[topo]` returns the list of kinematic invariants (masses and scalar products) present in the given topology `topo`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md).

### Examples

```mathematica
topo1 = FCTopology[topo2, {SFAD[{{l + P/2, 0}, {mq^2, 1}, 1}], SFAD[{{l - P/2, 0}, {mq^2, 1}, 1}], SFAD[{{k1 + l - P/2, 0}, {mq^2, 1}, 
      1}]}, {l}, {k1, P}, {Hold[Pair][Momentum[k1, D], Momentum[k1, D]] -> 0, Hold[
       Pair][Momentum[P, D], Momentum[q, D]] -> 0, Hold[Pair][Momentum[P, D], 
      Momentum[P, D]] -> 4*mq^2, Hold[Pair][Momentum[k2, D], Momentum[P, D]] -> 2*mq^
       2, Hold[Pair][Momentum[k1, D], Momentum[k2, D]] -> 2*mq^2}, {}]
```

$$\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{((l+\frac{P}{2})^2-\text{mq}^2+i \eta )},\frac{1}{((l-\frac{P}{2})^2-\text{mq}^2+i \eta )},\frac{1}{((\text{k1}+l-\frac{P}{2})^2-\text{mq}^2+i \eta )}\right\},\{l\},\{\text{k1},P\},\left\{\text{Hold}[\text{Pair}][\text{k1},\text{k1}]\to 0,\text{Hold}[\text{Pair}][P,q]\to 0,\text{Hold}[\text{Pair}][P,P]\to 4 \;\text{mq}^2,\text{Hold}[\text{Pair}][\text{k2},P]\to 2 \;\text{mq}^2,\text{Hold}[\text{Pair}][\text{k1},\text{k2}]\to 2 \;\text{mq}^2\right\},\{\}\right)$$

```mathematica
FCLoopGetKinematicInvariants[topo1]
```

$$\{\text{mq},\text{k1}\cdot P\}$$