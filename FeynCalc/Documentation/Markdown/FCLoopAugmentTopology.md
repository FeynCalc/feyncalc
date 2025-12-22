## FCLoopAugmentTopology

`FCLoopAugmentTopology[topo, {extraProps}]` augments the topology `topo` by adding new propagators `extraProps` to the basis. This is usually needed when a tensor reduction requires us to introduce an auxiliary vector that will appear in scalar products involving loop momenta.

The input topologies do not have to be complete.

The output of this routine contains augmented topologies and a list of replacement rules for converting `GLI`s depending on the old topologies into new ones.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopTensorReduce](FCLoopTensorReduce.md).

### Examples

```mathematica
topo = FCTopology["topo1", {SFAD[{q1, m^2}], SFAD[{q1 + p1}], 
   	SFAD[{q1 + p2}]}, {q1}, {p1, p2}, {Hold[SPD][p1] -> 0, Hold[SPD][p2] -> 0, 
   	Hold[SPD][p1, p2] -> 0}, {}]
```

$$\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{q1}^2-m^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p2}+\text{q1})^2+i \eta )}\right\},\{\text{q1}\},\{\text{p1},\text{p2}\},\{\text{Hold}[\text{SPD}][\text{p1}]\to 0,\text{Hold}[\text{SPD}][\text{p2}]\to 0,\text{Hold}[\text{SPD}][\text{p1},\text{p2}]\to 0\},\{\}\right)$$

The option `AugmentedTopologyMarker` denotes a symbol that is usually introduced by `FCLoopTensorReduce` when the reduction requires an auxiliary vector. Therefore, it will appear on the right hand side of the `GLI`-replacement rules. This can be disabled by setting this option to `False`

```mathematica
FCLoopAugmentTopology[topo, {SFAD[{{0, q1 . n}}]}, 
  FinalSubstitutions -> {Hold[SPD][n] -> 0, Hold[SPD][n, p1] -> np1, 
    Hold[SPD][n, p2] -> np2}]
```

$$\left\{\text{FCTopology}\left(\text{topo1A},\left\{\frac{1}{(\text{q1}^2-m^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p2}+\text{q1})^2+i \eta )},\frac{1}{(n\cdot \;\text{q1}+i \eta )}\right\},\{\text{q1}\},\{\text{p1},\text{p2},n\},\{\text{Hold}[\text{SPD}][\text{p1}]\to 0,\text{Hold}[\text{SPD}][\text{p2}]\to 0,\text{Hold}[\text{SPD}][\text{p1},\text{p2}]\to 0,\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][n,\text{p1}]\to \;\text{np1},\text{Hold}[\text{SPD}][n,\text{p2}]\to \;\text{np2}\},\{\}\right),\text{FCGV}(\text{AddPropagators})(\{n\}) G^{\text{topo1}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$}):\to G^{\text{topo1A}}(\text{n1},\text{n2},\text{n3},0)\right\}$$

```mathematica
FCLoopAugmentTopology[topo, {SFAD[{{0, q1 . n}}]}, 
  FinalSubstitutions -> {Hold[SPD][n] -> 0, Hold[SPD][n, p1] -> np1, 
    Hold[SPD][n, p2] -> np2}, AugmentedTopologyMarker -> False]
```

$$\left\{\text{FCTopology}\left(\text{topo1A},\left\{\frac{1}{(\text{q1}^2-m^2+i \eta )},\frac{1}{((\text{p1}+\text{q1})^2+i \eta )},\frac{1}{((\text{p2}+\text{q1})^2+i \eta )},\frac{1}{(n\cdot \;\text{q1}+i \eta )}\right\},\{\text{q1}\},\{\text{p1},\text{p2},n\},\{\text{Hold}[\text{SPD}][\text{p1}]\to 0,\text{Hold}[\text{SPD}][\text{p2}]\to 0,\text{Hold}[\text{SPD}][\text{p1},\text{p2}]\to 0,\text{Hold}[\text{SPD}][n]\to 0,\text{Hold}[\text{SPD}][n,\text{p1}]\to \;\text{np1},\text{Hold}[\text{SPD}][n,\text{p2}]\to \;\text{np2}\},\{\}\right),G^{\text{topo1}}(\text{n1$\_$},\text{n2$\_$},\text{n3$\_$}):\to G^{\text{topo1A}}(\text{n1},\text{n2},\text{n3},0)\right\}$$