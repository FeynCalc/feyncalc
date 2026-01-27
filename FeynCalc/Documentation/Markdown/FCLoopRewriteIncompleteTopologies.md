## FCLoopRewriteIncompleteTopologies

`FCLoopRewriteIncompleteTopologies[expr , topos]` handles topologies with incomplete propagator bases in the given expression. The routine will automatically perform basis completions by adding missing propagators, introduce new names for the resulting topologies and return back the expression depending on those new topologies together with a list of the corresponding topologies.

The input expression is expected to be of the form returned by `FCLoopFindTopologies`, e.g. with numerators separated from the denominators where the latter are written as `GLI`s.

The names of the automatically generated topology can be controlled using the `Names` option.

By default the basis completion approach (controlled by the `Method` option)  is set to `Automatic`. This means that the function will use propagators already present in the list of supplied topologies to find complete the bases. It is also possible to specify the propagators explicitly as a list or use the option `ScalarProduct` for automatically adding eikonal propagators.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindIncompleteTopologies](FCLoopFindIncompleteTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),
[SubtopologyMarker](SubtopologyMarker.md).

### Examples

```mathematica
topos = {FCTopology[topo1, {SFAD[k1], SFAD[k2]}, {k1, k2}, {}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k2}^2+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\},\{\},\{\}\right)\right\}$$

```mathematica
expr = FCGV["GLIProduct"][SPD[k1, k2], GLI[topo1, {1, 1}]]
```

$$\text{FCGV}(\text{GLIProduct})\left(\text{k1}\cdot \;\text{k2},G^{\text{topo1}}(1,1)\right)$$

```mathematica
FCLoopRewriteIncompleteTopologies[expr, topos, Method -> {SFAD[k1 - k2]}]
```

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{Found }1\text{ incomplete topologies.}$$

$$\left\{\text{FCGV}(\text{GLIProduct})\left(\text{k1}\cdot \;\text{k2},G^{\text{topo1Aug}}(1,1,0)\right),\left\{\text{FCTopology}\left(\text{topo1Aug},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{((\text{k1}-\text{k2})^2+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\},\{\},\{1\}\right)\right\}\right\}$$

```mathematica
FCLoopRewriteIncompleteTopologies[expr, topos, Method -> ScalarProduct]

```mathematica

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{Found }1\text{ incomplete topologies.}$$

$$\left\{\text{FCGV}(\text{GLIProduct})\left(\text{k1}\cdot \;\text{k2},G^{\text{topo1Aug}}(1,1,0)\right),\left\{\text{FCTopology}\left(\text{topo1Aug},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{(\text{k2}^2+i \eta )},\frac{1}{(\text{k1}\cdot \;\text{k2}+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\},\{\},\{1\}\right)\right\}\right\}$$