## FCLoopRewriteOverdeterminedTopologies

`FCLoopRewriteOverdeterminedTopologies[expr , topos]` handles topologies with overdetermined propagator bases in the given expression. The routine will automatically perform partial fraction decomposition on the affected topologies, introduce new names for the resulting topologies and return back the expression depending on those new topologies together with a list of the corresponding topologies.

The input expression is expected to be of the form returned by `FCLoopFindTopologies`, e.g. with numerators separated from the denominators where the latter are written as `GLI`s.

The names of the automatically generated topology can be controlled using the `Names` option.

Notice that the returned topologies can be related to each other, while some of them may even have identical sets of propagators. This is expected, because the output of this function usually gets passed to `FCLoopFindTopologyMappings`.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindOverdeterminedTopologies](FCLoopFindOverdeterminedTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),
[SubtopologyMarker](SubtopologyMarker.md).

### Examples

```mathematica
topos = {FCTopology[topo1, {SFAD[k1], SFAD[k1 + p], SFAD[k1 - p]}, {k1}, {p}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )},\frac{1}{((\text{k1}-p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right)\right\}$$

```mathematica
expr = FCGV["GLIProduct"][SPD[k1, p], GLI[topo1, {1, 1, 1}]]
```

$$\text{FCGV}(\text{GLIProduct})\left(\text{k1}\cdot p,G^{\text{topo1}}(1,1,1)\right)$$

```mathematica
FCLoopRewriteOverdeterminedTopologies[expr, topos]
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ overdetermined topologies.}$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Generated }3\text{ new topologies through partial fractioning.}$$

$$\left\{\text{FCGV}(\text{GLIProduct})\left(-\frac{\text{k1}\cdot p}{p^2},G^{\text{fcPFRTopology1}}(1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\frac{\text{k1}\cdot p}{2 p^2},G^{\text{fcPFRTopology2}}(1,1)\right)+\text{FCGV}(\text{GLIProduct})\left(\frac{\text{k1}\cdot p}{2 p^2},G^{\text{fcPFRTopology3}}(1,1)\right),\left\{\text{FCTopology}\left(\text{fcPFRTopology1},\left\{\frac{1}{((\text{k1}+p)^2+i \eta )},\frac{1}{((\text{k1}-p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fcPFRTopology2},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}-p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{fcPFRTopology3},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right)\right\}\right\}$$