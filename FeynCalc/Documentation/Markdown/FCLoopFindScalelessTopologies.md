## FCLoopFindScalelessTopologies

`FCLoopFindScalelessTopologies[topos]` finds scaleless topologies with in the given list of topologies. The function returns a list of two lists, where the first list contains all scaleless topologies and the second one the rest.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindOverdeterminedTopologies](FCLoopFindOverdeterminedTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),
[SubtopologyMarker](SubtopologyMarker.md).

### Examples

```mathematica
topos = {FCTopology[topo1, {SFAD[k1], SFAD[k1 + p], SFAD[k1 - p]}, {k1}, {p}, {}, {}], 
   FCTopology[topo2, {SFAD[k1], SFAD[k1 + p]}, {k1}, {p}, {}, {}], 
   FCTopology[topo3, {SFAD[k1]}, {k1}, {p}, {}, {}]}
```

$$\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )},\frac{1}{((\text{k1}-p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo3},\left\{\frac{1}{(\text{k1}^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right)\right\}$$

```mathematica
FCLoopFindScalelessTopologies[topos]
```

$$\left\{\left\{\text{FCTopology}\left(\text{topo3},\left\{\frac{1}{(\text{k1}^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right)\right\},\left\{\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )},\frac{1}{((\text{k1}-p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right),\text{FCTopology}\left(\text{topo2},\left\{\frac{1}{(\text{k1}^2+i \eta )},\frac{1}{((\text{k1}+p)^2+i \eta )}\right\},\{\text{k1}\},\{p\},\{\},\{\}\right)\right\}\right\}$$