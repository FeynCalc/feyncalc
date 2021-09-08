## FCPermuteMomentaRules

`FCPermuteMomentaRules[{p1, p2, ...}]` returns a set of rules that contain all possible permutations of the momenta `p1`, `p2`, ... . This can be useful when working with amplitudes that exhibit a symmetry in some or all of the final state momenta or when trying to find mappings between loop integrals from different topologies.

### See also

[Overview](Extra/FeynCalc.md), [FCReplaceMomenta](FCReplaceMomenta.md).

### Examples

```mathematica
FCPermuteMomentaRules[{p1, p2}]
f[p1, p2] /. %
```

$$\{\{\},\{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1}\}\}$$

$$\{f(\text{p1},\text{p2}),f(\text{p2},\text{p1})\}$$

```mathematica
FCPermuteMomentaRules[{p1, p2, p3}]
f[p1, p2, p3] /. % 
  
 

```

$$\{\{\},\{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p1}\},\{\text{p1}\to \;\text{p3},\text{p3}\to \;\text{p1}\},\{\text{p2}\to \;\text{p3},\text{p3}\to \;\text{p2}\},\{\text{p1}\to \;\text{p2},\text{p2}\to \;\text{p3},\text{p3}\to \;\text{p1}\},\{\text{p1}\to \;\text{p3},\text{p2}\to \;\text{p1},\text{p3}\to \;\text{p2}\}\}$$

$$\{f(\text{p1},\text{p2},\text{p3}),f(\text{p2},\text{p1},\text{p3}),f(\text{p3},\text{p2},\text{p1}),f(\text{p1},\text{p3},\text{p2}),f(\text{p2},\text{p3},\text{p1}),f(\text{p3},\text{p1},\text{p2})\}$$
