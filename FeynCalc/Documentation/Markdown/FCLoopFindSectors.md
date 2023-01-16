```mathematica
 
```

## FCLoopFindSectors

`FCLoopFindSectors[{GLI[...], ...}]` analyzes the indices of the GLI integrals in the given list and identifies sectors to which they belong. Notice that only `GLI`s with integer indices are supported.

If the option `GatherBy` is set to `True` (default), the output will be a list of two lists, where the former contains the original integrals sorted w.r.t the identified sectors, while the latter is a list of all available sectors.

For `GatherBy->False`, the output is a list containing all identified sectors without the original integrals.

Setting the option `Last` to `True`will return only the top sector.

### See also

[Overview](Extra/FeynCalc.md).

### Examples

```mathematica
ints = {
    GLI[topo1, {1, 1, 1, 1}], 
    GLI[topo1, {2, 1, 2, 1}], 
    GLI[topo2, {1, 0, 1, 1}], 
    GLI[topo3, {1, 0, 1, -1}] 
   };
```

```mathematica
FCLoopFindSectors[ints]
```

$$\left(
\begin{array}{ccc}
 \left\{\{1,0,1,0\},\left\{G^{\text{topo3}}(1,0,1,-1)\right\}\right\} & \left\{\{1,0,1,1\},\left\{G^{\text{topo2}}(1,0,1,1)\right\}\right\} & \left\{\{1,1,1,1\},\left\{G^{\text{topo1}}(1,1,1,1),G^{\text{topo1}}(2,1,2,1)\right\}\right\} \\
 \{1,0,1,0\} & \{1,0,1,1\} & \{1,1,1,1\} \\
\end{array}
\right)$$

```mathematica
FCLoopFindSectors[ints, Last -> True]
```

$$\left\{\{1,1,1,1\},\left\{G^{\text{topo1}}(1,1,1,1),G^{\text{topo1}}(2,1,2,1)\right\}\right\}$$

```mathematica
FCLoopFindSectors[ints, GatherBy -> False]
```

$$\left(
\begin{array}{cccc}
 1 & 0 & 1 & 0 \\
 1 & 0 & 1 & 1 \\
 1 & 1 & 1 & 1 \\
\end{array}
\right)$$

```mathematica
FCLoopFindSectors[ints, GatherBy -> False, Last -> True]
```

$$\{1,1,1,1\}$$