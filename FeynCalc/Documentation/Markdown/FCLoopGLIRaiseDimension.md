## FCLoopGLIRaiseDimension

`FCLoopGLIRaiseDimension[gli, topo]` raises the dimension of the given `GLI` from N to N+2 and expresses it in terms of `N`-dimensional loop integrals returned in the output.

The algorithm is based on the code of  the function `RaisingDRR` from R. Lee's LiteRed

### See also

[Overview](Extra/FeynCalc.md), [FCLoopGLILowerDimension](FCLoopGLILowerDimension.md).

### Examples

```mathematica
topo = FCTopology[
   topo1, {SFAD[p1], SFAD[p2], SFAD[Q - p1 - p2], SFAD[Q - p2], 
    SFAD[Q - p1]}, {p1, p2}, {Q}, {Hold[SPD[Q]] -> qq}, {}]
```

$$\text{FCTopology}\left(\text{topo1},\left\{\frac{1}{(\text{p1}^2+i \eta )},\frac{1}{(\text{p2}^2+i \eta )},\frac{1}{((-\text{p1}-\text{p2}+Q)^2+i \eta )},\frac{1}{((Q-\text{p2})^2+i \eta )},\frac{1}{((Q-\text{p1})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\text{Hold}[\text{SPD}(Q)]\to \;\text{qq}\},\{\}\right)$$

```mathematica
FCLoopGLIRaiseDimension[GLI[topo1, {1, 1, 1, 1, 1}], topo]
```

$$-\frac{G^{\text{topo1}}(-1,0,1,1,1)}{(1-D) (2-D) Q^2}-\frac{Q^2 G^{\text{topo1}}(1,1,0,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(0,-1,1,1,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(0,0,0,1,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(0,0,1,0,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(0,0,1,1,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(0,1,0,0,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(0,1,1,0,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(1,0,0,1,0)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(1,0,1,0,0)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(1,1,0,0,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(1,1,1,-1,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(1,1,1,0,-1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(0,0,1,1,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(0,1,0,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(0,1,1,1,0)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(1,0,0,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(1,0,1,0,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(1,1,-1,1,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(1,1,0,0,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(1,1,0,1,0)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(1,1,1,0,0)}{(1-D) (2-D)}$$

```mathematica
FCLoopGLIRaiseDimension[GLI[topo1, {n1, n2, n3, 1, 1}], topo]
```

$$-\frac{G^{\text{topo1}}(\text{n1}-2,\text{n2}-1,\text{n3},1,1)}{(1-D) (2-D) Q^2}-\frac{Q^2 G^{\text{topo1}}(\text{n1},\text{n2},\text{n3}-1,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2}-2,\text{n3},1,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2}-1,\text{n3}-1,1,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2}-1,\text{n3},0,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2}-1,\text{n3},1,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2},\text{n3}-1,0,1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2},\text{n3},0,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(\text{n1},\text{n2}-1,\text{n3}-1,1,0)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1},\text{n2}-1,\text{n3},0,0)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3}-1,0,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3},-1,0)}{(1-D) (2-D) Q^2}-\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3},0,-1)}{(1-D) (2-D) Q^2}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2}-1,\text{n3},1,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2},\text{n3}-1,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(\text{n1}-1,\text{n2},\text{n3},1,0)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(\text{n1},\text{n2}-1,\text{n3}-1,1,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(\text{n1},\text{n2}-1,\text{n3},0,1)}{(1-D) (2-D)}-\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3}-2,1,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3}-1,0,1)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3}-1,1,0)}{(1-D) (2-D)}+\frac{G^{\text{topo1}}(\text{n1},\text{n2},\text{n3},0,0)}{(1-D) (2-D)}$$