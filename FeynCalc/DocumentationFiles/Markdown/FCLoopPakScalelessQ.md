## FCLoopPakScalelessQ

`FCLoopPakScalelessQ[poly, x]` checks whether the characteristic (Pak) polynomial poly (in the UxF form) with the Feynman parameters `x[1],x[2], ...` corresponds to a scaleless loop integral or loop integral topology. The polynomial does not need to be canonically ordered.

The algorithm is based on arXiv:1011.4863 and the detailed description thereof in the PhD thesis of Jens Hoff (10.5445/IR/1000047447).

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakScalelessQ](FCLoopPakScalelessQ).

### Examples

```mathematica
FCLoopPakScalelessQ[-(SPD[p, p]*x[1]*x[2]*(x[1] + x[2])), x]
```

$$\text{False}$$

```mathematica
FCLoopPakScalelessQ[-(SPD[Q, Q]*(x[1]*x[2] + x[2]*x[3] + 
       x[1]*x[4] + x[3]*x[4] + x[1]*x[5] + x[2]*x[5] + x[3]*x[5] + x[4]*x[5])*
     (x[1]*x[2]*x[3] + x[1]*x[2]*x[4] + x[1]*x[3]*x[4] + x[2]*x[3]*x[4] 
       + x[1]*x[2]*x[5] + x[1]*x[3]*x[5] + x[2]*x[4]*x[5] + x[3]*x[4]*x[5])), x]
```

$$\text{False}$$