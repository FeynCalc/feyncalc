## FCLoopPakOrder

`FCLoopPakOrder[poly, {x1, x2, ...}]` determines a canonical ordering of the Feynman parameters `x1, x2, ...` in the polynomial poly using the algorithm of Alexey Pak (arXiv:1111.0868). 

Cf. also the PhD thesis of Jens Hoff (Hoff:2015kub, 10.5445/IR/1000047447) for a more detailed description of the algorithm.

The current implementation is based on the `PolyOrdering` function from FIRE 6 (arXiv:1901.07808)

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).

### Examples

```mathematica
FCLoopPakOrder[(x[1]*x[2] + x[1]*x[3] + x[2]*x[3] + x[2]*x[4] + 
     x[3]*x[4] + x[1]*x[5] + x[2]*x[5] + x[4]*x[5])*(m1^2*x[1]^2*x[2] + 
     m3^2*x[1]*x[2]^2 + m1^2*x[1]^2*x[3] + m1^2*x[1]*x[2]*x[3] + 
     m2^2*x[1]*x[2]*x[3] + m3^2*x[1]*x[2]*x[3] + 
     m3^2*x[2]^2*x[3] + m2^2*x[1]*x[3]^2 + m2^2*x[2]*x[3]^2 + 
     m1^2*x[1]*x[2]*x[4] - Pair[Momentum[q, D], Momentum[q, 
        D]]*x[1]*x[2]*x[4] + m3^2*x[2]^2*x[4] + m1^2*x[1]*x[3]*x[4] - 
     Pair[Momentum[q, D], Momentum[q, D]]*x[1]*x[3]*x[4] + 
     m2^2*x[2]*x[3]*x[4] + m3^2*x[2]*x[3]*x[4] - Pair[Momentum[q, D], 
       Momentum[q, D]]*x[2]*x[3]*x[4] + m2^2*x[3]^2*x[4] + m1^2*x[1]^2*x[5] 
     + m1^2*x[1]*x[2]*x[5] + m3^2*x[1]*x[2]*x[5] - Pair[Momentum[q, D], 
       Momentum[q, D]]*x[1]*x[2]*x[5] + 
     m3^2*x[2]^2*x[5] + m2^2*x[1]*x[3]*x[5] - Pair[Momentum[q, D], 
       Momentum[q, D]]*x[1]*x[3]*x[5] + m2^2*x[2]*x[3]*x[5] - 
     Pair[Momentum[q, D], Momentum[q, D]]*x[2]*x[3]*x[5] + 
     m1^2*x[1]*x[4]*x[5] - 
     Pair[Momentum[q, D], Momentum[q, D]]*x[1]*x[4]*x[5] + 
     m3^2*x[2]*x[4]*x[5] + m2^2*x[3]*x[4]*x[5] - Pair[Momentum[q, D], 
       Momentum[q, D]]*x[3]*x[4]*x[5]), {x[1], x[2], x[3], x[4], x[5]}]
```

$$\left(
\begin{array}{ccccc}
 1 & 3 & 2 & 5 & 4 \\
\end{array}
\right)$$