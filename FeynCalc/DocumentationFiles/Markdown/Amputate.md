## Amputate

`Amputate[exp, q1, q2, ...]` amputates `Eps` and `DiracGamma`. `Amputate[exp,q1,q2,Pair->{p}]` amputates also `p.q1` and `p.q2`; `Pair->All` amputates all except `OPEDelta`.

### See also

[DiracGamma](DiracGamma), [GA](GA), [DiracSimplify](DiracSimplify), [GS](GS), [DiracTrick](DiracTrick).

### Examples

```mathematica
GS[p] . GS[q]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)$$

```mathematica
Amputate[%, q]
```

$$q^{\text{$\$$AL$\$$15664}(1)} \left(\bar{\gamma }\cdot \overline{p}\right).\gamma ^{\text{$\$$AL$\$$15664}(1)}$$