## FCLoopSplit

`FCLoopSplit[exp, {q1, q2, ...}]` separates `exp` into the following four pieces: 

1) terms that are free of loop integrals

2) terms with scalar loop integrals

3) terms with tensor loop integrals, where all loop momenta are contracted

4) terms with tensor loop integrals, where at least some loop momenta have free indices

The result is returned as a list with the 4 above elements.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FVD[q, \[Mu]] FAD[{q, m}]
FCLoopSplit[%, {q}]
```

$$\frac{q^{\mu }}{q^2-m^2}$$

$$\left\{0,0,0,\frac{q^{\mu }}{q^2-m^2}\right\}$$

```mathematica
x + GSD[p + q] FAD[{q, m}]
FCLoopSplit[%, {q}]
```

$$\frac{\gamma \cdot (p+q)}{q^2-m^2}+x$$

$$\left\{x,\frac{\gamma \cdot p}{q^2-m^2},0,\frac{\gamma \cdot q}{q^2-m^2}\right\}$$
