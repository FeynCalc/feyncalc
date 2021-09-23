## SIS

`SIS[p]` can be used as input for $3$-dimensional $\sigma^{\mu } p_{\mu }$ with 4-dimensional Lorentz vector $p$ and is transformed into `PauliSigma[Momentum[p]]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SISD](SISD.md).

### Examples

```mathematica
SIS[p]
```

$$\bar{\sigma }\cdot \overline{p}$$

```mathematica
SIS[p] // FCI // StandardForm

(*PauliSigma[Momentum[p]]*)
```

```mathematica
SIS[p, q, r, s]
```

$$\left(\bar{\sigma }\cdot \overline{p}\right).\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{r}\right).\left(\bar{\sigma }\cdot \overline{s}\right)$$

```mathematica
SIS[p, q, r, s] // StandardForm

(*SIS[p] . SIS[q] . SIS[r] . SIS[s]*)
```

```mathematica
SIS[q] . (SIS[p] + m) . SIS[q]
```

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{p}+m\right).\left(\bar{\sigma }\cdot \overline{q}\right)$$
