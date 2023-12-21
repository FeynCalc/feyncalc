## CSIS

CSIS[p]can be used as input for 3-dimensional $\sigma ^i p^i$ with 3-dimensional Cartesian vector p and is transformed into PauliSigma[CartesianMomentum[p]] by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSIS[p]
```

$$\overline{\sigma }\cdot \overline{p}$$

```mathematica
CSIS[p] // FCI // StandardForm

(*PauliSigma[CartesianMomentum[p]]*)
```

```mathematica
CSIS[p, q, r, s]
```

$$\left(\overline{\sigma }\cdot \overline{p}\right).\left(\overline{\sigma }\cdot \overline{q}\right).\left(\overline{\sigma }\cdot \overline{r}\right).\left(\overline{\sigma }\cdot \overline{s}\right)$$

```mathematica
CSIS[p, q, r, s] // StandardForm

(*CSIS[p] . CSIS[q] . CSIS[r] . CSIS[s]*)
```