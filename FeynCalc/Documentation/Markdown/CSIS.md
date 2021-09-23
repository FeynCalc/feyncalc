## CSIS

CSIS[p]can be used as input for 3-dimensional $\sigma ^ip^i$ with 3-dimensional Cartesian vector p and is transformed into PauliSigma[CartesianMomentum[p]] by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).

### Examples

```mathematica
CSIS[p] 
 
CSIS[p] // FCI // StandardForm 
 
CSIS[p, q, r, s] 
 
CSIS[p, q, r, s] // StandardForm
```

$$\overline{\sigma }\cdot \overline{p}$$

```
(*PauliSigma[CartesianMomentum[p]]*)
```

$$\left(\overline{\sigma }\cdot \overline{p}\right).\left(\overline{\sigma }\cdot \overline{q}\right).\left(\overline{\sigma }\cdot \overline{r}\right).\left(\overline{\sigma }\cdot \overline{s}\right)$$

```
(*CSIS[p] . CSIS[q] . CSIS[r] . CSIS[s]*)
```
