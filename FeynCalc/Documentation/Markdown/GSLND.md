```mathematica
 
```

## GSLND

`GSLND[p,n,nb]` denotes the negative component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\frac{1}{2} (n \cdot p) (\gamma \cdot \bar{n})$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLRD](GSLRD.md).

### Examples

```mathematica
GSLND[p, n, nb]
```

$$\frac{1}{2} (n\cdot p) \gamma \cdot \;\text{nb}$$

```mathematica
StandardForm[GSLND[p, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[\text{nb},D],D] \;\text{Pair}[\text{Momentum}[n,D],\text{Momentum}[p,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLND[p, n, nb] . GSLND[q, n, nb] // DiracSimplify
```

$$\frac{1}{4} \;\text{nb}^2 (n\cdot p) (n\cdot q)$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GSLND[p, n, nb] . GSLND[q, n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```