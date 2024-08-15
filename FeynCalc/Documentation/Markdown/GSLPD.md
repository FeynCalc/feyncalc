## GSLPD

`GSLPD[p,n,nb]` denotes the positive component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\frac{1}{2} (\bar{n} \cdot p) (\gamma \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLND](GSLND.md), [GSLRD](GSLRD.md).

### Examples

```mathematica
GSLPD[p, n, nb]
```

$$\frac{1}{2} \gamma \cdot n (\text{nb}\cdot p)$$

```mathematica
StandardForm[GSLPD[p, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[n,D],D] \;\text{Pair}[\text{Momentum}[\text{nb},D],\text{Momentum}[p,D]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLPD[p, n, nb] . GSLPD[q, n, nb] // DiracSimplify
```

$$\frac{1}{4} n^2 (\text{nb}\cdot p) (\text{nb}\cdot q)$$

```mathematica
FCClearScalarProducts[]
SPD[n] = 0;
SPD[nb] = 0;
SPD[n, nb] = 2;
```

```mathematica
GSLPD[p, n, nb] . GSLPD[q, n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```