```mathematica
 
```

## GSLP

`GSLP[p,n,nb]` denotes the positive component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (\bar{n} \cdot p) (\gamma \cdot n)$.

If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLN](GSLN.md), [GSLR](GSLR.md).

### Examples

```mathematica
GSLP[p, n, nb]
```

$$\frac{1}{2} \bar{\gamma }\cdot \overline{n} \left(\overline{\text{nb}}\cdot \overline{p}\right)$$

```mathematica
StandardForm[GSLP[p, n, nb] // FCI]
```

$$\frac{1}{2} \;\text{DiracGamma}[\text{Momentum}[n]] \;\text{Pair}[\text{Momentum}[\text{nb}],\text{Momentum}[p]]$$

Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation

```mathematica
GSLP[p, n, nb] . GSLP[q, n, nb] // DiracSimplify
```

$$\frac{1}{4} \overline{n}^2 \left(\overline{\text{nb}}\cdot \overline{p}\right) \left(\overline{\text{nb}}\cdot \overline{q}\right)$$

```mathematica
FCClearScalarProducts[]
SP[n] = 0;
SP[nb] = 0;
SP[n, nb] = 2;
```

```mathematica
GSLP[p, n, nb] . GSLP[q, n, nb] // DiracSimplify
```

$$0$$

```mathematica
FCClearScalarProducts[]
```